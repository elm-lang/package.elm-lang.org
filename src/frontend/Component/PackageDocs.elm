module Component.PackageDocs where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Regex
import Set
import String
import Task

import Docs.Entry as Entry
import Docs.Name as Name
import Docs.Package as Docs
import Docs.Type as Type
import Native.Jump
import Page.Context as Ctx
import Parse.Type as Type
import Utils.Markdown as Markdown



-- MODEL


type Model
    = Loading
    | Failed Http.Error
    | Readme String
    | RawDocs (Info String)
    | ParsedDocs (Info Type.Type)


type alias Info tipe =
  { name : String
  , nameDict : Name.Dictionary
  , chunks : List (Chunk tipe)
  }


type Chunk tipe
    = Markdown String
    | Entry (Entry.Model tipe)


-- INIT


init : Ctx.VersionContext -> (Model, Effects Action)
init context =
  ( Loading
  , getContext context
  )



-- UPDATE


type Action
    = LoadDocs String Docs.Package
    | LoadParsedDocs (List (Chunk Type.Type))
    | LoadReadme String
    | Fail Http.Error
    | NoOp


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
        ( model
        , Fx.none
        )

    Fail httpError ->
        ( Failed httpError
        , Fx.none
        )

    LoadReadme readme ->
        ( Readme readme
        , Fx.none
        )

    LoadDocs moduleName docs ->
        case Dict.get moduleName docs of
          Just moduleDocs ->
              let
                chunks =
                  toChunks moduleDocs
              in
                ( RawDocs (Info moduleName (toNameDict docs) chunks)
                , delayedTypeParse chunks
                )

          Nothing ->
              ( Failed (Http.UnexpectedPayload ("Could not find module '" ++ moduleName ++ "'"))
              , Fx.none
              )

    LoadParsedDocs newChunks ->
        case model of
          RawDocs info ->
              ( ParsedDocs { info | chunks = newChunks }
              , jumpToHash
              )

          _ ->
              ( Failed (Http.UnexpectedPayload ("Something went wrong parsing types."))
              , Fx.none
              )


toNameDict : Docs.Package -> Name.Dictionary
toNameDict pkg =
  Dict.map (\_ modul -> Set.fromList (Dict.keys modul.entries)) pkg



-- EFFECTS


getContext : Ctx.VersionContext -> Effects Action
getContext context =
  case context.moduleName of
    Nothing ->
      Ctx.getReadme context
        |> Task.map LoadReadme
        |> flip Task.onError (Task.succeed << Fail)
        |> Fx.task

    Just name ->
      Ctx.getDocs context
        |> Task.map (LoadDocs name)
        |> flip Task.onError (Task.succeed << Fail)
        |> Fx.task


delayedTypeParse : List (Chunk String) -> Effects Action
delayedTypeParse chunks =
  Fx.task <|
    Task.succeed () `Task.andThen` \_ ->
        Task.succeed (LoadParsedDocs (List.map (chunkMap stringToType) chunks))


chunkMap : (a -> b) -> Chunk a -> Chunk b
chunkMap func chunk =
  case chunk of
    Markdown md ->
      Markdown md

    Entry entry ->
      Entry (Entry.map func entry)


stringToType : String -> Type.Type
stringToType str =
  case Type.parse str of
    Ok tipe ->
      tipe

    Err _ ->
      Type.Var str


jumpToHash : Effects Action
jumpToHash =
  Native.Jump.jump
    |> Task.map (always NoOp)
    |> Fx.task



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [ class "entry-list" ] <|
    case model of
      Loading ->
          [ p [] [text "Loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Documentation did not load."]
          , p [] [text (toString httpError)]
          ]

      Readme readme ->
          [ Markdown.block readme
          ]

      RawDocs {name,chunks} ->
          h1 [class "entry-list-title"] [text name]
          :: List.map (viewChunk Entry.stringView) chunks

      ParsedDocs {name,nameDict,chunks} ->
          h1 [class "entry-list-title"] [text name]
          :: List.map (viewChunk (Entry.typeView nameDict)) chunks


viewChunk : (Entry.Model tipe -> Html) -> Chunk tipe -> Html
viewChunk entryView chunk =
  case chunk of
    Markdown md ->
        span [class "markdown-entry"] [ Markdown.block md ]

    Entry entry ->
        entryView entry




-- MAKE CHUNKS


toChunks : Docs.Module -> List (Chunk String)
toChunks moduleDocs =
  case String.split "\n@docs " moduleDocs.comment of
    [] ->
        Debug.crash "Expecting some documented functions in this module!"

    firstChunk :: rest ->
        Markdown firstChunk
        :: List.concatMap (subChunks moduleDocs) rest


subChunks : Docs.Module -> String -> List (Chunk String)
subChunks moduleDocs postDocs =
    case Regex.split (Regex.AtMost 1) (Regex.regex "\n") postDocs of
      [] ->
        Debug.crash "Expected a newline between @docs statements!"

      firstChunk :: rest ->
        let
          handleRest =
            if String.endsWith "," (String.trimRight firstChunk)
            then multiLineDocsChunk moduleDocs
            else List.map Markdown
        in
          subChunksHelp moduleDocs (String.split "," firstChunk) ++ handleRest rest


multiLineDocsChunk : Docs.Module -> List String -> List (Chunk String)
multiLineDocsChunk moduleDocs docsToParse =
  let
    errMsg =
      "Found an @docs listing that ended with a comma, but found no more docs on the next line!"
  in
    case docsToParse of
      [] ->
        Debug.crash errMsg

      [rest] ->
        if String.startsWith "\n" rest || String.startsWith "#" rest
        then Debug.crash errMsg
        else subChunks moduleDocs rest

      _ ->
        Debug.crash "This shouldn't happen: the Regex was only supposed to take one line"


subChunksHelp : Docs.Module -> List String -> List (Chunk String)
subChunksHelp moduleDocs parts =
  case parts of
    [] ->
        []

    rawPart :: remainingParts ->
        let
          part =
            String.trim rawPart
        in
          case isValue part of
            Just valueName ->
              toEntry moduleDocs valueName
              :: subChunksHelp moduleDocs remainingParts

            Nothing ->
              let
                trimmedPart =
                  String.trimLeft rawPart
              in
                case String.words trimmedPart of
                  [] ->
                      [ Markdown (String.join "," parts) ]

                  token :: _ ->
                      case isValue token of
                        Just valueName ->
                          [ toEntry moduleDocs valueName
                          , Markdown (String.dropLeft (String.length token) trimmedPart)
                          ]

                        Nothing ->
                          [ Markdown (String.join "," parts) ]


var : Regex.Regex
var =
  Regex.regex "^[a-zA-Z0-9_']+$"


operator : Regex.Regex
operator =
  Regex.regex "^\\([^a-zA-Z0-9]+\\)$"


isValue : String -> Maybe String
isValue str =
  if Regex.contains var str then
    Just str

  else if Regex.contains operator str then
    Just (String.dropLeft 1 (String.dropRight 1 str))

  else
    Nothing



toEntry : Docs.Module -> String -> Chunk String
toEntry moduleDocs name =
  case Dict.get name moduleDocs.entries of
    Nothing ->
        Debug.crash ("docs have been corrupted, could not find " ++ name)

    Just entry ->
        Entry entry

