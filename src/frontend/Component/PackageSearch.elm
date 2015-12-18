module Component.PackageSearch where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    | RawDocs (Info String)
    | ParsedDocs (Info Type.Type)


type alias Info tipe =
  { name : String
  , nameDict : Name.Dictionary
  , chunks : List (Chunk tipe)
  , query : String
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
    = LoadDocs Docs.Package
    | LoadParsedDocs (List (Chunk Type.Type))
    | Fail Http.Error
    | Query String
    | NoOp


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
        ( model
        , Fx.none
        )

    Query query ->
        flip (,) Fx.none <|
          case model of
            ParsedDocs info ->
                ParsedDocs { info | query = query }

            RawDocs _ ->
                model

            Loading ->
                model

            Failed err ->
                model

    Fail httpError ->
        ( Failed httpError
        , Fx.none
        )

    LoadDocs docs ->
        let
          chunkEffects = docs
            |> Dict.toList
            |> List.map (\ (_, moduleDocs) -> delayedTypeParse (toChunks moduleDocs))

        in
          ( RawDocs (Info "" (toNameDict docs) [] "")
          , Fx.batch chunkEffects
          )

    LoadParsedDocs newChunks ->
        case model of
          RawDocs info ->
              ( ParsedDocs { info | chunks = newChunks }
              , Fx.none
              )

          ParsedDocs info ->
              ( ParsedDocs { info | chunks = info.chunks ++ newChunks }
              , Fx.none
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
  Ctx.getDocs context
    |> Task.map LoadDocs
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



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [class "pkg-search"] <|
    case model of
      Loading ->
          [ p [] [text "Loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Documentation did not load or parse."]
          , p [] [text (toString httpError)]
          ]

      RawDocs {name,chunks} ->
          [ p [] [text "Parsing..."]
          ]

      ParsedDocs {name,nameDict,chunks,query} ->
          input
            [ placeholder "Search function by name or type"
            , value query
            , on "input" targetValue (Signal.message addr << Query)
            ]
            []
          :: viewSearchResults nameDict query chunks


viewSearchResults : Name.Dictionary -> String -> List (Chunk Type.Type) -> List Html
viewSearchResults nameDict query chunks =
  let
    toEntry chunk =
      case chunk of
        Entry entry ->
          Just entry
        _ ->
          Nothing

    entries = List.filterMap toEntry chunks

    queryType = stringToType query

  in
    if String.isEmpty query then
      []

    else
      case queryType of
        Type.Var string ->
            entries
              |> List.filter (Entry.typeContainsQuery query)
              |> List.map (Entry.typeViewAnnotation nameDict)

        _ ->
            entries
              |> List.map (\ entry -> (Entry.typeSimilarity queryType entry,  entry))
              |> List.filter (\ (similarity, _) -> similarity > 0)
              |> List.sortBy (\ (similarity, _) -> -similarity)
              |> List.map (\ (_, entry) -> entry)
              |> List.map (Entry.typeViewAnnotation nameDict)



viewChunk : (Entry.Model tipe -> Html) -> Chunk tipe -> Html
viewChunk entryView chunk =
  case chunk of
    Markdown _ ->
        div [] []

    Entry entry ->
        entryView entry


-- MAKE CHUNKS


toChunks : Docs.Module -> List (Chunk String)
toChunks moduleDocs =
  case String.split "\n@docs " moduleDocs.comment of
    [] ->
        Debug.crash "Expecting some documented functions in this module!"

    firstChunk :: rest ->
        List.concatMap (subChunks moduleDocs) rest


subChunks : Docs.Module -> String -> List (Chunk String)
subChunks moduleDocs postDocs =
    subChunksHelp moduleDocs (String.split "," postDocs)


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

