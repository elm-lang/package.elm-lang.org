module Component.Search where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Regex
import Set
import String
import Task

import Docs.Summary as Summary
import Docs.Entry as Entry
import Docs.Name as Name
import Docs.Package as Docs
import Docs.Type as Type
import Page.Context as Ctx
import Parse.Type as Type



-- MODEL


type Model
    = Loading
    | Failed Http.Error
    | Catalog (List Summary.Summary)
    | Docs Info


type alias Info =
    { packageDict : Packages
    , chunks : List Chunk
    , query : String
    , failed : List Summary.Summary
    }


type alias PackageIdentifier =
    String


type alias Packages =
    Dict.Dict PackageIdentifier PackageInfo


type alias PackageInfo =
  { package: Docs.Package
  , context : Ctx.VersionContext
  , nameDict : Name.Dictionary
  }


type alias Chunk =
  { package : PackageIdentifier
  , name : Name.Canonical
  , entry : Entry.Model Type.Type
  }


-- INIT


init : (Model, Effects Action)
init =
  ( Loading
  , getPackageInfo
  )


-- UPDATE


type Action
    = Fail Http.Error
    | Load (List Summary.Summary, List String)
    | FailDocs Summary.Summary
    | LoadDocs Ctx.VersionContext Docs.Package
    | Query String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Query query ->
        flip (,) Fx.none <|
          case model of
            Docs info ->
                Docs { info | query = query }

            _ ->
                model

    Fail httpError ->
        ( Failed httpError
        , Fx.none
        )

    Load (allSummaries, updatedPkgs) ->
        let
          updatedSet =
            Set.fromList updatedPkgs

          (summaries, oldSummaries) =
            List.partition (\{name} -> Set.member name updatedSet) allSummaries

          contextEffects = List.map getDocs summaries

        in
          ( Catalog summaries
          , Fx.batch contextEffects
          )

    FailDocs summary ->
        case model of
          Docs info ->
              ( Docs { info | failed = summary :: info.failed }
              , Fx.none
              )

          _ ->
              ( Docs (Info (Dict.empty) [] "" [summary])
              , Fx.none
              )

    LoadDocs ctx docs ->
        let
          {user, project, version} = ctx

          pkgName = List.foldr (++) "" (List.intersperse "/" [user, project, version])

          pkgInfo = PackageInfo docs ctx (toNameDict docs)

          chunks = docs
            |> Dict.toList
            |> List.map (\ (name, moduleDocs) -> toChunks pkgName moduleDocs)
            |> List.concat

        in
          case model of
            Docs info ->
                ( Docs
                    { info
                    | packageDict = Dict.insert pkgName pkgInfo info.packageDict
                    , chunks = List.append info.chunks chunks
                    }
                , Fx.none
                )

            _ ->
                ( Docs (Info (Dict.singleton pkgName pkgInfo) chunks "" [])
                , Fx.none
                )


toNameDict : Docs.Package -> Name.Dictionary
toNameDict pkg =
  Dict.map (\_ modul -> Set.fromList (Dict.keys modul.entries)) pkg


latestVersionContext : Summary.Summary -> Ctx.VersionContext
latestVersionContext summary =
  let
    userProject = String.split "/" summary.name
    user = Maybe.withDefault "user" (List.head userProject)
    project = Maybe.withDefault "project" (List.head (List.reverse userProject))
    version = List.head summary.versions
      |> Maybe.withDefault (1,0,0)
      |> (\ (a,b,c) -> String.join "." (List.map toString [a,b,c]))
  in
    Ctx.VersionContext
      user
      project
      version
      []
      Nothing


-- EFFECTS


getPackageInfo : Effects Action
getPackageInfo =
  let
    getAll =
      Http.get Summary.decoder "/all-packages"

    getNew =
      Http.get (Json.list Json.string) "/new-packages"

  in
    Task.map2 (,) getAll getNew
      |> Task.map Load
      |> flip Task.onError (Task.succeed << Fail)
      |> Fx.task


getDocs : Summary.Summary -> Effects Action
getDocs summary =
  let
    context = latestVersionContext summary
  in
    Ctx.getDocs context
        |> Task.map (LoadDocs context)
        |> (flip Task.onError) (always (Task.succeed (FailDocs summary)))
        |> Fx.task


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
  div [class "search"] <|
    case model of
      Loading ->
          [ p [] [text "Loading list of packages..."]
          ]

      Failed httpError ->
          [ p [] [text "Package summary did not load."]
          , p [] [text (toString httpError)]
          ]

      Catalog catalog ->
          [ p [] [text <| "Loading docs for " ++ toString (List.length catalog) ++ "packages..."]
          ]

      Docs {packageDict,chunks,query} ->
          input
            [ placeholder "Search function by name or type"
            , value query
            , on "input" targetValue (Signal.message addr << Query)
            ]
            []
          :: viewSearchResults packageDict addr query chunks


viewSearchResults : Packages -> Signal.Address Action -> String -> List Chunk -> List Html
viewSearchResults packageDict addr query chunks =
  let
    queryType = stringToType query
    -- dict = Debug.log "nameDict" nameDict

    nameDictFor name =
        case Dict.get name packageDict of
            Just info
                -> .nameDict info
            Nothing
                -> Dict.empty

  in
    if String.isEmpty query then
      [ h1 [] [ text "Welcome to Elm Search" ]
      , p [] [ text "Search the latest Elm libraries by either function name, or by approximate type signature."]
      , p [] [ text (toString <| List.length chunks) ]
      , h2 [] [ text "Example searches" ]
      , ul []
        [ li [] [ a [ onClick addr (Query "map")] [ text "map" ] ]
        , li [] [ a
          [ onClick addr (Query "(a -> b -> b) -> b -> List a -> b")]
          [ text "(a -> b -> b) -> b -> List a -> b" ] ]
        ]
      ]

    else
      case queryType of
        Type.Var string ->
            chunks
              |> List.filter (\ {package, name, entry} -> Entry.typeContainsQuery query entry)
              |> List.map (\ {package, name, entry} -> Entry.typeViewAnnotation name (nameDictFor package) entry)

        _ ->
            chunks
              -- TODO: clean this up
              |> List.map (\ {package, name, entry} -> (Entry.typeSimilarity queryType entry, (package, name, entry)))
              |> List.filter (\ (similarity, _) -> similarity > 10)
              |> List.sortBy (\ (similarity, _) -> -similarity)
              |> List.map (\ (_, chunk) -> chunk)
              |> List.map (\ (package, name, entry) -> Entry.typeViewAnnotation name (nameDictFor package) entry)



-- MAKE CHUNKS


toChunks : String -> Docs.Module -> List Chunk
toChunks ctx moduleDocs =
  case String.split "\n@docs " moduleDocs.comment of
    [] ->
        Debug.crash "Expecting some documented functions in this module!"

    firstChunk :: rest ->
        List.concatMap (subChunks ctx moduleDocs) rest


subChunks : String -> Docs.Module -> String -> List Chunk
subChunks ctx moduleDocs postDocs =
    subChunksHelp ctx moduleDocs (String.split "," postDocs)


subChunksHelp : String -> Docs.Module -> List String -> List Chunk
subChunksHelp ctx moduleDocs parts =
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
              toEntry ctx moduleDocs valueName
              :: subChunksHelp ctx moduleDocs remainingParts

            Nothing ->
              let
                trimmedPart =
                  String.trimLeft rawPart
              in
                case String.words trimmedPart of
                  [] ->
                      []

                  token :: _ ->
                      case isValue token of
                        Just valueName ->
                          [ toEntry ctx moduleDocs valueName ]

                        Nothing ->
                          []


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



toEntry : String -> Docs.Module -> String -> Chunk
toEntry pkgName moduleDocs name =
  case Dict.get name moduleDocs.entries of
    Nothing ->
        Debug.crash ("docs have been corrupted, could not find " ++ name)

    Just entry ->
        Chunk
            pkgName
            (Name.Canonical moduleDocs.name name)
            (Entry.map stringToType entry)
