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


type alias PackageInfo =
  { package: Docs.Package
  , context : Ctx.VersionContext
  , nameDict : Name.Dictionary
  }


{-| All packages by canonicalized name, that is `user/project/version`.
-}
type alias Packages =
    Dict.Dict String PackageInfo


type alias Chunk tipe =
  { package : String
  , name : Name.Canonical
  , entry : Entry.Model tipe
  }


type alias Info tipe =
--   { packageDict : Packages
  { packageDict : Dict.Dict String Docs.Package
  , chunks : List (Chunk tipe)
  , query : String
  }


type Model
    = Loading
    | Failed Http.Error
    | Catalog (List Summary.Summary)
    | RawDocs (Info String)
    | ParsedDocs (Info Type.Type)



-- INIT


init : (Model, Effects Action)
init =
  ( Loading
  , getPackageInfo
  )



-- UPDATE


type Action
    = LoadCatalog (List Summary.Summary, List String)
    | LoadDocs Ctx.VersionContext Docs.Package
    | LoadParsedDocs (List (Chunk Type.Type))
    | Fail Http.Error
    | Query String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Query query ->
        flip (,) Fx.none <|
          case model of
            ParsedDocs info ->
                ParsedDocs { info | query = query }

            Catalog _ ->
                model

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

    LoadCatalog (allSummaries, updatedPkgs) ->
        let
          updatedSet =
            Set.fromList updatedPkgs

          (summaries, oldSummaries) =
            List.partition (\{name} -> Set.member name updatedSet) allSummaries

          contextEffects = summaries
            |> List.map latestVersionContext
            |> List.map getContext

        in
          ( Catalog summaries
          , Fx.batch contextEffects
          )

    LoadDocs {user, project, version} docs ->
        let
          pkgName = List.foldr (++) "" (List.intersperse "/" [user, project, version])

          m = case model of
            Loading -> "Loading"
            Failed _ -> "Failed"
            Catalog _ -> "Catalog"
            RawDocs _ -> "RawDocs"
            ParsedDocs _ -> "ParsedDocs"

          ms = Debug.log "model" m

          chunkEffects = docs
            |> Dict.toList
            |> List.map (\ (name, moduleDocs) -> delayedTypeParse (toChunks name moduleDocs))

        in
          ( RawDocs (Info (Dict.singleton pkgName docs) [] "")
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


latestVersionContext : Summary.Summary -> Ctx.VersionContext
latestVersionContext summary =
  let
    userProject = String.split "/" summary.name
    user = Maybe.withDefault "user" (List.head userProject)
    project = Maybe.withDefault "project" (List.head (List.reverse userProject))
    version = List.head summary.versions
      |> Maybe.withDefault (1,0,0)
      |> (\ (a,b,c) -> String.join "." (List.map toString [a,b,c]))
    allVersions = []
  in
    Ctx.VersionContext
      user
      project
      version
      allVersions
      -- TODO: Module name
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
      |> Task.map LoadCatalog
      |> flip Task.onError (Task.succeed << Fail)
      |> Fx.task


getContext : Ctx.VersionContext -> Effects Action
getContext context =
  Ctx.getDocs context
    |> Task.map (LoadDocs context)
    |> flip Task.onError (Task.succeed << Fail)
    |> Fx.task


delayedTypeParse : List (Chunk String) -> Effects Action
delayedTypeParse chunks =
  Fx.task <|
    Task.succeed () `Task.andThen` \_ ->
        Task.succeed (LoadParsedDocs (List.map (chunkMap stringToType) chunks))


chunkMap : (a -> b) -> Chunk a -> Chunk b
chunkMap func {name, entry} =
  Chunk "" name (Entry.map func entry)


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

      Catalog catalog ->
          [ p [] [text <| "Loading docs for " ++ toString (List.length catalog) ++ "packages..."]
          ]

      Failed httpError ->
          [ p [] [text "Documentation did not load or parse."]
          , p [] [text (toString httpError)]
          ]

      RawDocs {chunks} ->
          [ p [] [text "Parsing..."]
          ]

      ParsedDocs {packageDict,chunks,query} ->
        -- let
        --   pkgs = Debug.log "pkgs" packageDict
        -- in
          input
            [ placeholder "Search function by name or type"
            , value query
            , on "input" targetValue (Signal.message addr << Query)
            ]
            []
          :: viewSearchResults addr query chunks


viewSearchResults : Signal.Address Action -> String -> List (Chunk Type.Type) -> List Html
viewSearchResults addr query chunks =
  let
    queryType = stringToType query
    -- dict = Debug.log "nameDict" nameDict

  in
    if String.isEmpty query then
      [ h1 [] [ text "Welcome to Elm Search" ]
      , p [] [ text "Search the latest Elm libraries by either function name, or by approximate type signature."]
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
              |> List.map (\ {package, name, entry} -> Entry.typeViewAnnotation name Dict.empty entry)

        _ ->
            chunks
              -- TODO: clean this up
              |> List.map (\ {package, name, entry} -> (Entry.typeSimilarity queryType entry, (package, name, entry)))
              |> List.filter (\ (similarity, _) -> similarity > 10)
              |> List.sortBy (\ (similarity, _) -> -similarity)
              |> List.map (\ (_, chunk) -> chunk)
              |> List.map (\ (ctx, name, entry) -> Entry.typeViewAnnotation name (Dict.filter (\ key _ -> key == name.home) Dict.empty) entry)
            --   |> List.map (\ (ctx, name, entry) -> div [] [text (ctx.user ++ "." ++ ctx.project ++ "." ++ ctx.version ++ " : " ++ name.home ++ "." ++ name.name)])
            --   |> List.map (\ (ctx, name, entry) -> Name.toLink nameDict name)



-- MAKE CHUNKS


toChunks : String -> Docs.Module -> List (Chunk String)
toChunks ctx moduleDocs =
  case String.split "\n@docs " moduleDocs.comment of
    [] ->
        Debug.crash "Expecting some documented functions in this module!"

    firstChunk :: rest ->
        List.concatMap (subChunks ctx moduleDocs) rest


subChunks : String -> Docs.Module -> String -> List (Chunk String)
subChunks ctx moduleDocs postDocs =
    subChunksHelp ctx moduleDocs (String.split "," postDocs)


subChunksHelp : String -> Docs.Module -> List String -> List (Chunk String)
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



toEntry : String -> Docs.Module -> String -> Chunk String
toEntry ctx moduleDocs name =
  case Dict.get name moduleDocs.entries of
    Nothing ->
        Debug.crash ("docs have been corrupted, could not find " ++ name)

    Just entry ->
        Chunk "" (Name.Canonical moduleDocs.name name) entry
