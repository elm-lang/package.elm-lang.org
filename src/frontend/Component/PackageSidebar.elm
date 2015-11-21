module Component.PackageSidebar where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Regex
import String
import Task

import Docs.Package as Docs
import Docs.Entry as Entry
import Page.Context as Ctx
import Utils.Markdown as Markdown
import Utils.Path as Path exposing ((</>))


type Model
    = Loading
    | Failed Http.Error
    | Success
        { context : Ctx.VersionContext
        , searchDict : SearchDict
        , query : String
        }


type alias SearchDict =
    Dict.Dict String (List (String, String))
    -- moduleName => List (displayName, linkName)



-- INIT


init : Ctx.VersionContext -> (Model, Effects Action)
init context =
  ( Loading
  , loadDocs context
  )



-- UPDATE


type Action
    = Fail Http.Error
    | Load Ctx.VersionContext SearchDict
    | Query String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Query query ->
      flip (,) Fx.none <|
        case model of
          Success facts ->
              Success { facts | query = query }

          Loading ->
              model

          Failed err ->
              model

    Fail httpError ->
        ( Failed httpError
        , Fx.none
        )

    Load context searchDict ->
        ( Success
            { context = context
            , searchDict = searchDict
            , query = ""
            }
        , Fx.none
        )



-- EFFECTS


loadDocs : Ctx.VersionContext -> Effects Action
loadDocs context =
  Ctx.getDocs context
    |> Task.map (Load context << toSearchDict)
    |> flip Task.onError (Task.succeed << Fail)
    |> Fx.task


toSearchDict : Docs.Package -> SearchDict
toSearchDict pkg =
  Dict.map (\_ modul ->
    let entryNames = Dict.keys modul.entries
        tagNames =
          Dict.values modul.entries |> List.concatMap
            (\entry ->
              case entry.info of
                Entry.Union {tags} -> List.map (\tag -> (tag.tag, entry.name)) tags
                _ -> [])
          |> List.filter (uncurry (/=))
    in tagNames ++ List.map2 (,) entryNames entryNames) pkg



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [class "pkg-nav"] <|
    case model of
      Loading ->
          [ p [] [text "Loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Problem loading!"]
          , p [] [text (toString httpError)]
          ]

      Success {context, query, searchDict} ->
          [ moduleLink context Nothing
          , br [] []
          , githubLink context
          , h2 [] [ text "Module Docs" ]
          , input
              [ placeholder "Search"
              , value query
              , on "input" targetValue (Signal.message addr << Query)
              ]
              []
          , viewSearchDict context query searchDict
          ]


viewSearchDict : Ctx.VersionContext -> String -> SearchDict -> Html
viewSearchDict context query searchDict =
  if String.isEmpty query then
    ul [] (List.map (li [] << singleton << moduleLink context << Just) (Dict.keys searchDict))

  else
    let
      lowerQuery =
        String.toLower query

      containsQuery value =
        String.contains lowerQuery (String.toLower value)

      searchResults =
        Dict.map (\_ values -> List.filter (fst>>containsQuery) values) searchDict
          |> Dict.filter (\_ values -> not (List.isEmpty values))
          |> Dict.toList
    in
      ul [] (List.map (viewModuleLinks context) searchResults)


viewModuleLinks : Ctx.VersionContext -> (String, List (String, String)) -> Html
viewModuleLinks context (name, values) =
  li
    [ class "pkg-nav-search-chunk" ]
    [ moduleLink context (Just name)
    , ul [] (List.map (valueLink context name) values)
    ]


githubLink : Ctx.VersionContext -> Html
githubLink context =
  a [ class "pkg-nav-module"
    , href ("https://github.com" </> context.user </> context.project </> "tree" </> context.version)
    ]
    [ text "Browse source" ]


moduleLink : Ctx.VersionContext -> Maybe String -> Html
moduleLink context name =
  let
    visibleName =
      Maybe.withDefault "README" name

    url =
      Ctx.pathTo context (Maybe.withDefault "" (Maybe.map Path.hyphenate name))

    visibleText =
      if context.moduleName == name then
          span [ style [ "font-weight" => "bold", "text-decoration" => "underline" ] ] [ text visibleName ]

      else
          text visibleName
  in
    a [ class "pkg-nav-module", href url ] [ visibleText ]


valueLink : Ctx.VersionContext -> String -> (String, String) -> Html
valueLink context moduleName (displayName, linkName) =
  li
    [ class "pkg-nav-value"
    ]
    [ a [ href (Ctx.pathTo context (Path.hyphenate moduleName) ++ "#" ++ linkName) ] [ text displayName ]
    ]


singleton : a -> List a
singleton x =
  [x]
