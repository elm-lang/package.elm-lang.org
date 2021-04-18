module Href exposing
    ( toAbout
    , toModule
    , toProject
    , toVersion
    )

import Elm.Version as V
import Url.Builder as Url



-- HREFS


toProject : String -> String -> String
toProject author project =
    Url.absolute [ "packages", author, project, "" ] []


toVersion : String -> String -> Maybe V.Version -> String
toVersion author project version =
    Url.absolute [ "packages", author, project, vsnToString version, "" ] []


toModule : String -> String -> Maybe V.Version -> String -> Maybe String -> String
toModule author project version moduleName maybeValue =
    Url.custom Url.Absolute [ "packages", author, project, vsnToString version, String.replace "." "-" moduleName ] [] maybeValue


toAbout : String -> String -> Maybe V.Version -> String
toAbout author project version =
    Url.absolute [ "packages", author, project, vsnToString version, "about" ] []



-- HELPERS


vsnToString : Maybe V.Version -> String
vsnToString maybeVersion =
    case maybeVersion of
        Nothing ->
            "latest"

        Just version ->
            V.toString version
