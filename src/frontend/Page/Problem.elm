module Page.Problem exposing
    ( missingModule
    , notFound
    , offline
    , styles
    )

import Elm.Version as V
import Href
import Html exposing (Attribute, Html, a, br, code, div, p, text)
import Html.Attributes exposing (href, style)



-- NOT FOUND


notFound : List (Html msg)
notFound =
    [ div [ style "font-size" "12em" ] [ text "404" ]
    , div [ style "font-size" "3em" ] [ text "I cannot find this page!" ]
    ]


styles : List (Attribute msg)
styles =
    [ style "text-align" "center"
    , style "color" "#9A9A9A"
    , style "padding" "6em 0"
    ]



-- OFFLINE


offline : String -> List (Html msg)
offline file =
    [ div [ style "font-size" "3em" ]
        [ text "Cannot find "
        , code [] [ text file ]
        ]
    , p [] [ text "Are you offline or something?" ]
    ]



-- MISSING MODULE


missingModule : String -> String -> Maybe V.Version -> String -> List (Html msg)
missingModule author project version _ =
    [ div [ style "font-size" "3em" ]
        [ text "Module not found"
        ]
    , p []
        [ text "Maybe it existed in a "
        , a [ href (Href.toProject author project) ] [ text "previous release" ]
        , text "?"
        , br [] []
        , text "Maybe the "
        , a [ href (Href.toVersion author project version) ] [ text "README" ]
        , text " will help you figure out what changed?"
        ]
    ]
