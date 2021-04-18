module Utils.Markdown exposing (block)

import Html
import Markdown exposing (defaultOptions)


block : String -> Html.Html msg
block raw =
    Markdown.toHtmlWith myOptions [] raw


myOptions : Markdown.Options
myOptions =
    { defaultOptions | defaultHighlighting = Just "elm" }
