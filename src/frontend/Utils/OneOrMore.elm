module Utils.OneOrMore exposing
    ( OneOrMore(..)
    , append
    , decoder
    , head
    , map
    , tail
    , toList
    )

import Json.Decode as Decode



-- ONE OR MORE


type OneOrMore a
    = OneOrMore a (List a)


head : OneOrMore a -> a
head (OneOrMore x _) =
    x


tail : OneOrMore a -> List a
tail (OneOrMore _ xs) =
    xs


append : OneOrMore a -> OneOrMore a -> OneOrMore a
append (OneOrMore x xs) (OneOrMore y ys) =
    OneOrMore x (xs ++ y :: ys)


toList : OneOrMore a -> List a
toList (OneOrMore x xs) =
    x :: xs



-- MAPPING


map : (a -> b) -> OneOrMore a -> OneOrMore b
map func (OneOrMore x xs) =
    OneOrMore (func x) (List.map func xs)



-- DECODER


decoder : Decode.Decoder a -> Decode.Decoder (OneOrMore a)
decoder entryDecoder =
    Decode.list entryDecoder
        |> Decode.andThen checkList


checkList : List a -> Decode.Decoder (OneOrMore a)
checkList list =
    case list of
        [] ->
            Decode.fail "An array with one or more elements."

        x :: xs ->
            Decode.succeed (OneOrMore x xs)
