import Html exposing (Html, text, div)

import Data.Financial exposing (..)
import List

main = testSuite

testHtml : (String, Bool) -> Html
testHtml (s, v) =
    Html.div []
        [
            if v
            then text (s ++ " Pass")
            else text (s ++ " Fail")
        ]

testSuite : Html
testSuite = 
    Html.div []
        ((List.map testHtml tests)
        ++ [allPassHtml])

allPass : Bool
allPass =
    let tests_pass l =
        case l of
            (t, v)::tx -> if v then tests_pass tx else False
            [] -> True
    in
    tests_pass tests

allPassHtml : Html
allPassHtml =
    Html.div []
        [
            if allPass
            then text ("All Pass")
            else text ("Some Failures")
        ]
