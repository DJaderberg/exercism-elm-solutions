module ScrabbleScore exposing (scoreWord)

import Dict exposing (Dict)


type alias Language =
    Dict Char Int


scoreWord : String -> Int
scoreWord word =
    word
        |> String.toUpper
        |> String.toList
        |> List.filterMap (scoreLetter english)
        |> List.sum


scoreLetter : Language -> Char -> Maybe Int
scoreLetter lang char =
    Dict.get char lang


english : Language
english =
    createLanguage
        [ ( "AEIOULNRST", 1 )
        , ( "DG", 2 )
        , ( "BCMP", 3 )
        , ( "FHVWY", 4 )
        , ( "K", 5 )
        , ( "JX", 8 )
        , ( "QZ", 10 )
        ]


createLanguage : List ( String, Int ) -> Language
createLanguage list =
    list
        |> List.concatMap createLanguagePart
        |> Dict.fromList


createLanguagePart : ( String, Int ) -> List ( Char, Int )
createLanguagePart ( str, val ) =
    str
        |> String.toList
        |> List.map (\c -> ( c, val ))
