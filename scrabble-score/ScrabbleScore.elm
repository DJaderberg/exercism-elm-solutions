module ScrabbleScore exposing (scoreWord)

import Dict exposing (Dict)
import Maybe.Extra


type alias Language =
    Dict Char Int


scoreWord : String -> Int
scoreWord word =
    word
        |> String.toUpper
        |> String.toList
        |> List.map (scoreLetter english)
        |> Maybe.Extra.values
        |> List.foldl (+) 0


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
        |> List.map createLanguagePart
        |> List.foldr Dict.union Dict.empty


createLanguagePart : ( String, Int ) -> Language
createLanguagePart ( str, val ) =
    str
        |> String.toList
        |> List.map (\c -> ( c, val ))
        |> Dict.fromList
