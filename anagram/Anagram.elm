module Anagram exposing (detect)

import Dict exposing (Dict)


detect : String -> List String -> List String
detect word candidates =
    List.filter (anagram word) candidates


anagram : String -> String -> Bool
anagram a b =
    anagramCaseSensitive (String.toUpper a) (String.toUpper b)


anagramCaseSensitive a b =
    a /= b && count a == count b


{-| Count how many times each Char occurs in a String
-}
count : String -> Dict Char Int
count s =
    countHelper (String.toList s) Dict.empty


countHelper : List Char -> Dict Char Int -> Dict Char Int
countHelper list dict =
    case list of
        c :: cs ->
            countHelper cs (Dict.update c increment dict)

        [] ->
            dict


increment m =
    Just (1 + Maybe.withDefault 0 m)
