module Anagram exposing (detect)

import String exposing (toList, toUpper)


detect : String -> List String -> List String
detect word candidates =
    List.filter (anagram word) candidates


anagram : String -> String -> Bool
anagram a b =
    toUpper a /= toUpper b && upperSortedList a == upperSortedList b


upperSortedList =
    toUpper >> toList >> List.sort
