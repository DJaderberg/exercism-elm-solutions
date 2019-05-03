module SumOfMultiples exposing (sumOfMultiples)

import Set


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples multiples limit =
    List.concatMap (multiple limit) multiples
        |> Set.fromList
        |> Set.foldl (+) 0


multiple limit base =
    List.range 1 (limit - 1)
        |> List.filter (modBy base >> (==) 0)
