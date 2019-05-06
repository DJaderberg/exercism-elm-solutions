module SumOfMultiples exposing (sumOfMultiples)

import List.Extra


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples multiples limit =
    List.concatMap (multiplesTo limit) multiples
        |> List.Extra.unique
        |> List.sum


multiplesTo limit factor =
    List.range 1 ((limit - 1) // factor)
        |> List.map ((*) factor)
