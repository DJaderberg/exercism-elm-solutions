module SumOfMultiples exposing (sumOfMultiples)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples multiples limit =
    List.range 1 (limit - 1)
        |> List.filter (divisibleByAny multiples)
        |> List.sum


divisibleByAny : List Int -> Int -> Bool
divisibleByAny multiples value =
    List.any (divisibleBy value) multiples


divisibleBy : Int -> Int -> Bool
divisibleBy nom denom =
    modBy denom nom == 0
