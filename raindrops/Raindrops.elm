module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number =
    case soundsOf number of
        [] ->
            String.fromInt number

        sounds ->
            List.foldr (++) "" sounds


soundsOf : Int -> List String
soundsOf number =
    [ ( 3, "Pling" ), ( 5, "Plang" ), ( 7, "Plong" ) ]
        |> List.filter (Tuple.first >> divides number)
        |> List.map Tuple.second


divides number divisor =
    modBy divisor number == 0
