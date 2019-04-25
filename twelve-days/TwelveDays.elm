module TwelveDays exposing (recite)

import Dict


recite : Int -> Int -> List String
recite start stop =
    List.range start stop
        |> List.map (\n -> initial n ++ gifts n ++ ".")


{-| Say: "On the nth day of Christmas my true love gave to me" |
-}
initial : Int -> String
initial n =
    "On the " ++ numeral n ++ " day of Christmas my true love gave to me"


numeral : Int -> String
numeral n =
    let
        dict =
            Dict.fromList
                [ ( 1, "first" )
                , ( 2, "second" )
                , ( 3, "third" )
                , ( 4, "fourth" )
                , ( 5, "fifth" )
                , ( 6, "sixth" )
                , ( 7, "seventh" )
                , ( 8, "eighth" )
                , ( 9, "ninth" )
                , ( 10, "tenth" )
                , ( 11, "eleventh" )
                , ( 12, "twelfth" )
                ]
    in
    Maybe.withDefault "" (Dict.get n dict)


{-| Describe all gifts that were given in a day |
-}
gifts : Int -> String
gifts start =
    List.range 1 start
        |> List.reverse
        |> List.map gift
        |> addPunctuation ""


addPunctuation state list =
    case list of
        a :: [] ->
            state ++ ", " ++ a

        a :: b :: [] ->
            state ++ ", " ++ a ++ ", and " ++ b

        a :: t ->
            addPunctuation (state ++ ", " ++ a) t

        [] ->
            state


{-| Describe which gift was added on a certain day |
-}
gift : Int -> String
gift n =
    let
        dict =
            Dict.fromList
                [ ( 1, "a Partridge in a Pear Tree" )
                , ( 2, "two Turtle Doves" )
                , ( 3, "three French Hens" )
                , ( 4, "four Calling Birds" )
                , ( 5, "five Gold Rings" )
                , ( 6, "six Geese-a-Laying" )
                , ( 7, "seven Swans-a-Swimming" )
                , ( 8, "eight Maids-a-Milking" )
                , ( 9, "nine Ladies Dancing" )
                , ( 10, "ten Lords-a-Leaping" )
                , ( 11, "eleven Pipers Piping" )
                , ( 12, "twelve Drummers Drumming" )
                ]
    in
    Maybe.withDefault "" (Dict.get n dict)
