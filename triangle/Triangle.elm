module Triangle exposing (Triangle(..), triangleKind)

import Maybe.Extra
import Set exposing (Set)


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
    case error x y z of
        Nothing ->
            Ok (kind x y z)

        Just str ->
            Err str


kind : number -> number -> number -> Triangle
kind x y z =
    case [ x, y, z ] |> Set.fromList |> Set.size of
        1 ->
            Equilateral

        2 ->
            Isosceles

        _ ->
            Scalene


{-| Point out the problem with any illegal triangles.
Side length issues are returned when input raises both side length and triangle inequality issues.
-}
error : number -> number -> number -> Maybe String
error x y z =
    Maybe.Extra.or (sideLengths x y z) (inequality x y z)


sideLengths : number -> number -> number -> Maybe String
sideLengths x y z =
    if x > 0 && y > 0 && z > 0 then
        Nothing

    else
        Just "Invalid lengths"


inequality : number -> number -> number -> Maybe String
inequality x y z =
    case List.sort [ x, y, z ] of
        [ a, b, c ] ->
            if c > a + b then
                Just "Violates inequality"

            else if c == a + b then
                Just "Degenerate triangle"

            else
                Nothing

        _ ->
            Just "Unknown triangle inequality error"
