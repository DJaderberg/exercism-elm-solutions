module SpaceAge exposing (Planet(..), ageOn)


type Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune


ageOn : Planet -> Float -> Float
ageOn planet seconds =
    seconds / orbitalPeriod planet

{-| Orbital period of a planet, in seconds
-}
orbitalPeriod : Planet -> Float
orbitalPeriod planet =
    -- Use earth's orbital period as a baseline
    31557600 * case planet of
        Mercury -> 0.2408467
        Venus -> 0.61519726
        Earth -> 1
        Mars -> 1.8808158
        Jupiter -> 11.862615
        Saturn -> 29.447498
        Uranus -> 84.016846
        Neptune -> 164.79132
