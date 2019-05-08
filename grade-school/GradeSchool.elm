module GradeSchool exposing (addStudent, allStudents, empty, studentsInGrade)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Grade =
    Int


type alias Student =
    String


{-| Keep track of a school's students
Use Set to store students in each grade because it keeps them sorted.
-}
type alias School =
    Dict Grade (Set Student)


empty : School
empty =
    Dict.empty


addStudent : Grade -> Student -> School -> School
addStudent grade student school =
    Dict.update grade (insert student) school


insert student lookup =
    case lookup of
        Nothing ->
            Just (Set.singleton student)

        Just set ->
            Just (Set.insert student set)


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    Maybe.withDefault Set.empty (Dict.get grade school)
        |> Set.toList


allStudents : School -> List ( Grade, List Student )
allStudents school =
    Dict.toList school
        |> List.map (Tuple.mapSecond Set.toList)
