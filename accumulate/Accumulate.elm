module Accumulate exposing (accumulate)


accumulate : (a -> b) -> List a -> List b
accumulate func input =
    accumulateHelp func [] (List.reverse input)


accumulateHelp func state input =
    case input of
        a :: b ->
            accumulateHelp func (func a :: state) b

        [] ->
            state
