module Accumulate exposing (accumulate)


accumulate : (a -> b) -> List a -> List b
accumulate func input =
    case input of
        a :: b ->
            func a :: accumulate func b

        [] ->
            []
