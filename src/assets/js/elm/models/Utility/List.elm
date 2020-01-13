module Models.Utility.List exposing (..)

import List.Extra as ExList


exceptList : List a -> List a -> List a
exceptList list target =
    exceptListHelp list target


exceptListHelp : List a -> List a -> List a
exceptListHelp remaining accumulator =
    case remaining of
        [] ->
            accumulator

        first :: rest ->
            exceptListHelp rest (ExList.remove first accumulator)
