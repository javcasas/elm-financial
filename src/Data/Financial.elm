module Data.Financial
    ( Financial
    , fromInt
    , withDecimalsFromInt
    , genPercentage
    , significativeDigits
    , max_significative_digits
    , add
    , sub
    , mul
    , div
    , opposite
    , toString
    , toAccountingString
    , roundToDecimals
    , tests) where

import String
import Html
import Basics

type alias Decimals = Int
type alias Mantissa = Int

type Financial
    = Normal Mantissa Decimals
    | Invalid


fromInt : Int -> Financial
fromInt x = Normal x 0


withDecimalsFromInt : Int -> Int -> Financial
withDecimalsFromInt d i = Normal i d


significativeDigits : Int -> Int
significativeDigits v =
    let i = abs v in
    if i >= 10
    then 1 + significativeDigits (i // 10)
    else 1


max_significative_digits : Int
max_significative_digits =
    let valid_significative_digits v =
        let base = 10 ^ v in
        base + 1 > base
    in
    let max_digits c =
        if valid_significative_digits (c + 1)
        then max_digits (c + 1)
        else c - 1
    in
    max_digits 1


addDecimals : Int -> Financial -> Financial
addDecimals new_decimals f =
    let to_multiply = 10 ^ new_decimals in
    case f of
        Invalid ->
            Invalid
        Normal m current_decimals ->
            if current_decimals + new_decimals + significativeDigits m > max_significative_digits
            then Invalid
            else Normal (m * to_multiply) (current_decimals + new_decimals)


toCommonDecimals : (Financial, Financial) -> (Financial, Financial)
toCommonDecimals (f1, f2) =
    case (f1, f2) of
        (Normal m1 d1, Normal m2 d2) ->
            if d1 == d2
            then (f1, f2)
            else if d1 > d2 
            then (f1, addDecimals (d1 - d2) f2)
            else (addDecimals (d2 - d1) f1, f2)
        _ -> (f1, f2)


add : Financial -> Financial -> Financial
add f1 f2 =
    let (f3, f4) = toCommonDecimals (f1, f2) in
    case (f3, f4) of
        (Normal m1 d1, Normal m2 d2) ->
            Normal (m1 + m2) d1
        _ -> Invalid


sub : Financial -> Financial -> Financial
sub f1 f2 = f1 `add` (opposite f2)


mul : Financial -> Financial -> Financial
mul f1 f2 =
    case (f1, f2) of
        (Normal m1 d1, Normal m2 d2) ->
            if significativeDigits m1 + significativeDigits m2 > max_significative_digits
            then Invalid
            else Normal (m1 * m2) (d1 + d2)
        _ -> Invalid


genPercentage : Int -> Financial
genPercentage p = Normal p 2


opposite : Financial -> Financial
opposite f =
    case f of
        Normal m d ->
            Normal (-m) d
        _ -> Invalid

    
toBaseString : Financial -> String
toBaseString f =
    case f of
        Invalid -> "Invalid"
        Normal m d ->
            let base = Basics.toString (abs m) in
            let base_with_lead_zeros =
                if String.length base <= d
                then (String.repeat (1 + (d - String.length base)) "0") ++ base
                else base
            in
            String.dropRight d base_with_lead_zeros ++ "." ++ String.right d base_with_lead_zeros


toString : Financial -> String
toString f =
    case f of
        Invalid -> "Invalid"
        Normal m d ->
            if m < 0
            then "-" ++ (toBaseString f)
            else toBaseString f


toAccountingString : Financial -> String
toAccountingString f =
    case f of
        Invalid -> "Invalid"
        Normal m d ->
            if m < 0
            then "(" ++ toBaseString f ++ ")"
            else toBaseString f


roundToDecimals : Int -> Financial -> Financial
roundToDecimals new_decimals f =
    case f of
        Invalid -> Invalid
        Normal mantissa curr_decimals ->
            if curr_decimals == new_decimals
            then f
            else if curr_decimals < new_decimals
            then addDecimals (new_decimals - curr_decimals) f
            else let digits_to_remove = curr_decimals - new_decimals
                     mod_factor = 10 ^ digits_to_remove
                     limit_test = mod_factor // 2
                     divide = mantissa // mod_factor
                     remainder = (abs mantissa) `rem` mod_factor
                     direction = if mantissa >= 0 then 1 else -1 in
            if remainder >= limit_test
            then Normal (divide + direction) new_decimals
            else Normal divide new_decimals


prepare_num_for_div : Financial -> Financial
prepare_num_for_div f =
    let f2 = addDecimals 1 f in
    case f2 of
        Invalid -> f
        _ -> prepare_num_for_div f2


prepare_den_for_div : Financial -> Financial
prepare_den_for_div f =
    case f of
        Invalid -> f
        Normal m d ->
            if m == 0
            then f
            else if m `rem` 10 == 0
            then prepare_den_for_div (Normal (m // 10) (d - 1))
            else f


div : Financial -> Financial -> Financial
div f1 f2 =
    let f1p = prepare_num_for_div f1 in
    let f2p = prepare_den_for_div f2 in
    case (f1p, f2p) of
        (Normal m1 d1, Normal m2 d2) ->
            Normal (m1 // m2) (d1 - d2)
        _ -> Invalid


tests : List (String, Bool)
tests = [ ("Significative Digits 1", significativeDigits 100 == 3)
        , ("Significative Digits 2", significativeDigits -100 == 3)
        , ("Significative Digits 3", significativeDigits 10000000000 == 11)
        , ("Max Significative Digits for this implementation: " ++ Basics.toString max_significative_digits, max_significative_digits > 10)
        , ("toString 1", toString (Normal -100 2) == "-1.00")
        , ("toString 2", toString (Normal 100 2) == "1.00")
        , ("toString 3", toString (Normal 1 2) == "0.01")
        , ("toString 4", toString (Normal 10 2) == "0.10")
        , ("toString 5", toString (Normal -100 2) == "-1.00")
        , ("toAccountingString 1", toAccountingString (Normal -100 2) == "(1.00)")
        , ("toAccountingString 2", toAccountingString (Normal 100 2) == "1.00")
        , ("addDecimals 1", addDecimals 2 (Normal 1 0) == Normal 100 2)
        , ("roundToDecimals 1", roundToDecimals 2 (Normal 1 0) == Normal 100 2)
        , ("roundToDecimals 2", roundToDecimals 0 (Normal 15 1) == Normal 2 0)
        , ("roundToDecimals 3", roundToDecimals 0 (Normal -15 1) == Normal -2 0)
        , ("roundToDecimals 4", roundToDecimals 10 (Normal -1 0) == Normal -10000000000 10)
        , ("toCommonDecimals 1", toCommonDecimals (Normal -1 0, Normal -10000000000 10)
                                 == (Normal -10000000000 10, Normal -10000000000 10))
        , ("add 1", Normal -1 0 `add` Normal -10000000000 10
                                 == Normal -20000000000 10)
        , ("substract 1", Normal -1 0 `sub` Normal -10000000000 10
                                 == Normal 0 10)
        , ("mul 1", Normal 1 0 `mul` Normal 1 0
                                 == Normal 1 0)
        , ("mul 2", Normal 10000000000 0 `mul` Normal 10000000000 0
                                 == Invalid)
        , ("div 1 " ++ Basics.toString (120000000 // 18), roundToDecimals 4 (Normal 1200 0 `div` Normal 1800 0)
                                 == Normal 6667 4)
        , ("equals 1", Normal 1 0 == Normal 1 0)
        , ("equals 2", Normal 1 0 /= Normal 10 1)
        , ("genPercentage 1", genPercentage 12 == Normal 12 2)
        , ("genPercentage 2", roundToDecimals 0 (Normal 100 0 `mul` genPercentage 10) == Normal 10 0)
        ]
