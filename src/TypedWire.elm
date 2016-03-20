module TypedWire
  ( AsBase64(..)
  , decAsBase64, encAsBase64
  , Date
  , toStdDate, fromStdDate
  , decDate, encDate
  , Time
  , toStdTime, fromStdTime
  , decTime, encTime
  , DateTime
  , toStdDateTime, fromStdDateTime
  , decDateTime, encDateTime
  , encMaybe
  ) where

{-| Helper functions for defining typed-wire json representations

# Maybe
@docs encMaybe

# Bytes
@docs AsBase64, decAsBase64, encAsBase64

# Date
@docs Date, toStdDate, fromStdDate, decDate, encDate

# Time
@docs Time, toStdTime, fromStdTime, decTime, encTime

# DateTime
@docs DateTime, toStdDateTime, fromStdDateTime, decDateTime, encDateTime
-}

import Date as D
import Time as T
import String as S
import Json.Decode as JD
import Json.Encode as JE

{-| bytes represented as base64 -}
type AsBase64 = AsBase64 String

{-| json base64 string decoder -}
decAsBase64 : JD.Decoder AsBase64
decAsBase64 = JD.map AsBase64 JD.string

{-| json base64 string encoder -}
encAsBase64 : AsBase64 -> JE.Value
encAsBase64 (AsBase64 str) = JE.string str

{-| typed wire date primitive -}
type Date = Date String

{-| convert typed wire date to elm date -}
toStdDate : Date -> Maybe D.Date
toStdDate (Date dateStr) =
    case D.fromString dateStr of
        Err _ -> Nothing
        Ok val -> Just val

{-| convert elm date to typed wire date -}
fromStdDate : D.Date -> Date
fromStdDate d =
    let y = D.year d |> toString
        m = D.month d |> monthToInt |> toString |> S.padLeft 2 '0'
        day = D.day d |> toString |> S.padLeft 2 '0'
    in Date <| y ++ "-" ++ m ++ "-" ++ day

{-| convert elm month to int -}
monthToInt : D.Month -> Int
monthToInt m =
    case m of
        D.Jan -> 1
        D.Feb -> 2
        D.Mar -> 3
        D.Apr -> 4
        D.May -> 5
        D.Jun -> 6
        D.Jul -> 7
        D.Aug -> 8
        D.Sep -> 9
        D.Oct -> 10
        D.Nov -> 11
        D.Dec -> 12

{-| json date decoder -}
decDate : JD.Decoder Date
decDate = JD.map Date JD.string

{-| json date encoder -}
encDate : Date -> JE.Value
encDate (Date str) = JE.string str

{-| typed wire time primitive -}
type Time = Time String

{-| convert typed wire time to elm time -}
toStdTime : Time -> Maybe T.Time
toStdTime (Time t) =
    case List.map S.toInt <| S.split ":" t of
        [Ok h, Ok m, Ok s] ->
            Just (toFloat h * T.hour + toFloat m * T.minute + toFloat s * T.second)
        _ -> Nothing

{-| convert elm time to typed wire time -}
fromStdTime : T.Time -> Time
fromStdTime t =
    let secs = floor (T.inSeconds t) |> mk2Digit
        mins = floor (T.inMinutes t) |> mk2Digit
        hours = floor (T.inHours t) |> mk2Digit
    in Time (hours ++ ":" ++ mins ++ ":" ++ secs)

{-| json time decoder -}
decTime : JD.Decoder Time
decTime = JD.map Time JD.string

{-| json time encoder -}
encTime : Time -> JE.Value
encTime (Time str) = JE.string str

{-| typed wire datetime primitive -}
type DateTime = DateTime String

{-| convert typed wire datetime to elm date -}
toStdDateTime : DateTime -> Maybe D.Date
toStdDateTime (DateTime dateStr) =
    case D.fromString dateStr of
        Err _ -> Nothing
        Ok val -> Just val

{-| convert elm date to typed wire datetime -}
fromStdDateTime : D.Date -> Date
fromStdDateTime d =
    let y = D.year d |> toString
        m = D.month d |> monthToInt |> mk2Digit
        day = D.day d |> mk2Digit
        h = D.hour d |> mk2Digit
        min = D.minute d |> mk2Digit
        sec = D.second d |> mk2Digit
    in Date <| y ++ "-" ++ m ++ "-" ++ day ++ "T" ++ h ++ ":" ++ m ++ ":" ++ sec

{-| json datetime decoder -}
decDateTime : JD.Decoder DateTime
decDateTime = JD.map DateTime JD.string

{-| json datetime encoder -}
encDateTime : DateTime -> JE.Value
encDateTime (DateTime str) = JE.string str

{-| json encoder for maybes -}
encMaybe : (a -> JE.Value) -> Maybe a -> JE.Value
encMaybe f v =
    case v of
        Nothing -> JE.null
        Just a -> f a

mk2Digit : Int -> String
mk2Digit = toString >> S.padLeft 2 '0'
