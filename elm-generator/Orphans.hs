{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans
where

import           RIO
import qualified RIO.Time as Time (Day(..), TimeOfDay(..))
import qualified RIO.Vector.Boxed as VB (Vector)

import qualified Data.Aeson as A
import qualified Generics.SOP as SOP
import qualified Language.Elm.Type as T (apps)
import qualified Language.Elm.Expression as E (apps)
import           Language.Haskell.To.Elm
    ( HasElmEncoder(..)
    , HasElmDecoder(..)
    , HasElmType(..)
    , Options(..)
    , defaultOptions
    , deriveElmJSONEncoder
    , deriveElmJSONDecoder
    , deriveElmTypeDefinition
    )

import           App.Api (RenameArgs)
import           App.WorkOption
    ( WorkOption
    , SetArrived
    , SetLeft
    , SetNotes
    , SetOffice
    , SetProj
    )
import           Db.DayF (DayWithHalfDays)
import           Db.HalfDay (HalfDay)
import           Db.Idle (Idle)
import           Db.IdleDayType (IdleDayType)
import           Db.Month (Month)
import           Db.MonthF (MonthWithDays)
import           Db.Notes (Notes)
import           Db.Office (Office)
import           Db.Project (Project)
import           Db.TimeInDay (TimeInDay)
import           Db.Worked (Worked)

--

instance SOP.Generic TimeInDay
instance SOP.HasDatatypeInfo TimeInDay

instance HasElmType TimeInDay where
    elmDefinition =
        Just $ deriveElmTypeDefinition @TimeInDay defaultOptions "Api.TimeInDay"

instance HasElmDecoder A.Value TimeInDay where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @TimeInDay defaultOptions A.defaultOptions "Api.TimeInDay.decoder"

instance HasElmEncoder A.Value TimeInDay where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @TimeInDay defaultOptions A.defaultOptions "Api.TimeInDay.encoder"

--

instance SOP.Generic IdleDayType
instance SOP.HasDatatypeInfo IdleDayType

instance HasElmType IdleDayType where
    elmDefinition =
        Just $ deriveElmTypeDefinition @IdleDayType defaultOptions "Api.IdleDayType"

instance HasElmDecoder A.Value IdleDayType where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @IdleDayType defaultOptions A.defaultOptions "Api.IdleDayType.decoder"

instance HasElmEncoder A.Value IdleDayType where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @IdleDayType defaultOptions A.defaultOptions "Api.IdleDayType.encoder"

--

instance SOP.Generic Idle
instance SOP.HasDatatypeInfo Idle

instance HasElmType Idle where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Idle removeUnderscoreOptions "Api.Idle"

instance HasElmDecoder A.Value Idle where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Idle removeUnderscoreOptions A.defaultOptions "Api.Idle.decoder"

instance HasElmEncoder A.Value Idle where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Idle removeUnderscoreOptions A.defaultOptions "Api.Idle.encoder"

--

instance SOP.Generic Office
instance SOP.HasDatatypeInfo Office

instance HasElmType Office where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Office defaultOptions "Api.Office"

instance HasElmDecoder A.Value Office where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Office defaultOptions A.defaultOptions "Api.Office.decoder"

instance HasElmEncoder A.Value Office where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Office defaultOptions A.defaultOptions "Api.Office.encoder"

--

instance SOP.Generic Notes
instance SOP.HasDatatypeInfo Notes

instance HasElmType Notes where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Notes defaultOptions "Api.Notes"

instance HasElmDecoder A.Value Notes where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Notes defaultOptions A.defaultOptions "Api.Notes.decoder"

instance HasElmEncoder A.Value Notes where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Notes defaultOptions A.defaultOptions "Api.Notes.encoder"

--

instance SOP.Generic Project
instance SOP.HasDatatypeInfo Project

instance HasElmType Project where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Project defaultOptions "Api.Project"

instance HasElmDecoder A.Value Project where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Project defaultOptions A.defaultOptions "Api.Project.decoder"

instance HasElmEncoder A.Value Project where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Project defaultOptions A.defaultOptions "Api.Project.encoder"

--

instance SOP.Generic Worked
instance SOP.HasDatatypeInfo Worked

instance HasElmType Worked where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Worked removeUnderscoreOptions "Api.Worked"

instance HasElmDecoder A.Value Worked where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Worked removeUnderscoreOptions A.defaultOptions "Api.Worked.decoder"

instance HasElmEncoder A.Value Worked where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Worked removeUnderscoreOptions A.defaultOptions "Api.Worked.encoder"

--

instance SOP.Generic HalfDay
instance SOP.HasDatatypeInfo HalfDay

instance HasElmType HalfDay where
    elmDefinition =
        Just $ deriveElmTypeDefinition @HalfDay defaultOptions "Api.HalfDay"

instance HasElmDecoder A.Value HalfDay where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @HalfDay defaultOptions A.defaultOptions "Api.HalfDay.decoder"

instance HasElmEncoder A.Value HalfDay where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @HalfDay defaultOptions A.defaultOptions "Api.HalfDay.encoder"

--

instance SOP.Generic WorkOption
instance SOP.HasDatatypeInfo WorkOption

instance HasElmType WorkOption where
    elmDefinition =
        Just $ deriveElmTypeDefinition @WorkOption defaultOptions "Api.WorkOption"

instance HasElmDecoder A.Value WorkOption where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @WorkOption defaultOptions A.defaultOptions "Api.WorkOption.decoder"

instance HasElmEncoder A.Value WorkOption where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @WorkOption defaultOptions A.defaultOptions "Api.WorkOption.encoder"

--

instance SOP.Generic SetArrived
instance SOP.HasDatatypeInfo SetArrived

instance HasElmType SetArrived where
    elmDefinition =
        Just $ deriveElmTypeDefinition @SetArrived defaultOptions "Api.SetArrived"

instance HasElmDecoder A.Value SetArrived where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @SetArrived defaultOptions A.defaultOptions "Api.SetArrived.decoder"

instance HasElmEncoder A.Value SetArrived where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @SetArrived defaultOptions A.defaultOptions "Api.SetArrived.encoder"

--

instance SOP.Generic SetLeft
instance SOP.HasDatatypeInfo SetLeft

instance HasElmType SetLeft where
    elmDefinition =
        Just $ deriveElmTypeDefinition @SetLeft defaultOptions "Api.SetLeft"

instance HasElmDecoder A.Value SetLeft where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @SetLeft defaultOptions A.defaultOptions "Api.SetLeft.decoder"

instance HasElmEncoder A.Value SetLeft where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @SetLeft defaultOptions A.defaultOptions "Api.SetLeft.encoder"

--

instance SOP.Generic SetNotes
instance SOP.HasDatatypeInfo SetNotes

instance HasElmType SetNotes where
    elmDefinition =
        Just $ deriveElmTypeDefinition @SetNotes defaultOptions "Api.SetNotes"

instance HasElmDecoder A.Value SetNotes where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @SetNotes defaultOptions A.defaultOptions "Api.SetNotes.decoder"

instance HasElmEncoder A.Value SetNotes where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @SetNotes defaultOptions A.defaultOptions "Api.SetNotes.encoder"

--

instance SOP.Generic SetOffice
instance SOP.HasDatatypeInfo SetOffice

instance HasElmType SetOffice where
    elmDefinition =
        Just $ deriveElmTypeDefinition @SetOffice defaultOptions "Api.SetOffice"

instance HasElmDecoder A.Value SetOffice where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @SetOffice defaultOptions A.defaultOptions "Api.SetOffice.decoder"

instance HasElmEncoder A.Value SetOffice where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @SetOffice defaultOptions A.defaultOptions "Api.SetOffice.encoder"

--

instance SOP.Generic SetProj
instance SOP.HasDatatypeInfo SetProj

instance HasElmType SetProj where
    elmDefinition =
        Just $ deriveElmTypeDefinition @SetProj defaultOptions "Api.SetProj"

instance HasElmDecoder A.Value SetProj where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @SetProj defaultOptions A.defaultOptions "Api.SetProj.decoder"

instance HasElmEncoder A.Value SetProj where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @SetProj defaultOptions A.defaultOptions "Api.SetProj.encoder"

--

instance SOP.Generic RenameArgs
instance SOP.HasDatatypeInfo RenameArgs

instance HasElmType RenameArgs where
    elmDefinition =
        Just $ deriveElmTypeDefinition @RenameArgs defaultOptions "Api.RenameArgs"

instance HasElmDecoder A.Value RenameArgs where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @RenameArgs defaultOptions A.defaultOptions "Api.RenameArgs.decoder"

instance HasElmEncoder A.Value RenameArgs where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @RenameArgs defaultOptions A.defaultOptions "Api.RenameArgs.encoder"

--

instance HasElmType VB.Vector where
    elmType = "Array.Array"

instance HasElmEncoder A.Value VB.Vector where
  elmEncoder = "Json.Encode.array"

instance HasElmDecoder A.Value VB.Vector where
  elmDecoder = "Json.Decode.array"

instance (HasElmType a) => HasElmType (VB.Vector a) where
  elmType =
    T.apps (elmType @Vector) [elmType @a]

instance (HasElmDecoder A.Value a) => HasElmDecoder A.Value (VB.Vector a) where
  elmDecoder =
    E.apps (elmDecoder @A.Value @VB.Vector) [elmDecoder @A.Value @a]

instance (HasElmEncoder A.Value a) => HasElmEncoder A.Value (VB.Vector a) where
  elmEncoder =
    E.apps (elmEncoder @A.Value @VB.Vector) [elmEncoder @A.Value @a]

--

instance SOP.Generic MonthWithDays
instance SOP.HasDatatypeInfo MonthWithDays

instance HasElmType MonthWithDays where
    elmDefinition =
        Just $ deriveElmTypeDefinition @MonthWithDays removeUnderscoreOptions "Api.MonthWithDays"

instance HasElmDecoder A.Value MonthWithDays where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @MonthWithDays removeUnderscoreOptions A.defaultOptions "Api.MonthWithDays.decoder"

instance HasElmEncoder A.Value MonthWithDays where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @MonthWithDays removeUnderscoreOptions A.defaultOptions "Api.MonthWithDays.encoder"

--

instance SOP.Generic DayWithHalfDays
instance SOP.HasDatatypeInfo DayWithHalfDays

instance HasElmType DayWithHalfDays where
    elmDefinition =
        Just $ deriveElmTypeDefinition @DayWithHalfDays removeUnderscoreOptions "Api.DayWithHalfDays"

instance HasElmDecoder A.Value DayWithHalfDays where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @DayWithHalfDays removeUnderscoreOptions A.defaultOptions "Api.DayWithHalfDays.decoder"

instance HasElmEncoder A.Value DayWithHalfDays where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @DayWithHalfDays removeUnderscoreOptions A.defaultOptions "Api.DayWithHalfDays.encoder"

--

instance SOP.Generic Month
instance SOP.HasDatatypeInfo Month

instance HasElmType Month where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Month removeUnderscoreOptions "Api.Month"

instance HasElmDecoder A.Value Month where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Month removeUnderscoreOptions A.defaultOptions "Api.Month.decoder"

instance HasElmEncoder A.Value Month where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Month removeUnderscoreOptions A.defaultOptions "Api.Month.encoder"

--

instance HasElmType Integer where
  elmType = "Basics.Int"

instance HasElmEncoder A.Value Integer where
  elmEncoder = "Json.Encode.int"

instance HasElmDecoder A.Value Integer where
  elmDecoder = "Json.Decode.int"

--

deriving instance Generic Time.Day
instance SOP.Generic Time.Day
instance SOP.HasDatatypeInfo Time.Day

instance HasElmType Time.Day where
    elmType = "Date.Date"

instance HasElmEncoder A.Value Time.Day where
  elmEncoder = "Date.Extended.encode"

instance HasElmDecoder A.Value Time.Day where
  elmDecoder = "Date.Extended.decoder"

--

deriving instance Generic Time.TimeOfDay
instance SOP.Generic Time.TimeOfDay
instance SOP.HasDatatypeInfo Time.TimeOfDay

instance HasElmType Time.TimeOfDay where
    elmType = "Api.TimeOfDay.TimeOfDay"

instance HasElmEncoder A.Value Time.TimeOfDay where
  elmEncoder = "Api.TimeOfDay.encode"

instance HasElmDecoder A.Value Time.TimeOfDay where
  elmDecoder = "Api.TimeOfDay.decoder"

--

-- | Remove the first underscore of a string
removeUnderscore :: String -> String
removeUnderscore ('_':xs) = xs
removeUnderscore x = x

-- | Options to rewrite the fields name. It seems that Elm doesn't accept
-- identifiers starting with underscore
removeUnderscoreOptions :: Options
removeUnderscoreOptions = Options removeUnderscore
