{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans
where

import           RIO
import qualified RIO.Time as Time (Day(..), TimeOfDay(..))

import qualified Data.Aeson as A
import qualified Generics.SOP as SOP
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

import           Db.HalfDay (HalfDay)
import           Db.Idle (Idle)
import           Db.IdleDayType (IdleDayType)
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
        Just $ deriveElmJSONDecoder @Idle defaultOptions A.defaultOptions "Api.Idle.decoder"

instance HasElmEncoder A.Value Idle where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Idle defaultOptions A.defaultOptions "Api.Idle.encoder"

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
        Just $ deriveElmJSONDecoder @Worked defaultOptions A.defaultOptions "Api.Worked.decoder"

instance HasElmEncoder A.Value Worked where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Worked defaultOptions A.defaultOptions "Api.Worked.encoder"

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

deriving instance Generic Time.Day
instance SOP.Generic Time.Day
instance SOP.HasDatatypeInfo Time.Day

instance HasElmType Time.Day where
    elmType = "Date.Date"

instance HasElmEncoder A.Value Time.Day where
  elmEncoder = "Date.Extra.encode"

instance HasElmDecoder A.Value Time.Day where
  elmDecoder = "Date.Extra.decoder"

--

deriving instance Generic Time.TimeOfDay
instance SOP.Generic Time.TimeOfDay
instance SOP.HasDatatypeInfo Time.TimeOfDay

instance HasElmType Time.TimeOfDay where
    elmType = "TimeOfDay.TimeOfDay"

instance HasElmEncoder A.Value Time.TimeOfDay where
  elmEncoder = "Date.Extra.encode"

instance HasElmDecoder A.Value Time.TimeOfDay where
  elmDecoder = "Date.Extra.decoder"

--

-- | Remove the first underscore of a string
removeUnderscore :: String -> String
removeUnderscore ('_':xs) = xs
removeUnderscore x = x

-- | Options to rewrite the fields name. It seems that Elm doesn't accept
-- identifiers starting with underscore
removeUnderscoreOptions :: Options
removeUnderscoreOptions = Options removeUnderscore
