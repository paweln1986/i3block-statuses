{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.Aeson          (omitNothingFields)
import qualified Data.Aeson          as Aeson
import           Data.Aeson.Casing   (aesonDrop, snakeCase)
import           Data.Text           (unpack)
import qualified Data.Text.Encoding  as TE
import           Control.Lens hiding (strict)
import           Lib
import           Relude
import           Turtle
import           Turtle.Prelude
import BlockDetails

data BatteryState
  = Discharging
  | Charging
  | Plugged
  | Unplugged
  | Unknown
  deriving (Show, Read)

main :: IO ()
main =
  sh $ do
    acpi <- acpi'
    let result = listToMaybe $ match parseBatteryStatus acpi
    case result of
      Just (status, percent) -> do
        let a =
              defaultBlockDetails & urgent ?~ isUrgent percent &
              fullText .~ (stateToIcon status <> show percent <> "% ") &
              background ?~ stateToBGColor status percent
        putLBSLn $ Aeson.encode a
      Nothing -> putTextLn "N/A"
  where
    isUrgent percent = percent <= 5

stateToBGColor Charging percent
  | percent >= 95 = "#3BFF11"
  | otherwise = "#0081FF"
stateToBGColor _ percent
  | percent >= 95 = "#3BFF11"
  | percent >= 65 = "#0081FF"
  | percent >= 35 = "#FFA900"
  | percent >= 15 = "#FF3900"

stateToIcon :: BatteryState -> Text
stateToIcon Plugged  = " \61926 "
stateToIcon Charging = " \61926 "
stateToIcon _        = " \61671 "

parseBatteryStatus :: Pattern (BatteryState, Int)
parseBatteryStatus = (,) <$> ("Battery" *> space *> decimal @Int *> ":" *> batteryState) <*> percent
  where
    batteryState :: Pattern BatteryState
    batteryState = parseState <$> (space *> chars1)
    parseState :: Text -> BatteryState
    parseState name = fromMaybe Unknown $ readMaybe (unpack name)

percent :: Pattern Int
percent = "," *> space *> decimal <* "%" <* chars1

acpi' :: Shell Text
acpi' = strict $ inshell "acpi" empty
