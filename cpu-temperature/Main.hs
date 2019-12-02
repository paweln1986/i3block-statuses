{-# LANGUAGE TypeApplications #-}

module Main where

import           BlockDetails
import           Control.Lens    hiding (strict)
import qualified Data.Aeson      as Aeson
import           Data.Aeson.Lens
import qualified Data.Text       as Text
import           Relude
import           Turtle          hiding (view)

fanIcon :: Text
fanIcon = "\63587"

degreeIcon :: Text
degreeIcon = "\176"

temperatureIcon :: Text
temperatureIcon = "\62154"

main :: IO ()
main =
  sh $ do
    sensorsResponse <- cpuStats'
    let fanDetails = sensorsResponse ^.. _Object . traverse . fanKeyNames . members . _Integer
    let cpuDetails = sensorsResponse ^? _Object . traverse . key "Package id 0" . key "temp1_input" . _Integer
    let fan = Text.intercalate " " $ fmap (\val -> fanIcon <> " " <> show val) fanDetails
    let cpu = foldl' (\_ y -> temperatureIcon <> "" <> show y <> degreeIcon) (temperatureIcon <> "N/A") cpuDetails
    putLBSLn $ Aeson.encode $ defaultBlockDetails & fullText .~ (" " <> fan <> " " <> cpu <> " ")
  where
    fanKeyNames = mconcat $ fmap (\val -> key $ "fan" <> show val) [0 :: Int .. 9]

cpuStats' :: Shell Text
cpuStats' = strict $ inshell "sensors -j" empty
