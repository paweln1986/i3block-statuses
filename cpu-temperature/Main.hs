
module Main where

import           BlockDetails
import qualified Data.Aeson          as Aeson
import           Data.Aeson.Lens
import           Control.Lens       hiding (strict)
import           Relude
import           Turtle hiding (view)
import qualified Data.Text as Text

fanIcon :: Text
fanIcon = "\63587"

degreeIcon :: Text
degreeIcon = "\176"

temperatureIcon :: Text
temperatureIcon = "\62154"

main :: IO ()
main =
  sh $ do
    a <- cpuStats'
    let fanDetails = a ^.. key "dell_smm-virtual-0" . _Object . traverse . _Object . traverse . _Integer
    let cpuDetails = a ^? key "coretemp-isa-0000" . key "Package id 0" . key "temp1_input" . _Integer
    let fan = Text.intercalate " " $ fmap (\val -> fanIcon <> " "<> show val) fanDetails
    let cpu = foldl' (\_ y -> temperatureIcon <> ""<>show y <> degreeIcon) (temperatureIcon <>"N/A") cpuDetails
    putLBSLn $ Aeson.encode $ defaultBlockDetails & fullText .~ (" " <> fan <> " " <> cpu <> " ")

cpuStats' :: Shell Text
cpuStats' = strict $ inshell "sensors -j" empty
