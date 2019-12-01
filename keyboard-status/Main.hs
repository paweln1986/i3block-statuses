module Main where

import           BlockDetails
import           Control.Lens hiding (strict)
import qualified Data.Aeson   as Aeson
import qualified Data.Text    as T
import           Relude       hiding (state, stdout)
import           Turtle

cpuIcon :: Text
cpuIcon = "\61587"

main :: IO ()
main =
  sh $ do
    aa <- grep (Turtle.contains "Caps") cpuStats1'
    let keyState = cut spaces1 (T.strip $ lineToText aa)
    let caps = keyState ^? ix 3
    let capsState = value $ keyState ^? ix 3
    let c =
          defaultBlockDetails & fullText .~ capsState & background ?~ toBgColor caps &
          minWidth ?~ ("  " <> cpuIcon <> " off" <> " ")
    putLBSLn $ Aeson.encode c

value :: Maybe Text -> Text
value (Just "on")  = cpuIcon <> " on "
value (Just "off") = cpuIcon <> " off"
value _            = cpuIcon <> "N/A"

toBgColor :: Maybe Text -> Text
toBgColor (Just "on") = "#FFB200"
toBgColor _           = "#000000"

cpuStats1' :: Shell Line
cpuStats1' = inshell "xset -q" empty
