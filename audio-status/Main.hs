module Main where

import           BlockDetails
import           Control.Lens hiding (strict)
import qualified Data.Aeson   as Aeson
import qualified Data.Text    as T
import           Relude       hiding (state, stdout)
import           Turtle
import System.Environment (getArgs)

cpuIcon :: Text
cpuIcon = "\61598"

cpuIcon1 :: Text
cpuIcon1 = "\63145"

main :: IO ()
main =
  sh $ do
    args <- liftIO getArgs
    let aa = listToMaybe args
    case aa of
      Just "1" -> void $ Turtle.shell "i3-msg -q -- exec pavucontrol &> /dev/null" empty
      _ -> return ()     
    leftSpeaker <- extractSpeakerDetails "Front Left:"
    rightSpeaker <- extractSpeakerDetails "Front Right:"
    let leftSpeakerPercent = maybe "0%" (T.dropAround (\val -> val == '[' || val == ']')) (leftSpeaker ^? ix 4)
    let rightSpeakerPercent = maybe "0%" (T.dropAround (\val -> val == '[' || val == ']')) (rightSpeaker ^? ix 4)
    let c =
          defaultBlockDetails & fullText .~ " " <> value leftSpeakerPercent rightSpeakerPercent <> " " &
          background ?~ toBgColor leftSpeakerPercent rightSpeakerPercent &
          minWidth ?~ ("  " <> cpuIcon1 <> " 100%" <> " 100% ")
    putLBSLn $ Aeson.encode c
  where
    cutLine = cut spaces1 . T.strip . lineToText
    extractSpeakerDetails name = cutLine <$> grep (Turtle.contains name) cpuStats1'

value :: Text -> Text -> Text
value "0%" "0%" = cpuIcon1 <> " 0%" <> " 0%"
value lp rp     = cpuIcon <> " " <> lp <> " " <> rp

toBgColor :: Text -> Text -> Text
toBgColor "0%" "0%" = "#FFB200"
toBgColor _ _       = "#000000"

cpuStats1' :: Shell Line
cpuStats1' = inshell "amixer get Master" empty
