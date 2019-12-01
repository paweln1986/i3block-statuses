module Main where

import           BlockDetails
import           Control.Lens    hiding (strict)
import qualified Data.Aeson      as Aeson
import           Data.Aeson.Lens
import           Relude          hiding (state)
import           Turtle
import System.Environment (getArgs)

cpuIcon :: Text
cpuIcon = "\62171"

main :: IO ()
main =
  sh $ do
    args <- liftIO getArgs
    let aa = listToMaybe args
    case aa of
      Just "1" -> void $ Turtle.shell "i3-msg -q -- exec gnome-system-monitor -r &> /dev/null" empty
      _ -> return ()     
    cpuStats <- cpuStats'
    let cpuIdleValue =
          cpuStats ^? key "sysstat" . key "hosts" . _Array . traverse . key "statistics" . _Array . traverse .
          key "node-load" .
          _Array .
          traverse .
          key "idle" .
          _Integer
    let cpu = 100 - fromMaybe 100 cpuIdleValue
    let cpuBox = " " <> cpuIcon <> " " <> show cpu <> "% "
    let c =
          defaultBlockDetails & fullText .~ cpuBox & minWidth ?~ (" " <> cpuIcon <> "100%  ") & background ?~
          toBgColor cpu
    putLBSLn $ Aeson.encode c

toBgColor :: Integer -> Text
toBgColor percent
  | percent >= 90 = "#FF0005"
  | percent >= 60 = "#FF8200"
  | percent >= 40 = "#FFE400"
  | otherwise = "#000000"

--xset -q | grep Caps | awk '{ print $2, $3, $4 }'
cpuStats' :: Shell Text
cpuStats' = strict $ inshell "mpstat 1 1 -n -o JSON" empty
