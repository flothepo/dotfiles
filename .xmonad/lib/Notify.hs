module Notify where
import           XMonad
import           Text.Printf

notify' :: MonadIO a => String -> String -> String -> a ()
notify' nTitle icon text =
  spawn $ printf "notify-send -t 1000 '%s' '%s' --icon=%s" nTitle text icon

notify :: MonadIO a => String -> a ()
notify = notify' "xmonad" "xmonad"
