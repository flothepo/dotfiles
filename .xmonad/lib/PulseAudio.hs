module PulseAudio
  ( VolumeChange(..)
  , toggleMute
  , changeVolume
  )
where
import           XMonad
import           Text.Printf

data VolumeChange = Up | Down

instance Show VolumeChange where
  show Up   = "+2%"
  show Down = "-2%"

defaultSink :: String
defaultSink = "@DEFAULT_SINK@"

volumeCmd :: String
volumeCmd = "set-sink-volume"

toggleMute :: MonadIO m => m ()
toggleMute = pactl "set-sink-mute" defaultSink "toggle"

changeVolume :: MonadIO m => VolumeChange -> m ()
changeVolume = pactl volumeCmd defaultSink . show

pactl :: MonadIO m => String -> String -> String -> m ()
pactl cmd sink value = spawn $ printf "pactl %s %s %s" cmd sink value
