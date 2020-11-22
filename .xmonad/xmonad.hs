{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           System.Exit                    ( exitSuccess )
import           System.Environment             ( getProgName )
import           XMonad
import qualified Data.Map                      as M
import           XMonad.Config.Desktop          ( desktopConfig )
import           XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet               as W

-- Gaps between windows
import           XMonad.Layout.Spacing
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders


import           XMonad.Actions.CycleWS

import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docks
                                                )
import           XMonad.Hooks.SetWMName
-- for compatibility with polybars xworkspaces
import           XMonad.Hooks.EwmhDesktops

import           XMonad.Hooks.ManageHelpers     ( doCenterFloat
                                                , doFullFloat
                                                )

import           XMonad.Util.SpawnOnce          ( spawnOnce )

-- control mpd
import qualified Network.MPD                   as MPD

-- Special KeySyms
import           Graphics.X11.ExtraTypes.XF86

-- utilities
import           Control.Monad                  ( void
                                                , when
                                                , join
                                                )
import           Text.Printf                    ( printf )
import           Data.Maybe                     ( maybeToList )


myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = "emacsclient -c"

myFileManager :: String
myFileManager = "nautilus"

myNormalBorderColor :: String
myNormalBorderColor = "#aaaaaa"


myFocusedBorderColor :: String
myFocusedBorderColor = "#afdedc"

myWorkspaces :: [String]
-- [cli, browser, mail, work] ++ numbered
myWorkspaces = ["\xf120", "\xe007", "\xf086", "\xf0b1", "\xf11b"]
  ++ map (show :: Int -> String) [6 .. 9]

--------------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { XMonad.modMask = modM }) =
  M.fromList
    $  [ ((modM, xK_Return)         , spawn myTerminal)
       , ((modM, xK_w)              , kill)
       , ((modM .|. shiftMask, xK_q), io exitSuccess)
       , ( (modM, xK_q)
         , restartXmonad
         )
       -- powermenu
       , ( (modM, xK_Escape)
         , spawn "rofi -show powermenu -modi powermenu:rofi-powermenu"
         )
       -- C-M-l to send system to sleep
       , ( (controlMask .|. mod1Mask, xK_l)
         , spawn "systemctl suspend"
         )
       -- Utilities
       , ((modM, xK_b), spawn myBrowser)
       , ((modM, xK_e), spawn myEditor)
       , ( (modM, xK_t)
         , spawn myFileManager
         )
       --rofi
       , ((modM, xK_space), spawn "rofi -show drun")
       , ( (modM .|. shiftMask, xK_e)
         , spawn "rofi -show emoji -modi emoji:rofimoji"
         )
       -- file manager
       , ( (modM .|. shiftMask, xK_f)
         , spawn myFileManager
         )
       --Screenshots
       , ( (noModMask, xK_Print)
         , spawn "screenshot"
         )
       -- Window navigation
       , ((modM, xK_j), windows W.focusDown)
       , ( (modM, xK_k)
         , windows W.focusUp
         )
       -- Audio
       , ((noModMask, xF86XK_AudioLowerVolume), changeVolume Down)
       , ((noModMask, xF86XK_AudioRaiseVolume), changeVolume Up)
       , ( (noModMask, xF86XK_AudioMute)
         , toggleMute
         )
       -- mpd
       , ((noModMask, xF86XK_AudioPlay), io $ void $ MPD.withMPD mpdToggle)
       , ((noModMask, xF86XK_AudioPrev), io $ void $ MPD.withMPD MPD.previous)
       , ( (noModMask, xF86XK_AudioNext)
         , io $ void $ MPD.withMPD MPD.next
         )
       --move Windows between workspaces
       , ( (modM .|. shiftMask, xK_Right)
         , shiftTo Next AnyWS >> moveTo Next AnyWS
         )
       , ( (modM .|. shiftMask, xK_Left)
         , shiftTo Prev AnyWS >> moveTo Prev AnyWS
         )
       -- Switch to previously displayed workspace
       , ( (modM, xK_Tab)
         , toggleWS
         )
       -- Push Window into tiled mode
       , ( (modM, xK_t)
         , withFocused $ windows . W.sink
         )
       --  Toggle Fullscreening of window
       , ( (modM, xK_f)
         , sendMessage ToggleLayout
         )
       --Resize the master window
       , ((modM, xK_h), sendMessage Shrink)
       , ((modM, xK_l), sendMessage Expand)
       ]
    ++
    -- modm + workspace number commands
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
        [ ((m .|. modM, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

--------------------------------------------------------------------------------
myMouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modM } = M.fromList
  --Set the window to floating mode and move by dragging
  [ ( (modM, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )

    --Raise the window to the top of the stack
  , ( (modM, button2)
    , \w -> focus w >> windows W.shiftMaster
    )

    --Set the window to floating mode and resize by dragging
  , ( (modM, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )
  ]
--------------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook =
  mapM_ spawnOnce autostarts >> setWMName "compiz" >> addEWMHFullscreen
 where
  autostarts =
    [ "launch-polybar"          -- launch polybar on all monitors
    , "nitrogen --restore"      -- reload wallpaper
    , "picom"                   -- compositor
    ]

--------------------------------------------------------------------------------

myLayoutHook = avoidStruts $ toggleLayouts full tall
 where
  tall = gaps $ Tall 1 (3 / 100) (1 / 2)
  full = noBorders $ fullscreenFull Full
  gaps = spacingRaw True (Border 0 0 0 0) True (Border 4 4 4 4) True

--------------------------------------------------------------------------------
myManageHook = composeAll
  [ className =? "KeePassXC" --> doFloat
  , className =? "firefox" <&&> appName =? "Toolkit" --> doFloat
  , className =? "matplotlib" --> doFloat
  , title =? "Microsoft Teams Notification" --> doFloat
  , className =? "Org.gnome.Nautilus" --> doCenterFloat
  , className =? "Gcr-prompter" <&&> title =? "Unlock Keyring" --> doCenterFloat
  , className =? "Pavucontrol" --> doCenterFloat
  , className =? "Evolution-alarm-notify" --> doFloat
  , className =? "kdeconnect.daemon" --> doFullFloat
  ]
--------------------------------------------------------------------------------

myConfig = desktopConfig { modMask            = myModMask
                         , layoutHook         = myLayoutHook
                         , terminal           = myTerminal
                         , borderWidth        = 2
                         , normalBorderColor  = myNormalBorderColor
                         , focusedBorderColor = myFocusedBorderColor
                         , startupHook        = myStartupHook
                         , logHook            = ewmhDesktopsLogHook
                         , manageHook         = myManageHook
                         , keys               = myKeys
                         , workspaces         = myWorkspaces
                         , mouseBindings      = myMouseBindings
                         }

configModifiers :: [XConfig a -> XConfig a]
configModifiers = [ewmh, docks]
--------------------------------------------------------------------------------


main :: IO ()
main = do
  xmonad $ fullscreenSupport $ foldl1 (.) configModifiers myConfig

-- UTILITIES
--------------------------------------------------------------------------------
restartXmonad :: X ()
restartXmonad = do
  status <- recompile True
  let statusMessage = if status then "successful" else "failed"
  notify $ "Recompilation " ++ statusMessage
  whoami <- liftIO getProgName
  when status $ restart whoami True

-- mpd

-- Toggle play/pause
mpdToggle :: MPD.MonadMPD m => m ()
mpdToggle = do
  s <- MPD.stState <$> MPD.status
  MPD.pause $ b s
 where
  b MPD.Playing = True
  b _           = False


-- Notifications
notify' :: MonadIO a => String -> String -> String -> a ()
notify' nTitle icon text =
  spawn $ printf "notify-send -t 1000 '%s' '%s' --icon=%s" nTitle text icon

notify :: MonadIO a => String -> a ()
notify = notify' "xmonad" "xmonad"


-- Fullscreen when Application requests it (firefox video)
addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r               <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a               <- getAtom "ATOM"
  liftIO $ do
    sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $ changeProperty32 dpy
                                                           r
                                                           a_NET_SUPPORTED
                                                           a
                                                           propModeAppend
                                                           [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]

-- Pulseaudio
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

