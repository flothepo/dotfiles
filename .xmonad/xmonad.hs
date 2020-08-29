module Main
  ( main
  )
where

--------------------------------------------------------------------------------
import           System.Exit
import           XMonad
import qualified Data.Map                      as M
import           XMonad.Config.Desktop
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet               as W

-- Gaps between windows
import           XMonad.Layout.Spacing
import           XMonad.Layout.Fullscreen

import           XMonad.Actions.CycleWS

import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
-- for compatibility with polybars xworkspaces
import           XMonad.Hooks.EwmhDesktops

import           XMonad.Util.SpawnOnce

-- control mpd
import qualified Network.MPD                   as MPD

-- Special KeySyms
import           Graphics.X11.ExtraTypes.XF86

-- utilities
import           Text.Printf
import           Control.Monad


myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = "emacsclient -c"

myFileManager :: String
myFileManager = "thunar"

myNormalBorderColor :: String
myNormalBorderColor = "#aaaaaa"


myFocusedBorderColor :: String
myFocusedBorderColor = "#afdedc"

myWorkspaces :: [String]
myWorkspaces = ["\xf120", "\xe007", "\xf0e0", "\xf8d9"]
  ++ map (show :: Int -> String) [5 .. 9]

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
         , xmonadNotify "Recompiled"
           >> spawn "pkill xmobar;xmonad --recompile && xmonad --restart"
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
       , ((noModMask, xF86XK_AudioLowerVolume), myAudioControl "down")
       , ((noModMask, xF86XK_AudioRaiseVolume), myAudioControl "up")
       , ( (noModMask, xF86XK_AudioMute)
         , myAudioControl "togmute"
         )
       -- mpd
       , ((noModMask, xF86XK_AudioPlay), io $ void $ MPD.withMPD $ mpdToggle)
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
myMouseBindings (XConfig { XMonad.modMask = modM }) = M.fromList
  --Set the window to floating mode and move by dragging
  [ ( (modM, button1)
    , (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    )

    --Raise the window to the top of the stack
  , ( (modM, button2)
    , (\w -> focus w >> windows W.shiftMaster)
    )

    --Set the window to floating mode and resize by dragging
  , ( (modM, button3)
    , (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    )
  ]
--------------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook =
  spawnOnce "nitrogen --restore"
    >> spawnOnce "launch_polybar"
    >> spawnOnce "xfce4-power-manager"
    >> spawnOnce "pgrep nextcloud || nextcloud"
    >> setWMName "compiz"

--------------------------------------------------------------------------------

myLayoutHook =
  avoidStruts
    $ spacingRaw True (Border 0 0 0 0) True (Border 4 4 4 4) True
    $ toggleLayouts Full
    $ layoutHook def

--------------------------------------------------------------------------------
myManageHook = composeAll
  [ className =? "KeePassXC" --> doFloat
  , className =? "firefox" <&&> resource =? "Toolkit" --> doFloat
  , className =? "matplotlib" --> doFloat
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
                         }

configModifiers :: [(XConfig a -> XConfig a)]
configModifiers = [ewmh, docks]
--------------------------------------------------------------------------------


main :: IO ()
main = do
  xmonad $ fullscreenSupport $ (foldl1 (.) configModifiers) $ myConfig

-- UTILITIES
--------------------------------------------------------------------------------
-- pulseaudio
myAudioControl :: String -> X ()
myAudioControl =
  spawn . ("~/.config/polybar/scripts/pulseaudio-control.sh " ++)

-- Notifications
notify :: MonadIO a => String -> String -> String -> a ()
notify title icon text =
  spawn $ printf "notify-send '%s' '%s' --icon=%s" title text icon

xmonadNotify :: MonadIO a => String -> a ()
xmonadNotify = notify "xmonad" "xmonad"

-- mpd

-- Toggle play/pause
mpdToggle :: MPD.MonadMPD m => m ()
mpdToggle = do
  s <- MPD.stState <$> MPD.status
  MPD.pause $ b s
 where
  b MPD.Playing = True
  b _           = False
