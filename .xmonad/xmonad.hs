{-# OPTIONS_GHC -Wall #-}
import XMonad
import qualified XMonad.StackSet as W

-- XMonadContrib
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Submap
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Prompt hiding (font)
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.EZConfig

import qualified Data.Map as M
import Data.Ratio ((%))
import Control.Monad (ap)

main :: IO ()
main = do
  (mst, scr) <- measure "-misc-fixed-medium-r-*-*-18-*-*-*-*-*-*-*"
  let tiled = Tall 1 (2%100) (mst % scr) -- Tall <nmaster> <delta> <ratio>
      customKeys = (additionalKeys `ap` addKeys) . (removeKeys `ap` delKeys)
  xmonad $ customKeys $ defaultConfig
       { workspaces = ["1:emacs", "2:ssh", "3:web", "4:qemu"]
       , layoutHook = avoidStruts (tiled ||| Mirror tiled ||| noBorders Full)
       , modMask    = mod4Mask
       }

measure :: String -> IO (Integer, Integer)
measure font = do
  dpy <- openDisplay ""
  fs  <- loadQueryFont dpy font
  let m = textWidth fs (take 83 $ repeat 'X')
      n = displayWidth dpy (defaultScreen dpy)
  m `seq` n `seq` closeDisplay dpy
  return (toInteger m, toInteger n)

type Key = (KeyMask, KeySym)

delKeys :: XConfig l -> [Key]
delKeys XConfig {modMask = modm} =
    [ (modm .|. shiftMask, xK_Return) -- > terminal
    , (modm,               xK_p)      -- > dmenu
    , (modm .|. shiftMask, xK_p)      -- > gmrun
    , (modm .|. shiftMask, xK_c)      -- > close the focused window
    , (modm,               xK_b)      -- > toggle status bar gap
    ]

addKeys :: XConfig l -> [(Key, X ())]
addKeys conf@(XConfig {modMask = modm}) =
    [ ((mod1Mask, xK_F2    ), spawn $ terminal conf) -- mod1-f2 %! Run a terminal emulator
    , ((modm,     xK_Delete), kill) -- %! Close the focused window
    , ((mod1Mask, xK_Down  ), spawn "amixer -q set Master 1-")   -- mod1-down %! Decrease audio volume
    , ((mod1Mask, xK_Up    ), spawn "amixer -q set Master 1+")   -- mod1-up   %! Increase audio volume
    , ((modm,     xK_z     ), spawn "mpc --no-status prev")   -- mod1-z %! MPD: Play the previous song in the current playlist
    , ((modm,     xK_c     ), spawn "mpc --no-status toggle") -- mod1-c %! MPD: Toggle play/pause, play if stopped
    , ((modm,     xK_v     ), spawn "mpc --no-status stop")   -- mod1-v %! MPD: Stop the currently playing playlists
    , ((modm,     xK_b     ), spawn "mpc --no-status next")   -- mod1-b %! MPD: Play the next song in the current playlist
    , ((modm,     xK_s     ), sendMessage ToggleStruts) -- mod1-s %! Toggle the status bar gap
    , ((modm .|. controlMask, xK_F1), spawn "mpc --no-status pause; xscreensaver-command -lock") -- %! Lock the screen
    , ((modm .|. controlMask, xK_F3), spawn "lsmod | grep -q psmouse && sudo rmmod psmouse || sudo modprobe psmouse") -- %! Toggle touchpad
      -- http://wiki.debian.org/SynapticsTouchpad
    ]
    ++
    [ ((mod1Mask, xK_F1), manPrompt   customXPConfig) -- mod1-f1 %! Query for manual page to be displayed
    , ((mod1Mask, xK_F3), shellPrompt customXPConfig) -- mod1-f3 %! Query for command line to execute
    , ((mod1Mask, xK_F4), sshPrompt   customXPConfig) -- mod1-f4 %! Query for host to connect to with SSH
    , ((mod1Mask, xK_F5), prompt "xterm -e dict" customXPConfig) -- XXX
    -- , ((mod1Mask, xK_F5), dictPrompt  customXPConfig) -- mod1-f5 %! Dictionary search (DICT)

    , ((modm, xK_0    ), toggleWS) -- mod-0 %! Toggle to the workspace displayed previously
    , ((modm, xK_quoteleft), toggleWS) -- mod-` %! Same as mod-0.
    , ((modm, xK_minus), prevWS  ) -- mod-- %! Switch to the previous workspace
    , ((modm, xK_equal), nextWS  ) -- mod-= %! Switch to the next workspace

    , (-- mod-a submap
       (modm, xK_a), submap . M.fromList $
       [ ((m, k), f)
         | m <- [0, modm]
         , (k, f) <- [ (xK_a, toggleWS) -- mod-a a, mod-a mod-a %! Toggle to the workspace displayed previously
                     , (xK_p, prevWS  ) -- mod-a p, mod-a mod-p %! Switch to the previous workspace
                     , (xK_n, nextWS  ) -- mod-a n, mod-a mod-n %! Switch to the next workspace
                     , (xK_Escape, return () ) -- mod-a esc, mod-a mod-a esc %! Do nothing (useful if `mod-a' was pressed by mistake)
                     ]
       ]
       ++
       [ ((0,         xK_apostrophe), selectWorkspace customXPConfig) -- mod-a ' %! Prompt for a workspace number/name to switch to
       , ((shiftMask, xK_a         ), renameWorkspace customXPConfig) -- mod-a A %! Allow the user to enter a name for the current workspace
       , ((0,         xK_BackSpace ), removeWorkspace) -- mod-a backspace %! Destroy current workspace
       ]
       ++
       [ ((m, k), withNthWorkspace f i)
         | (i, k) <- zip [0..] [xK_1..xK_9]
         , (f, m) <- [ (W.greedyView, 0   ) -- mod-a [1..9] %! Switch to workspace 0..9
                     , (W.greedyView, modm) -- mod-a mod-[1..9] %! Switch to workspace 0..9
                     , (W.shift, shiftMask) -- mod-a shift-[1..9] %! Move client to workspace 0..9
                     ]
       ]
      )
    ]

customXPConfig :: XPConfig
customXPConfig = defaultXPConfig { historyFilter = ignoredups }
  where
    ignoredups (x':x:xs) | x' == x = ignoredups (x:xs)
    ignoredups xs = xs
