{-# OPTIONS_GHC -Wall #-}
import XMonad
import XMonad.Layout
import XMonad.Operations
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib

-- XMonadContrib
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Submap
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.CustomKeys

import Data.Bits ((.|.))
import qualified Data.Map as M
import Data.Ratio ((%))

main :: IO ()
main = xmonad defaultConfig
       { workspaces         = ["1:term", "2:emacs", "3:web"]
       , layoutHook = avoidStruts (tiled ||| Mirror tiled ||| noBorders Full)
       , focusedBorderColor = "#00ff00"
       , modMask            = mod4Mask
       , keys               = customKeys delKeys insKeys
       }
    where
      tiled = Tall 1 (2%100) (748%1440) -- Tall <nmaster> <delta> <ratio>

delKeys :: XConfig l -> [(KeyMask, KeySym)]
delKeys XConfig {modMask = modm} =
    [ (modm .|. shiftMask, xK_Return) -- > terminal
    , (modm,               xK_p)      -- > dmenu
    , (modm .|. shiftMask, xK_p)      -- > gmrun
    , (modm .|. shiftMask, xK_c)      -- > close the focused window
    , (modm,               xK_b)      -- > toggle status bar gap
    ]

insKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
insKeys conf@(XConfig {modMask = modm}) =
    [ ((mod1Mask, xK_F2    ), spawn $ terminal conf) -- mod1-f2 %! Run a terminal emulator
    , ((modm,     xK_Delete), kill) -- %! Close the focused window
    , ((mod1Mask, xK_Down  ), spawn "amixer -q set Master 1-")   -- mod1-down %! Decrease audio volume
    , ((mod1Mask, xK_Up    ), spawn "amixer -q set Master 1+")   -- mod1-up   %! Increase audio volume
    , ((modm,     xK_z     ), spawn "mpc --no-status prev")   -- mod1-z %! MPD: Play the previous song in the current playlist
    , ((modm,     xK_c     ), spawn "mpc --no-status toggle") -- mod1-c %! MPD: Toggle play/pause, play if stopped
    , ((modm,     xK_v     ), spawn "mpc --no-status stop")   -- mod1-v %! MPD: Stop the currently playing playlists
    , ((modm,     xK_b     ), spawn "mpc --no-status next")   -- mod1-b %! MPD: Play the next song in the current playlist
    , ((modm,     xK_s     ), sendMessage ToggleStruts) -- mod1-s %! Toggle the status bar gap
    , ((modm .|. controlMask, xK_F1), spawn "xscreensaver-command -lock") -- %! Lock the screen
    , ((modm .|. controlMask, xK_F3), spawn "lsmod | grep -q psmouse && sudo rmmod psmouse || sudo modprobe psmouse") -- %! Toggle touchpad
    ]
    ++
    [ ((mod1Mask, xK_F1), manPrompt   defaultXPConfig) -- mod1-f1 %! Query for manual page to be displayed
    , ((mod1Mask, xK_F3), shellPrompt defaultXPConfig) -- mod1-f3 %! Query for command line to execute
    , ((mod1Mask, xK_F4), sshPrompt   defaultXPConfig) -- mod1-f4 %! Query for host to connect to with SSH

    , ((modm, xK_0    ), toggleWS) -- mod-0 %! Toggle to the workspace displayed previously
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
       [ ((0,         xK_apostrophe), selectWorkspace defaultXPConfig) -- mod-a ' %! Prompt for a workspace number/name to switch to
       , ((shiftMask, xK_a         ), renameWorkspace defaultXPConfig) -- mod-a A %! Allow the user to enter a name for the current workspace
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
