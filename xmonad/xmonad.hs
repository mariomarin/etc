-- xmonad.hs - xmonad config file
-- Ethan Schoonover <es@ethanschoonover.com>
--
-- Current version always available at:
-- https://github.com/altercation/es-etc/blob/master/xmonad/xmonad.hs

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

-- Default imports -----------------------------------------------------

import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Taffybar related ----------------------------------------------------

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import DBus.Client.Simple
import System.Taffybar.XMonadLog ( dbusLog )

-- Handle fullscreen events --------------------------------------------

-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages and require
-- X.L.Fullscreen, so we import both and reference X.H.EwmhDesktops as E
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Layout.Fullscreen
import XMonad.Hooks.ManageHelpers

-- UI/UX ---------------------------------------------------------------

import XMonad.Layout.NoBorders
import XMonad.Layout.Decoration
import XMonad.Layout.Named
import XMonad.Layout.Simplest
import XMonad.Layout.TabBarDecoration

-- Application/System --------------------------------------------------

import XMonad.Actions.WithAll -- used for killall

-- Key Bindings --------------------------------------------------------

-- this method of applying X.U.EZConfig from loupgaroublonds config
import qualified XMonad.Util.EZConfig as EZ
import qualified Data.Map as M

-- Navigation ----------------------------------------------------------
import XMonad.Actions.CycleWS

------------------------------------------------------------------------
-- Applications and Utilities
------------------------------------------------------------------------

myTerminal      = "urxvtc"
myBrowser       = "chromium" --firefox, uzbl-browser

--myStartup                       :: X ()
--myStartup                       = spawn 

myRestart			:: X ()
myRestart			= spawn 
				$  myRestartKill "xmobar"
				++ myRestartKill "taffybar"
				++ myRestartKill "conky"
				++ myRestartKill "dzen2"
				++ myRestartKill "stalonetray"
				++ myRestartKill "trayer"
				++ myRestartKill "xcompmgr"
				++ myRestartKill "cairo-compmgr"
				++ myRestartKill "report-volume"
				++ "xmonad --recompile && xmonad --restart"
myRestartKill			:: String -> String
myRestartKill p			= "for pid in `pgrep " ++ p 
				++ "`; do kill -9 $pid; done && "

------------------------------------------------------------------------
-- UI/UX
------------------------------------------------------------------------

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- mod1Mask (left alt), mod3Mask (right alt), mod4Mask (super)
myModMask       = mod1Mask

-- workspaces = ["web", "irc", "code" ] ++ map show [4..9]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
myBorderWidth   = 1

-- Theme (Solarized) ---------------------------------------------------
base03          = "#002b36"
base02          = "#073642"
base01          = "#586e75"
base00          = "#657b83"
base0           = "#839496"
base1           = "#93a1a1"
base2           = "#eee8d5"
base3           = "#fdf6e3"
yellow          = "#b58900"
orange          = "#cb4b16"
red             = "#dc322f"
magenta         = "#d33682"
violet          = "#6c71c4"
blue            = "#268bd2"
cyan            = "#2aa198"
green           = "#719e07"

myNormalBorderColor = base02
myFocusedBorderColor  = green
--myFontSize s  = "xft:lettergothicmono:style=regular:pixelsize=" ++ show s
myFontSize s    = "xft:lettergothicmono:style=bold:pixelsize=" ++ show s
myFontBig       = myFontSize 16
myFont          = myFontSize 14
myFontSmall     = myFontSize 12

myTabTheme :: Theme
myTabTheme = defaultTheme
    { activeColor           = base03
    , inactiveColor         = base02
    , urgentColor           = yellow
    , activeBorderColor     = base03
    , inactiveBorderColor   = base03
    , urgentBorderColor     = yellow
    , activeTextColor       = base2
    , inactiveTextColor     = base01
    , urgentTextColor       = yellow
    , fontName              = myFontSmall
--  , decoHeight            = 20
    }

------------------------------------------------------------------------
-- Prompts
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Bindings
------------------------------------------------------------------------

-- Key bindings --------------------------------------------------------

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X())
myKeys = \conf -> EZ.mkKeymap conf $(_myKeys conf)
 
_myKeys :: XConfig Layout -> [(String, X())]
_myKeys  = \conf ->

    -- from Brent Yorgey's xmonad.sh in config archive
    -- mod-[1..],       Switch to workspace N
    -- mod-shift-[1..], Move client to workspace N
    -- mod-ctrl-[1..],  Switch to workspace N on other screen
    [ (m ++ "M-" ++ [k], f i)                                   -- (0)
        | (i, k) <- zip (XMonad.workspaces conf) "1234567890-=[]\\" -- (0)
        , (f, m) <- [ (windows . W.view, "")                    -- (0a)
                    , (windows . W.shift, "S-")
                    , (\ws -> nextScreen >> (windows . W.view $ ws), "C-")
                    ]
    ]
 
    ++

    [
    -- launch a terminal
      ("M-S-<Return>", spawn $ XMonad.terminal conf)

    -- close focused window
    , ("M-c", kill)

    -- close all windows
    , ("M-S-c", killAll)

     -- Rotate through the available layout algorithms
    , ("M-<Space>", sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ("M-n", refresh)

    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)

    -- Move focus to the next window
    , ("M-j", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-k", windows W.focusUp)

    -- Move focus to the master window
    , ("M-m", windows W.focusMaster)

    -- Swap the focused window and the master window
    , ("M-<Return>", windows W.swapMaster)

    -- Swap the focused window with the next window
    , ("M-S-j", windows W.swapDown)

    -- Swap the focused window with the previous window
    , ("M-S-k", windows W.swapUp)

    -- Shrink the master area
    , ("M-h", sendMessage Shrink)

    -- Expand the master area
    , ("M-l", sendMessage Expand)

    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-.", sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ("M-b", sendMessage ToggleStruts >> refresh)

    -- Quit xmonad
    , ("M-S-q", io (exitWith ExitSuccess))

    -- Restart xmonad
    , ("M-q", myRestart )
    ]

    ++
    [
      ("M4-b", spawn $ myBrowser )
    ]


    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
--    [((m .|. modm, k), windows $ f i)
--        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
--    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse bindings ------------------------------------------------------


------------------------------------------------------------------------
-- Layouts:
------------------------------------------------------------------------

myLayout = smartBorders $ 
           (tabs ||| tiled ||| Mirror tiled ||| Full)
           where
           tiled   = Tall nmaster delta ratio
           nmaster = 1
           ratio   = 1/2
           delta   = 3/100
           tabs    = named "Tabbed" $ mkTab $ Simplest
           mkTab l = tabBar shrinkText myTabTheme Top 
                     $ resizeVertical (fi $ decoHeight myTabTheme) $ l

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------

-- use xprop to id windows
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Skype"          --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , fullscreenManageHook
    , manageDocks
    ]
--  , resource  =? "kdesktop"       --> doIgnore ] <+> fullscreenManageHook

------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------

-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages and require
-- X.L.Fullscreen, hence the use of E.fullscreenEventHook and the 
-- XMonad.Layout.fullscreenEventHook below
myEventHook = E.ewmhDesktopsEventHook
          <+> E.fullscreenEventHook
          <+> fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------

myBar = "taffybar" -- Command to launch the bar.
myPP = defaultPP

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--myStartupHook = return ()
myStartupHook = do
    E.ewmhDesktopsStartup
    return ()

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- The main function.
main = do
    client <- connectSession
    xmonad =<< statusBar myBar myPP toggleStrutsKey (E.ewmh $ defaultConfig) {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
      --mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dbusLog client myPP,
        startupHook        = myStartupHook
    }

