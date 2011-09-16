import XMonad                       -- I am the MCP, tron
import XMonad.Actions.WithAll       -- act on all windows (killall)
import XMonad.Actions.GridSelect    -- fancy grid menus for all
import XMonad.Hooks.DynamicLog      -- simplifies xmobar use
import XMonad.Actions.CycleWS       -- cycle on / shift to workspaces
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
                                    -- hiding fullscreenEventHook as
                                    -- we are handling this via the
                                    -- newer Layout.Fullscreen
import XMonad.Hooks.ManageDocks     -- status bar awareness
import XMonad.Hooks.ManageHelpers   -- isFullscreen, doFullFloat
import XMonad.Layout.NoBorders      -- get smart about borders
import XMonad.Layout.Fullscreen     -- a better way to fullscreen
import XMonad.Util.EZConfig         -- easier key mapping
import XMonad.Layout.Named          -- to name a thing is to control it
import XMonad.Hooks.UrgencyHook     -- get my attention
import SendFile
import qualified XMonad.Prompt as P

--import XMonad.Util.WindowProperties
import XMonad.Util.NamedWindows
import qualified XMonad.StackSet as W
--import qualified Data.Map        as M

import XMonad.Util.Run
import XMonad.Layout.Simplest
import XMonad.Layout.Decoration
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.ResizeScreen

--MAIN
------------------------------------------------------------------------
main            = xmonad =<<
                  statusBar myBar myPP toggleStrutsKey (ewmh $ myUrgencyHook $ myConfig)

myUrgencyHook   = withUrgencyHook NoUrgencyHook

--myUrgencyHook   = withUrgencyHook XmobarUrgencyHook

--highlight workspace name/number in xmobar

--data XmobarUrgencyHook = XmobarUrgencyHook deriving (Read, Show)
--instance UrgencyHook XmobarUrgencyHook where
--    urgencyHook _ _ = return ()

--dzenUrgencyHook :: DzenUrgencyHook
--dzenUrgencyHook = DzenUrgencyHook { duration = seconds 5, args = [] }
--
--
--data XmobarUrgencyHook = XmobarUrgencyHook deriving (Read, Show)
--instance UrgencyHook XmobarUrgencyHook where
--    urgencyHook XmobarUrgencyHook w = do
--        name <- getName w
--        ws <- gets windowset
--        whenJust (W.findTag w ws) (flash name)
--      where flash name index = spawn "xmobar /home/es/.xmonad/xmobarrc-simple"
--      --where flash name index = safeSpawn "xmobar" (show name ++ " requests your attention on workspace " ++ index)

--XMOBAR
------------------------------------------------------------------------
myBar           = "xmobar"
myPP            = xmobarPP
                { ppCurrent = xmobarColor base0 "" . wrap "[" "]" 
                , ppVisible = wrap "(" ")"
                --, ppHidden  = wrap " " " "
                , ppUrgent  = xmobarColor red base03 . wrap "*" "*"
                , ppWsSep   = " "
                , ppTitle   = xmobarColor green "" . shorten 40 }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

--APPS
------------------------------------------------------------------------
myTerminal      = "urxvtc"
myBrowser       = "chrome"
--myBrowser       = "firefox"
--myBrowser       = "uzbl-browser"

-- | Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

--THEME
------------------------------------------------------------------------
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

myModMask       = mod1Mask
myBorderWidth   = 1
myBorder        = base02
myBorderFocus   = green
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
--    , decoHeight            = 20
    }

--GRIDSELECT
------------------------------------------------------------------------
myGSConfig      = defaultGSConfig
                { gs_cellheight = 45
                , gs_cellwidth  = 200
                , gs_font = myFontBig
                }

--MANAGEHOOK
------------------------------------------------------------------------
myManageHook = composeOne 
                                [ transience
                                , isFullscreen -?> doFullFloat
                                , className =? "Skype"          -?> doFloat
                                ] <+> composeAll
                                [ fullscreenManageHook
                                ]
--
--myManageHook    = composeAll
--                [ fullscreenManageHook
--                , isFullscreen                  --> doFullFloat
--                , className =? "MPlayer"        --> doFloat
--                , className =? "Gimp"           --> doFloat
--                , className =? "Skype"          --> doFloat
--                , className =? "Cellwriter"     --> doIgnore
--                , resource  =? "desktop_window" --> doIgnore ]
--                <+> manageDocks

--LAYOUT
------------------------------------------------------------------------
--myLayout        = fullscreenFocus $ fullscreenFloat $
--                  Tall 1 (1/2) (3/100)

myLayout        = fullscreenFocus $
                    fullscreenFloat $ 
                    avoidStruts $
                    smartBorders $ 
                    (tabs ||| tiled ||| Mirror tiled ||| Full)
                        where
                        tiled   = Tall nmaster delta ratio
                        nmaster = 1
                        ratio   = 1/2
                        delta   = 3/100
                        tabs    = named "Tabbed"
                                  $ myTab $ Simplest
                        myTab l = tabBar shrinkText myTabTheme Top
                                  $ resizeVertical (fi $ decoHeight myTabTheme)
                                  $ l

--myLayout        = avoidStruts $ smartBorders $
--                  (tiled ||| Mirror tiled ||| Full)
--                  where
--                    tiled   = Tall nmaster delta ratio
--                    nmaster = 1
--                    ratio   = 1/2
--                    delta   = 3/100

--BINDINGS
------------------------------------------------------------------------
myKeys          = myKeymap myConfig
myKeymap conf   = [ ("S-M-c",               killAll                 )
                  , ("M-<Right>",           moveTo Next NonEmptyWS  )
                  , ("M-<Left>",            moveTo Prev NonEmptyWS  )
                  , ("M4-b",                spawn myBrowser         )
                  , ("M4-m",                sendFilePrompt P.defaultXPConfig "~/var/mail/contacts/aliases2")
                  , ("M4-x",                goToSelected myGSConfig )
                  , ("M3",                spawn myBrowser )
                  , ("<XF86RotateWindows>", goToSelected myGSConfig )
                  , ("M-g",                 goToSelected myGSConfig )
                  , ("M-c",                 kill                    ) ]
--TODO: break out apps as separate concatenated list
--myMouseBindings = [ ((0,button1), (\w -> nextWS)) ]

--STARTUP
------------------------------------------------------------------------
myStartupHook   = do
                  ewmhDesktopsStartup
                  return () >> checkKeymap myConfig myKeys
-- myStartupHook   = return () >> checkKeymap myConfig myKeys

--CONFIG
------------------------------------------------------------------------
myConfig        = defaultConfig
                  { borderWidth         = myBorderWidth
                  , normalBorderColor   = myBorder
                  , focusedBorderColor  = myBorderFocus
                  , focusFollowsMouse   = myFocusFollowsMouse
                  , handleEventHook     = fullscreenEventHook
                  , layoutHook          = myLayout
                  , manageHook          = myManageHook
                  , modMask             = myModMask
                  , startupHook         = myStartupHook
                  , terminal            = myTerminal }
                  `additionalKeysP`     (myKeys)
--                `additionalMouseBindings` (myMouseBindings)
