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

-- Statusbar related ----------------------------------------------------

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

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

import XMonad.Layout.SimpleFloat

-- Application/System --------------------------------------------------

import XMonad.Actions.WithAll -- used for killall

-- Key Bindings --------------------------------------------------------

-- this method of applying X.U.EZConfig from loupgaroublonds config
--import qualified XMonad.Util.EZConfig as EZ
import XMonad.Util.EZConfig
import qualified Data.Map as M

import Data.List -- for ConditionalKeys

-- for fuzzy window prompt test
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowBringer

-- Navigation ----------------------------------------------------------
import XMonad.Actions.CycleWS

import XMonad.Prompt
import XMonad.Prompt.Window

import XMonad.Actions.Navigation2D

-- For fuzzy prompt matching test --------------------------------------
import Data.List
import Text.EditDistance
import XMonad.Prompt.Shell

------------------------------------------------------------------------
-- Fuzzy Spawn Test
------------------------------------------------------------------------
data FuzzySpawn = FuzzySpawn deriving (Read, Show)
instance XPrompt FuzzySpawn where showXPrompt _ = "Prompt Test: "

fuzzySpawn = do
    cmds <- io getCommands
    let compl s
          | null s = []
          | otherwise = let weight c = levenshteinDistance defaultEditCosts s c
--          | otherwise = let weight c = restrictedDamerauLevenshteinDistance defaultEditCosts s c
            in map snd $ take 10 $ sort $ map (\c -> (weight c,c)) cmds
    mkXPrompt FuzzySpawn myPromptConfig (return . compl) spawn

------------------------------------------------------------------------
-- Applications and Utilities
------------------------------------------------------------------------

urxvt           = "urxvtc || (urxvtd -f -o -q && urxvtc)"
chrome          = "chromium --memory-model=low --enable-print-preview --enable-smooth-scrolling"
firefox         = "aurora"
skype           = "skype"
calendar        = ""
mail            = ""
tasks           = ""

myBrowser       = chrome
myTerminal      = "urxvtc || (urxvtd -f -o -q && urxvtc)"

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
				++ myRestartKill "dunst"
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
myBorderWidth   = 2

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

myNormalBorderColor     = base02
myFocusedBorderColor    = green
--myFontSize s  = "xft:lettergothicmono:style=regular:pixelsize=" ++ show s
myFontSize s    = "-*-terminus-medium-r-normal--" ++ show s ++ "-*-*-*-*-*-*-*"
--myFontSize s    = "xft:Terminus:style=Medium:pixelsize=" ++ show s
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
    , fontName              = myFont
--  , decoHeight            = 20
    }

myPromptConfig :: XPConfig
myPromptConfig = defaultXPConfig
        { font			= "xft:Terminus:style=Medium:pixelsize=18"
        , bgColor		= base02
        , fgColor		= base0
        , fgHLight		= base2
        , bgHLight		= base03
        , borderColor		= base03
        , promptBorderWidth	= 2
        , height		= 26
        , autoComplete          = Just 500000
        }

------------------------------------------------------------------------
-- Prompts
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Applications
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Bindings
------------------------------------------------------------------------

-- Key bindings --------------------------------------------------------

myKeys  = \conf -> mkKeymap conf $

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [(m ++ i, windows $ f j)
        | (i, j) <- zip (map show [1..9]) (XMonad.workspaces conf)
        , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]] ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [(m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m) <- [(W.view, "M-"), (W.shift, "M-S-")]] ++

    -- Switch between layers
    [ ("M4-C-<Space>", switchLayer)

    -- Directional navigation of windows
    , ("M4-<Right>", windowGo R False)
    , ("M4-<Left>", windowGo L False)
    , ("M4-<Up>", windowGo U False)
    , ("M4-<Down>", windowGo D False)

    -- Swap adjacent windows
    , ("M4-C-<Right>", windowSwap R False)
    , ("M4-C-<Left>", windowSwap L False)
    , ("M4-C-<Up>", windowSwap U False)
    , ("M4-C-<Down>", windowSwap D False)

    -- Directional navigation of screens
    , ("M4-r", screenGo R False)
    , ("M4-l", screenGo L False)
    , ("M4-u", screenGo U False)
    , ("M4-d", screenGo D False)

    -- Swap workspaces on adjacent screens
    , ("M4-C-r", screenSwap R False)
    , ("M4-C-l", screenSwap L False)
    , ("M4-C-u", screenSwap U False)
    , ("M4-C-d", screenSwap D False)

    -- Send window to adjacent screen
    , ("M4-S-r", windowToScreen R False)
    , ("M4-S-l", windowToScreen L False)
    , ("M4-S-u", windowToScreen U False)
    , ("M4-S-d", windowToScreen D False)

    ] ++

    -- close focused window ---------------------------------------------
    -- close all windows ------------------------------------------------
    [ ("M-c",                   kill                                    )
    , ("M-S-c",                 killAll                                 )

     -- Rotate through the available layout algorithms ------------------
    , ("M-<Space>",             sendMessage NextLayout                  )

    --  Reset the layouts on the current workspace to default -----------
    , ("M-S-<Space>",           setLayout $ XMonad.layoutHook conf      )

    -- Resize viewed windows to the correct size ------------------------
    , ("M-n",                   refresh                                 )

    -- Move focus to the next window ------------------------------------
    -- Move focus to the next window ------------------------------------
    -- Move focus to the previous window --------------------------------
    -- Move focus to the master window ----------------------------------
    , ("M-<Tab>",               windows W.focusDown                     )
    --, ("M-j",                   windows W.focusDown                     )
    --, ("M-k",                   windows W.focusUp                       )

    -- vim movement through windows and navigation through workspaces
    , ("M-h", bindOn LD [("Tabbed", windows W.focusUp), ("", windowGo L False)])
    , ("M-j", windowGo D False)
    , ("M-k", windowGo U False)
    , ("M-l", bindOn LD [("Tabbed", windows W.focusDown), ("", windowGo R False)])

    , ("M-m",                   windows W.focusMaster                   )

    -- Swap the focused window and the master window --------------------
    -- Swap the focused window with the next window ---------------------
    -- Swap the focused window with the previous window -----------------
    , ("M-<Return>",            windows W.swapMaster                    )
    , ("M-S-j",                 windows W.swapDown                      )
    , ("M-S-k",                 windows W.swapUp                        )

    -- Shrink the master area -------------------------------------------
    -- Expand the master area -------------------------------------------
    , ("M-S-,",                 sendMessage Expand                      )
    , ("M-S-.",                 sendMessage Shrink                      )

    -- Push window back into tiling -------------------------------------
    , ("M-t",                   withFocused $ windows . W.sink          )

    -- Increment the number of windows in the master area ---------------
    -- Deincrement the number of windows in the master area -------------
    , ("M-,",                   sendMessage (IncMasterN 1)              )
    , ("M-.",                   sendMessage (IncMasterN (-1))           )

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ("M-b",                   sendMessage ToggleStruts >> refresh     )

    -- Quit xmonad ------------------------------------------------------
    -- Restart xmonad ---------------------------------------------------
    , ("M-S-q",                 io (exitWith ExitSuccess)               )
    , ("M-q",                   myRestart                               )
    ] ++

    -- launch a terminal ------------------------------------------------
    [ ("M-S-<Return>",          spawn $ XMonad.terminal conf            )
    , ("M4-b",                  spawn $ myBrowser                       )
    , ("M4-c",                  spawn $ chrome                          )
    , ("M4-f",                  spawn $ firefox                         )
    , ("M4-t",                  spawn $ "thunar &"                      )
    , ("M4-x",                  fuzzySpawn )
    ]
 ++

    [ ("<XF86Display>",         spawn $ "execute display cycle"         )
    , ("C-<XF86Display>",       spawn $ "execute display external"      )
    , ("S-<XF86Display>",       spawn $ "execute display internal"      )
    , ("S-C-<XF86Display>",     spawn $ "execute display mirror"        )
    ]

--  ++

--  [ ("M4-w", windowPromptGoto myPromptConfig) ]

-- >     , ("C-M-h", bindOn LD [("MRT", sendMessage MirrorExpand), ("", sendMessage Shrink)]
-- >     , ("C-M-l", bindOn LD [("MRT", sendMessage MirrorShrink), ("", sendMessage Expand)]
-- >     , ("C-M-k", bindOn LD [("MRT", sendMessage Shrink), ("", sendMessage MirrorExpand)]
-- >     , ("C-M-j", bindOn LD [("MRT", sendMessage Expand), ("", sendMessage MirrorShrink)]
--
-- >  -- # vs.
-- >     , ("C-M-h", LK.bindOn [("MRT", sendMessage MirrorExpand), ("", sendMessage Shrink)]
-- >  -- # or
-- >     , ("C-M-h", bindOnLayouts [("MRT", sendMessage MirrorExpand), ("", sendMessage Shrink)]


-- XMonad.Actions.ConditionalKeys code ---------------------------------
-- from: http://www.haskell.org/pipermail/xmonad/attachments/20100322/e7ea7411/ConditionalKeys.obj

data XCond = WS | LD

-- | Choose an action based on the current workspace id (WS) or
-- layout description (LD).
chooseAction :: XCond -> (String->X()) -> X()
chooseAction WS f = withWindowSet (f . W.currentTag)
chooseAction LD f = withWindowSet (f . description . W.layout . W.workspace . W.current)

-- | If current workspace or layout string is listed, run the associated
-- action (only the first match counts!) If it isn't listed, then run the default
-- action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindOn :: XCond -> [(String, X())] -> X()
bindOn xc bindings = chooseAction xc $ chooser where
    chooser xc = case find ((xc==).fst) bindings of
        Just (_, action) -> action
        Nothing -> case find ((""==).fst) bindings of
            Just (_, action) -> action
            Nothing -> return ()


-- Mouse bindings ------------------------------------------------------


------------------------------------------------------------------------
-- Layouts:
------------------------------------------------------------------------

-- The addition of noBorders in addition to smartBorders on all layouts
-- is to address borders showing up on full screen windows while in 
-- multimonitor mode.
myLayout = smartBorders $ (tabs ||| tiledX ||| tiledY ||| full ||| float)
           where
           tiledX       = named "Tiled Tall" $ Tall nmaster delta ratio
           tiledY       = named "Tiled Wide" $ Mirror $ Tall nmaster delta ratio
           --full       = named "Fullscreen" $ noBorders $ Full
           full         = named "Fullscreen" $ Full
           float        = named "Floating" $ simpleFloat
           nmaster      = 1
           ratio        = 1/2
           delta        = 3/100
           --tabs       = named "Tabbed" $ noBorders $ mkTab $ Simplest
           tabs         = named "Tabbed" $ makeTab $ Simplest
           makeTab l    = tabBar shrinkText myTabTheme Top 
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
myEventHook   = E.ewmhDesktopsEventHook
            <+> E.fullscreenEventHook
            <+> fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------

--myBar = "taffybar" -- Command to launch the bar.
myBar = "xmobar" -- Command to launch the bar.
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
    spawn $ "trayer"
          ++ " --edge top --align right"
          ++ " --SetDockType true --SetPartialStrut true"
          ++ " --expand true --widthtype percent --width 5"
          ++ " --transparent true --tint 0x073642 --alpha 50"
          ++ " --height 20"
          ++ " &"
    spawn $ "xcompmgr -f -D 6 &"
    spawn $ "dunst -geometry 0x10-0 -fn "   -- width of message, max ten lines high, right flush top
          ++ show myFontBig 
          ++ " -lb " ++ show yellow 
          ++ " -nb " ++ show green 
          ++ " -cb " ++ show magenta 
          ++ " -lf " ++ show base03 
          ++ " -nf " ++ show base03 
          ++ " -cf " ++ show base03 
          ++ " -key Return -mod mod4"       -- Super+Return to dismiss
          ++ " -lto 2 -nto 5 -cto 0"        -- 2/5 second time out for low/normal, no timeout for critical
          ++ " -s"                          -- sort messages by urgency
          ++ " &"
    return ()

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- The main function.
--main = xmonad =<< xmobar (E.ewmh $ withNavigation2DConfig defaultNavigation2DConfig $ myConfig)

main = do 
    xmonad . E.ewmh . withNavigation2DConfig defaultNavigation2DConfig
    =<< xmobar myConfig

--main = xmonad $ addDescrKeys ((mod4Mask, xK_F1), xMessage) myKeys
--                    defaultConfig { modMask = mod4Mask }

--myConfig = defaultConfig {
--myConfig = addDescrKeys ((mod4Mask, xK_d), xMessage) myKeys $ defaultConfig {
myConfig = defaultConfig {
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
        --logHook            = myLogHook,
        startupHook        = myStartupHook
    }
-- >    -- Switch between layers
-- >    , ((modm,                 xK_space), switchLayers)
-- >
-- >    -- Directional navigation of windows
-- >    , ((modm,                 xK_Right), windowGo R False)
-- >    , ((modm,                 xK_Left ), windowGo L False)
-- >    , ((modm,                 xK_Up   ), windowGo U False)
-- >    , ((modm,                 xK_Down ), windowGo D False)
-- >
-- >    -- Swap adjacent windows
-- >    , ((modm .|. controlMask, xK_Right), windowSwap R False)
-- >    , ((modm .|. controlMask, xK_Left ), windowSwap L False)
-- >    , ((modm .|. controlMask, xK_Up   ), windowSwap U False)
-- >    , ((modm .|. controlMask, xK_Down ), windowSwap D False)
-- >
-- >    -- Directional navigation of screens
-- >    , ((modm,                 xK_r    ), screenGo R False)
-- >    , ((modm,                 xK_l    ), screenGo L False)
-- >    , ((modm,                 xK_u    ), screenGo U False)
-- >    , ((modm,                 xK_d    ), screenGo D False)
-- >
-- >    -- Swap workspaces on adjacent screens
-- >    , ((modm .|. controlMask, xK_r    ), screenSwap R False)
-- >    , ((modm .|. controlMask, xK_l    ), screenSwap L False)
-- >    , ((modm .|. controlMask, xK_u    ), screenSwap U False)
-- >    , ((modm .|. controlMask, xK_d    ), screenSwap D False)
-- >
-- >    -- Send window to adjacent screen
-- >    , ((modm .|. mod1Mask,    xK_r    ), windowToScreen R False)
-- >    , ((modm .|. mod1Mask,    xK_l    ), windowToScreen L False)
-- >    , ((modm .|. mod1Mask,    xK_u    ), windowToScreen U False)
-- >    , ((modm .|. mod1Mask,    xK_d    ), windowToScreen D False)
--
-- and add the configuration of the module to your main function:
--
