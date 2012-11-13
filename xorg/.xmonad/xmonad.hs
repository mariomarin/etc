-- xmonad config file
-- es@ethanschoonover.com

-- coding style guidelines: http://goo.gl/MW05v
-- sources of inspiration and code:
-- https://github.com/thomasf/dotfiles-thomasf-xmonad/
-- https://github.com/league/dot-files/blob/master/xmonad.hs
-- https://github.com/pbrisbin/xmonad-config/
-- https://github.com/mntnoe/mntnoe-dotfiles

-- Imports
------------------------------------------------------------------------

import           Control.Monad
import           Data.List
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit(ExitCode(ExitSuccess), exitWith)
import           System.IO
import           XMonad hiding ((|||))
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CopyWindow(copy)
import           XMonad.Actions.CycleSelectedLayouts
import           XMonad.Actions.CycleWS
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.Submap
import           XMonad.Actions.Submap
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowGo
import           XMonad.Actions.WithAll
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Decoration
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.LayoutCombinators ((|||))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Simplest
import           XMonad.Layout.TabBarDecoration
import           XMonad.ManageHook
import           XMonad.Prompt
import           XMonad.Prompt
import           XMonad.Prompt.Workspace
import           XMonad.Util.NamedActionsLocal
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.WindowProperties
import           XMonad.Layout.Decoration
import           XMonad.Layout.DecorationAddons
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.Drawer
import           XMonad.Layout.ImageButtonDecoration 
import           XMonad.Layout.ShowWName
import           XMonad.Layout.WindowSwitcherDecoration
import           XMonad.Layout.TabbedWindowSwitcherDecoration
import           XMonad.Util.Image
import qualified Data.Map as M
import qualified System.IO.UTF8
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Hooks.EwmhDesktops as E
import qualified XMonad.StackSet as W


-- Prompt(s)
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Workspace

-- Topics
import XMonad.Actions.TopicSpace


-- TODO: implement a preconfigured grid select for tablet mode
-- implement normal gridselect
import XMonad.Actions.GridSelect



--import Text.Regex.Posix ((=~))

-- Note on fullscreen related modules
-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages and require
-- X.L.Fullscreen, so we import both and reference X.H.EwmhDesktops as E

--Notes on window properties:
--Where XMonad.Util.WindowProperties is used, the following properties
--and operators are used:
--
--Title String	 
--ClassName String	 
--Resource String	 
--Role String	
--WM_WINDOW_ROLE property
--Machine String	
--WM_CLIENT_MACHINE property
--And Property Property	 
--Or Property Property	 
--Not Property	 
--Const Bool


-- Keyboard configuration:
------------------------------------------------------------------------

super = mod4Mask
alt   = mod1Mask
ctrl  = controlMask
shft  = shiftMask
myModMask = alt

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf@(XConfig {XMonad.modMask = modm}) =

    [ subtitle "MAIN APPS"
    , ((modm, xK_Return),           addName "New terminal"              $ newTerminal)
    , ((modm .|. shft, xK_Return),  addName "Next existing terminal"    $ nextTerminal)
    , ((super, xK_Return),          addName "New browser"               $ newBrowser)
    , ((super .|. shft, xK_Return), addName "Next existing browser"     $ nextBrowser)
    , ((super .|. shft, xK_Return), addName "Show marginal notes"       $ nextBrowser)

    , subtitle "TEST"
    , ((super, xK_y),               addName "Test 1"                    $ cycleTiledLayouts)
    , ((super .|. ctrl, xK_y),      addName "Test 1"                    $ fullScreen)
    , ((super .|. shft, xK_y),      addName "Test 2"                    $ sendMessage $ SetStruts [] [minBound .. maxBound])

    , subtitle "SECONDAY APPS"
    , ((super, xK_m),               addName "Show mail"                 $ showMail)
    , ((super, xK_p),               addName "Show web contacts"         $ showWebContacts)
    , ((super .|. shft, xK_c),      addName "Show calendar"             $ showWebCalendar)
    , ((super, xK_t),               addName "Show tasks"                $ showWebTasks)
    , ((super, xK_d),               addName "Show Google drive"         $ showWebDrive)
    , ((super, xK_i),               addName "Show Chat Drawer"          $ showChatDrawer)
    , ((super, xK_v),               addName "New Vim"                   $ newVim)
    , ((super .|. shft, xK_v),      addName "Next Vim"                  $ nextVim)

    --, subtitle "COMMON"
    -- TODO: implement persistent (copied) windows that toggle off via kill1
    -- functioning similarly to named scratch pads with a toggle function and a
    -- possibly global layout modification that pushes other layout over
    --, ((super, xK_e),               addName "Common Editor"             $ toggleEditor)

    , subtitle "SCRATCHPADS"
    , ((super, xK_n),               addName "Persistent Notes"          $ toggleScratchpad "notepad")
    , ((super, xK_x),               addName "Audio Mixer"               $ toggleScratchpad "mixer")
    , ((super, xK_h),               addName "Process monitor"           $ toggleScratchpad "htop")
    , ((super, xK_c),               addName "Calendar - week"           $ toggleScratchpad "calweek")
    , ((super, xK_v),               addName "Calendar - month"          $ toggleScratchpad "calmonth")
    , ((super, xK_w),               addName "Wifi connection menu"      $ toggleScratchpad "wifi")

    , subtitle "KILL & QUIT"
    , ((modm, xK_BackSpace),        addName "Close focused"             $ kill1)
    , ((modm .|. shft, bkSpc),      addName "Close all on workspace"    $ killAll)
    , ((modm, xK_q),                addName "Restart XMonad"            $ myRestart)
    , ((modm .|. shft, xK_q),       addName "Quit XMonad"               $ myQuit)

-- TODO: make all movement hjkl ... alt+ = local windows, super+ = workspaces, alt+super+ = screens

    , subtitle "WINDOW CYCLING (J/K) [+=focus] [+ctrl=keep focus] [+shift=move]"
    , ((modm, xK_j),                addName "Focus next window"         $ windows W.focusDown)
    , ((modm, xK_k),                addName "Focus previous window"     $ windows W.focusUp)
    , ((modm .|. shft, xK_j),       addName "Swap focused w/previous"   $ windows W.swapDown)
    , ((modm .|. shft, xK_k),       addName "Swap focused w/next"       $ windows W.swapUp)
    , ((modm .|. ctrl, xK_j),       addName "Rotate counterclockwise"   $ rotAllDown)
    , ((modm .|. ctrl, xK_k),       addName "Rotate clockwise"          $ rotAllUp)

    , subtitle "WINDOW ACTIONS"
    , ((modm, xK_m),                addName "Move focused to master"    $ windows W.focusMaster)
    , ((modm .|. shft, xK_m),       addName "Swap focused and master"   $ windows W.swapMaster)
    , ((modm, xK_t),                addName "Tile this window"          $ withFocused (windows . W.sink))
    , ((modm, xK_u),                addName "Focus urgent winow"        $ focusUrgent)
    , ((modm .|. ctrl, xK_u),       addName "Clear urgent status"       $ clearUrgents)
    , ((modm, xK_a),                addName "Pin window"                $ windows copyToAll)
    , ((modm .|. shft, xK_a),       addName "Unpin window"              $ killAllOtherCopies)

    -- X.A.CycleWS is doing the heavy lifting here
    -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-CycleWS.html
    , subtitle "WORKSPACE ACTIONS (N/P) [mod=from prefix] [mod+ctrl=from all]"
    , ((modm, xK_l),                addName "Next workspace"            $ nextWS >> flashWS )
    , ((modm, xK_h),                addName "Previous workspace"        $ prevWS >> flashWS)
    , ((modm .|. shft, xK_l),       addName "Move to next workspace"    $ shiftToNext >> nextWS >> flashWS)
    , ((modm .|. shft, xK_h),       addName "Move to prev workspace"    $ shiftToPrev >> prevWS >> flashWS)
    , ((modm .|. ctrl, xK_l),       addName "Toss to next workspace"    $ shiftToNext >> flashWS)
    , ((modm .|. ctrl, xK_h),       addName "Toss to prev workspace"    $ shiftToPrev >> flashWS)
    , ((modm, xK_o),                addName "Jump to prev workspace"    $ toggleWS >> flashWS)

    , subtitle "SCREEN CYCLING (D/F) [+=select] [+ctrl=swap] [+shift=move window to]"
    , ((modm, xK_n),                addName "Next screen"               $ nextScreen >> moveCursor)
    , ((modm, xK_p),                addName "Previous screen"           $ prevScreen >> moveCursor)
    , ((modm .|. shft, xK_n),       addName "Move win to next screen"   $ shiftNextScreen >> nextScreen >> moveCursor)
    , ((modm .|. shft, xK_p),       addName "Move win to prev screen"   $ shiftPrevScreen >> prevScreen >> moveCursor)
    , ((modm .|. ctrl, xK_n),       addName "Swap screen w/next"        $ swapNextScreen >> nextScreen >> moveCursor)
    , ((modm .|. ctrl, xK_p),       addName "Swap screen w/prev"        $ swapPrevScreen >> nextScreen >> moveCursor)

--  , subtitle "Workspace prompts"
--  , ((modm.|. shft, xK_n),        addName "Make/change workspc prmpt" $ rmEmptyWs $ selectWorkspacePrompt >> maybeWorkspaceAction >> moveCursor)
--  , ((modm.|. shft, xK_n),        addName "Move win to workspc prmpt" $ DW.withWorkspace myXPConfig (windows . W.shift) >> movePointer)
--  , ((modm.|. ctrl, xK_n),        addName "Rename current workspace"  $ DW.renameWorkspace myXPConfig >> movePointer)
--  , ((modm.|. ctrl, bkSpc),       addName "Remove current workspace"  $ DW.removeWorkspace >> movePointer)
--  , ((modm, xK_o),                addName "Window search prmpt"       $ gotoMenuArgs ["-l 23"] >> movePointer)

-- TODO: decide if I'm going to use different cycleThroughLayouts or limit layouts on a perworkspace basis

    , subtitle "WORKSPACE LAYOUTS (H/L=size ,.=) [+alt=toggle]"
    , ((modm, xK_space),            addName "Switch to next layout"     $ cycleAllLayouts)
    , ((modm .|. shft, xK_space),   addName "Reset to default layout"   $ setLayout $ XMonad.layoutHook conf)
    , ((modm .|. ctrl, xK_space),   addName "Refresh layout"            $ refresh)
    , ((modm, xK_f),                addName "Full Screen"               $ fullScreen)

    , ((modm .|. shft, xK_9),       addName "Shrink the master area"    $ sendMessage Shrink)
    , ((modm .|. shft, xK_0),       addName "Expand the master area"    $ sendMessage Expand)
    , ((modm, xK_comma),            addName "+ windows in master area"  $ sendMessage (IncMasterN 1))
    , ((modm, xK_period),           addName "- windows in master area"  $ sendMessage (IncMasterN (-1)))

    , subtitle "WORKSPACE LAYOUTS (H/L=size ,.=) [+alt=toggle]"
    -- this is the inverse of what I'm going to do
    --, ((ctrl, xK_j),              addName "Next tab in Chrome"        $ focusedHasProperty (ClassName chromeClass) >>= 
    --                                                                    flip unless chromeTabNext) 
    --, ((ctrl, xK_k),              addName "Previous tab in Chrome"    $ focusedHasProperty (ClassName chromeClass) >>= 
    --                                                                    flip unless chromeTabPrevious) 
    --, ((ctrl, xK_k),              addName "Previous tab in Chrome"    $ spawn "sleep 0.1 && xdotool key shift+control+Tab")

    , subtitle "test submap" 
    , ((modm, xK_z), submapName $ 
    [ ((0, xK_o),                   addName "test submap"               $ sendMessage Shrink)
    , ((0, xK_z),                   addName "test submap"               $ sendMessage Shrink)]
    )


    , subtitle "MEDIA KEYS"
    -- c.f. key names at http://hackage.haskell.org/cgi-bin/hackage-scripts/package/X11

    , ((0, volUp),                  addName "Volume up by 1"            $ spawn "volume up")
    , ((0, volDown),                addName "Volume down by 1"          $ spawn "volume down")
    , separator
    , ((0 .|. shft, volUp),         addName "Volume up by 10"           $ spawn "volume up 10")
    , ((0 .|. shft, volDown),       addName "Volume down by 10"         $ spawn "volume down 10")
    , separator
    , ((0 .|. ctrl, volUp),         addName "Volume at 50%"             $ spawn "volume max")
    , ((0 .|. ctrl, volDown),       addName "Volume at maximum"         $ spawn "volume mid")
    , ((0, volMute),                addName "Volume mute toggle"        $ spawn "volume toggle")


    , subtitle "HOT KEYS" 

    , ((0, btnBatt),                addName "Toggle min/max pwr modes"  $ spawn "power toggle")
    , ((0 .|. shft, btnBatt),       addName "Toggle miv/mov pwr modes"  $ spawn "power toggle pinned")
    , ((0 .|. ctrl, btnBatt),       addName "Auto power modes"          $ spawn "power toggle pinned")
    , ((0, btnSleep),               addName "System sleep"              $ spawn "system sleep")
    , ((0, btnPower),               addName "System power off"          $ spawn "system off")
    , ((0 .|. shft, btnPower),      addName "System reboot"             $ spawn "system reboot")
    , ((0, btnLockScreen),          addName "Lock screen"               $ spawn "display lock")
    , ((0, btnDisplay),             addName "Cycle display mode"        $ spawn "display toggle")
    , ((0 .|. shft, btnDisplay),    addName "Mirror display mode"       $ spawn "display mirror")
    , ((0 .|. ctrl, btnDisplay),    addName "Span display mode"         $ spawn "display span")
    , ((0, btnBluetooth),           addName "Bluetooth toggle"          $ spawn "wireless bluetooth toggle")
    , ((0, prtSc),                  addName "Screendraw - start/finish" $ spawn "screendraw")
    , ((0 .|. shft, prtSc),         addName "Screendraw - force finish" $ spawn "screendraw finish")
    , ((modm, prtSc),               addName "Screendraw - cancel"       $ spawn "screendraw cancel")
    , ((0, btnRotate),              addName "Screendraw - clear"        $ spawn "screendraw clear")
    --, ((0, btnSuspend),           addName "System Reboot"             $ spawn "system reboot")
    ]

    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N

    ++
    subtitle "WORKSPACE SWITCHING: Alt+1-9":
--    [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
--        | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shft, "Move client to workspace ")]
--        , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
    [((m .|. modm, k), addName (n ++ i) $ (windows $ f i) >> moveCursor >> flashWS)
        | (f, m, n) <- [(W.view, 0, "Switch to workspace "), (W.shift, shft, "Move client to workspace ")]
        , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]

    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3

    ++
    subtitle "SCREEN SWITCHING":
    [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shft, "Move client to screen number ")]
        , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

--    ++
--    subtitle "WORKSPACE SWITCHING":
--    [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
--        | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shft, "Move client to workspace ")]
--        , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
--
--    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
--    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
--
--    ++
--    subtitle "SCREEN SWITCHING":
--    [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
--        | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shft, "Move client to screen number ")]
--        , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

        where

        volUp           = xF86XK_AudioRaiseVolume
        volDown         = xF86XK_AudioLowerVolume
        volMute         = xF86XK_AudioMute
        btnDisplay      = xF86XK_Display
        btnLockScreen   = xF86XK_ScreenSaver
        btnBatt         = 0x1008ff93
        btnSleep        = xF86XK_Sleep
        btnPower        = xF86XK_PowerOff
        btnBluetooth    = xF86XK_Launch1
        btnRotate       = xF86XK_RotateWindows
        btnSuspend      = 0x1008ffa7
        prtSc           = xK_Print
        bkSpc           = xK_BackSpace

        moveCursor = updatePointer (Relative 0.99 0.99)

        -- use the following in a key binding such as:
        --, ((modm, xK_space),
        --  addName "Switch to next layout"
        --  $ sendMessage NextLayout >> (curLayout >>= \d->spawn $"flash "++d)
        curLayout :: X String
        curLayout = gets windowset >>= return . description . W.layout . W.workspace . W.current

        curWorkspace :: X String
        curWorkspace = withWindowSet (return . W.currentTag)
        --flashWS = (curWorkspace >>= \d->spawn $"flash "++d)
        flashWS = return ()

        -- | Select workspae prompt
        selectWorkspacePrompt = workspacePrompt myPromptConfig $ \w ->
                                do s <- gets windowset
                                   if W.tagMember w s
                                     then windows $ W.view w
                                     else DW.addWorkspace w


-- Applications
------------------------------------------------------------------------

myTerminal          = "urxvtc"
myTerminalStart :: X ()
myTerminalStart     = spawn "pgrep urxvtd || urxvtd -f -o -q"
newTerminal :: X ()
newTerminal         = spawn myTerminal
nextTerminal        = raiseNextMaybe
                      (spawn $ myTerminal) (className =? "URxvt")

showNotesDrawer     = raiseNextMaybe
                      (spawn $ myTerminal ++ " -name notes") (resource =? "notes")

showChatDrawer      = raiseNextMaybe
                      (spawn $ myTerminal ++ " -name drawer") (resource =? "drawer")

newVim :: X ()
newVim              = runInTerm "" "vim"
nextVim             = raiseNextMaybe
                      (runInTerm "" "vim") (title =? "vim")

showMail            = raiseMaybe 
                      (runInTerm "" "mutt") (title =? "mutt")

chromeClass         = "Chromium"
chromeBase          = "chromium --memory-model=low"
                    ++ "     --enable-print-preview"
                    ++ "     --enable-smooth-scrolling"
                    ++ "     --enable-sync-extensions"
                    ++ "     --enable-webgl"
                    ++ "     --ignore-gpu-blacklist"
chrome              = chromeBase
                    ++ " --class=Chromium --name=chromium"

newBrowser :: X ()
newBrowser          = spawn $ chrome
nextBrowser         = raiseNextMaybe
                      (spawn $ chrome) (className =? chromeClass)

showWebApp :: String -> String -> X ()
showWebApp u m      = raiseNextMaybe
                      (spawn $ (appBaseCmdURL $ u) ) 
                      (className =? "ChromiumAppMode" 
                      <&&>
                      (fmap
                      (\t -> isPrefixOf m t || isSuffixOf m t)
                      title)) 

newWebApp :: String -> X ()
newWebApp u         = spawn $ (appBaseCmdURL $ u) 

appBaseCmdURL :: String -> String
appBaseCmdURL u     = chromeBase
                    ++ " --class=ChromiumAppMode"
                    ++ " --name==chromiumappmode"
                    ++ " --user-data-dir="
                    ++ "/home/es/.config/chromium-app-mode"
                    ++ " --app=" ++ u

showWebMail         = showWebApp "https://mail.google.com"
                      "Ethan Schoonover Mail"

showWebCalendar     = showWebApp "https://calendar.google.com"
                      "Ethan Schoonover - Calendar"

showWebContacts     = showWebApp "https://www.google.com/contacts"
                      "Google Contacts"

showWebDrive        = showWebApp "https://drive.google.com"
                      "Google Drive"

newWebDrive         = newWebApp  "https://drive.google.com" 

showWebNews         = showWebApp "https://reader.google.com"
                      "Google Reader"

showWebTasks        = showWebApp "https://astrid.com"
                      "Astrid"

showWebVault        = showWebApp
                      ("chrome-extension://"
                      ++ "hdokiejnpimakedhajhdlcegeplioahd"
                      ++ "/homelocal2.html")
                      "My LastPass Vault"

unspawn :: String -> X ()
unspawn p = spawn $ "for pid in `pgrep " 
                  ++ p ++ "`; do kill -9 $pid; done && "

startCoreApps :: X ()
startCoreApps       = do
                      showWebNews
                      showWebDrive
                      showWebContacts
                      showWebTasks
                      showWebCalendar
                      showMail
                      showWebVault

mySystemTrayStart   :: X ()
mySystemTrayStop    :: X ()
mySystemTrayStart   = spawn   "systray &"
mySystemTrayStop    = unspawn "trayer"

myCompositorStart   :: X ()
myCompositorStop    :: X ()
myCompositorStart   = spawn   "compton -f -D 6 -m 0.95 &"
myCompositorStop    = unspawn "compton"
-- old xcompmgr command: "xcompmgr -f -D 6 &"

myNotifierStart     :: X ()
myNotifierStop      :: X ()
myNotifierStart     = spawn   "dunst &"
myNotifierStop      = unspawn "dunst"

myStatusBarStart    :: X ()
myStatusBarStop     :: X ()
myStatusBarStart    = return ()
myStatusBarStop     = unspawn "xmobar"


-- Startup & Restarting
------------------------------------------------------------------------

myXMonadRestart :: X ()
myXMonadRestart  = spawn $ "xmonad --recompile && xmonad --restart"
                   ++ " || warn 'XMonad recompile failed'"

myStartupHook = do
    E.ewmhDesktopsStartup
    mySystemTrayStart
    myCompositorStart
    myNotifierStart
    myTerminalStart

myRestart :: X ()
myRestart = do
    myStatusBarStop
    mySystemTrayStop
    myCompositorStop
    myNotifierStop
    myXMonadRestart

myQuit = do
    io (exitWith ExitSuccess)


-- Scratch Pads
------------------------------------------------------------------------

toggleScratchpad sp = namedScratchpadAction myScratchpads sp

spTerminal :: String -> String -> String -> String
spTerminal f a c    = myTerminal
                    ++ " -fn " ++ show f ++ " -fb " ++ show f
                    ++ " -fi " ++ show f ++ " -fbi " ++ show f
                    ++ " +sb " ++ " -b 15 " ++ a ++ " -e " ++ c

myScratchpads =
    [ NS "htop"
      (spTerminal myFont "" "htop")
      (title =? "htop") centWin

    , NS "wifi"
      (spTerminal myFont "-name wifi" "wifi")
      (resource =? "wifi") centSquare

    , NS "notepad"
      (spTerminal myFont "-name notepad" "vim")
      (resource =? "notepad") centSquare

    , NS "mixer"
      (spTerminal myFontBig "" "alsamixer")
      (title =? "alsamixer") centWinBig

    , NS "calweek"
      (spTerminal myFont
      ("-name calweek -cr " ++ show base03) "gcal view week")
      (resource =? "calweek") centWinThin

    , NS "calmonth"
      (spTerminal myFontSmall
      ("-name calmonth -cr " ++ show base03) "gcal view month")
      (resource =? "calmonth") centWinMax

    ] where

        -- order of ratios: left-margin top-margin width height

        centWin     = (customFloating 
                      $ W.RationalRect (1/6) (1/6) (2/3) (2/3))

        centWinBig  = (customFloating 
                      $ W.RationalRect (1/8) (1/8) (3/4) (3/4))

        centWinMax  = (customFloating 
                      $ W.RationalRect (2/15) (1/20) (11/15) (9/10))

        centWinThin = (customFloating 
                      $ W.RationalRect (1/30) (1/4) (28/30) (1/2))

        centSquare  = (customFloating 
                      $ W.RationalRect (1/3) (1/4) (1/3) (1/2))

        lowerThird  = (customFloating 
                      $ W.RationalRect (0) (2/3) (1) (1/3))


-- Window rules:
------------------------------------------------------------------------
-- use xprop to id windows

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Skype"          --> doFloat
    , className =? "Onboard"        --> doIgnore
    , className =? "onboard"        --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    --, fullscreenManageHook
    , manageDocks
    ] <+> namedScratchpadManageHook myScratchpads


-- Event handling
------------------------------------------------------------------------

-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages & require
-- X.L.Fullscreen, hence the use of E.fullscreenEventHook and the 
-- XMonad.Layout.fullscreenEventHook below

myEventHook = E.ewmhDesktopsEventHook
    <+> E.fullscreenEventHook
    <+> fullscreenEventHook


-- Workspaces
------------------------------------------------------------------------

-- workspaces   = ["web", "irc", "code" ] ++ map show [4..9]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- irc, web, com, org, txt, wrk, img, dev, des
-- consider topic spaces
-- consider dynamic workspace modules
-- consider showWName


-- Layouts:
------------------------------------------------------------------------

-- my goal here is to have one main layout and one alternate layout per
-- workspace (e.g. tabbed and marginalia), with other layouts summoned
-- directly on an as desired basis
--
-- The addition of noBorders in addition to smartBorders on all layouts
-- is to address borders showing up on full screen windows while in 
-- multimonitor mode.
--
-- the tabs/simplest combo below doesn't need noBorders except in 
-- dual monitor spanned instances


-- Out of the myriad X.L.* modules, I'm choosing to focus on
-- XMonad.Layout.PerWorkspace
-- XMonad.Layout.CycleSelectedLayouts
-- XMonad.Layout.Combo
-- and XMonad.Layout.WindowNavigation,
-- as these seem to be updated and the most recent generation of
-- solution to the navigation and combined layout problem.
-- Keeping an eye out for 2DNavigation, which handles multimonitor
-- setups well (though I don't know yet if X.L.WindowNavigation.
--
-- There is also XMonad.Layout.LayoutCombinators to consider,
-- which has the useful JumpToLayout function, recreated below.
--
-- Other contenders in lieu of Combo: X.L.SubLayouts.
--
-- In terms of navigation: WindowNavigation and WindowArranger seem to
-- have some overlap and it might be worth trialing the latter.
--
-- Note that order of "addTabs" and "addBars" prior to setting
-- noBorders is intentional. Inverse causes problems.

------------------------------------------------------------------------
--myLayoutHook = showWName' mySWNConfig $
myLayoutHook = onWorkspaces ["exmaple","andanother"] 
                            (tabs ||| full ||| float)
               $ (tabs ||| tiledX ||| tiledXNude ||| full) where

    -- drawer is probably best used in conjunction with tagging the drawer window boring
    drawer      = renamed [Replace "Drawer"] $ leftDrawer `onLeft` tabs
                  where
                    leftDrawer = simpleDrawer 0.2 0.5 (Resource "drawer" `Or` ClassName "Drawer")

    tiledX      = renamed [Replace "Vert Tiled"] 
                $ dragBars $ noBorders $ Tall nmaster delta thirds

    tiledXNude  = renamed [Replace "Vert Tiled Nude"] 
                $ smartBorders $ Tall nmaster delta thirds

    tiledY      = renamed [Replace "Wide Tiled"] 
                $ smartBorders $ Mirror $ Tall nmaster delta halfs

    full        = renamed [Replace "Fullscreen"]
                $ noBorders $ Full

    float       = renamed [Replace "Floating"]
                $ smartBorders $ simpleFloat

    tabs        = renamed [Replace "Tabbed"] 
                $ dragTabs $ noBorders $ Simplest

    simpleTabs  = renamed [Replace "Simple Tabbed"] 
                $ addTabs $ noBorders $ Simplest

    addTabs l   = tabBar shrinkText myTheme Top 
                $ resizeVertical (fi $ decoHeight myTheme) $ l

    dragTabs l  = tabbedWindowSwitcherDecorationWithImageButtons shrinkText myThemeWithImageButtons (draggingVisualizer $ l)

    dragBars l  = windowSwitcherDecorationWithImageButtons shrinkText myThemeWithImageButtons (draggingVisualizer $ l)

    nmaster     = 1
    halfs       = 1/2
    thirds      = 1/3
    delta       = 3/100

cycleLayouts = sendMessage NextLayout
cycleAllLayouts = sendMessage NextLayout
cycleMainLayouts = cycleThroughLayouts ["Tabbed", "Vert Tiled" ]
cycleTiledLayouts = cycleThroughLayouts ["Vert Tiled", "Vert Tiled Nude"]
cycleAlternateLayouts = cycleThroughLayouts ["Tabbed", "Vert Tiled" ]
myJumpToLayout l = cycleThroughLayouts [l]
-- refresh layout to unfullscreen quickly and restore struts
-- TODO: make this a toggle function
fullScreen = cycleThroughLayouts ["Fullscreen"] >> (sendMessage $ SetStruts [] [minBound .. maxBound])


-- Interface
------------------------------------------------------------------------

-- Solarized Colors
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#719e07"

myFocusFollowsMouse         = False
myBorderWidth               = 1

-- intense
--myNormalBorderColor         = base02
--myFocusedBorderColor        = green

-- subdued
myNormalBorderColor         = base03
myFocusedBorderColor        = blue

myFontSize s                = "-*-terminus-medium-r-normal--" 
                              ++ show s ++ "-*-*-*-*-*-*-*"
myFontExtraBig              = myFontSize 24
myFontBig                   = myFontSize 16
myFont                      = myFontSize 14
myFontSmall                 = myFontSize 12
myFontExtraSmall            = myFontSize 10

myTheme :: Theme
myTheme = defaultTheme
    { activeColor           = base03
    , inactiveColor         = base02
    , urgentColor           = yellow
    , activeBorderColor     = base03
    , inactiveBorderColor   = base03
    , urgentBorderColor     = yellow
    , activeTextColor       = blue
    , inactiveTextColor     = base01
    , urgentTextColor       = base02
    , fontName              = myFont
    , decoHeight            = 22
    }

myBarTheme :: Theme
myBarTheme = myTheme
    { -- base00, base01, blue all good activeColors
      activeColor           = blue
    , activeTextColor       = base03
    }

myThemeWithImageButtons :: Theme
myThemeWithImageButtons = myBarTheme {
    windowTitleIcons = [ (closeButton, CenterLeft 6) ] }
--  windowTitleIcons = [ (menuButton, CenterLeft 6),
--      (closeButton, CenterRight 6)]
--      (maxiButton, CenterRight 18),
--      (miniButton, CenterRight 33) ]
--      }

convertToBool' :: [Int] -> [Bool]
convertToBool' = map (\x -> x == 1)

convertToBool :: [[Int]] -> [[Bool]]
convertToBool = map convertToBool'

menuButton' :: [[Int]]
menuButton' = [[1,1,1,1,1,1,1,1,1,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,1,1,1,1,1,1,1,1,1]]

menuButton :: [[Bool]]
menuButton = convertToBool menuButton'

miniButton' :: [[Int]]
miniButton' = [[0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1]]

miniButton :: [[Bool]]
miniButton = convertToBool miniButton'

maxiButton' :: [[Int]]
maxiButton' = [[1,1,1,1,1,1,1,1,1,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,1,1,1,1,1,1,1,1,1]]

maxiButton :: [[Bool]]
maxiButton = convertToBool maxiButton'

closeButton' :: [[Int]]
closeButton' = [[0,0,0,0,0,0,0,0,0,0],
                [0,1,0,0,0,0,0,0,1,0],
                [0,0,1,0,0,0,0,1,0,0],
                [0,0,0,1,0,0,1,0,0,0],
                [0,0,0,0,1,1,0,0,0,0],
                [0,0,0,0,1,1,0,0,0,0],
                [0,0,0,1,0,0,1,0,0,0],
                [0,0,1,0,0,0,0,1,0,0],
                [0,1,0,0,0,0,0,0,1,0],
                [0,0,0,0,0,0,0,0,0,0]]


closeButton :: [[Bool]]
closeButton = convertToBool closeButton' 

myPromptConfig :: XPConfig
myPromptConfig = defaultXPConfig
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = base00
    , fgHLight              = base1
    , bgHLight              = base02
    , borderColor           = base02
    , promptBorderWidth     = 1
    , height                = 22
    , autoComplete          = Just 500000
    }

mySWNConfig :: SWNConfig 
mySWNConfig = defaultSWNConfig 
    { swn_font    = myFontExtraBig
    , swn_bgcolor = base03
    , swn_color   = base00
    , swn_fade    = 1
    }


-- Status bars and logging
------------------------------------------------------------------------

myXmobar conf = statusBar "xmobar" myPP toggleStrutsKey conf

myPP = defaultPP
    { ppCurrent             = xmobarColor base02 blue . wrap " " " "
    , ppTitle               = xmobarColor blue "" . shorten 40
    , ppVisible             = wrap "(" ")"
    , ppUrgent              = xmobarColor base02 yellow . wrap " " " "
    , ppHidden              = id
    , ppHiddenNoWindows     = const ""
    , ppSep                 = " : "
    , ppWsSep               = " "
    , ppLayout              = id
    , ppOrder               = id
    , ppOutput              = putStrLn
    , ppSort                = fmap 
                              (namedScratchpadFilterOutWorkspace.)
                              (ppSort defaultPP)
    , ppExtras              = []
    }

--myLogHook = dynamicLogWithPP $ myPP


-- Main
------------------------------------------------------------------------

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main = xmonad =<< myXmobar (E.ewmh 
    $ withUrgencyHook NoUrgencyHook 
    $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    $ defaultConfig 
    { terminal              = myTerminal
    , focusFollowsMouse     = myFocusFollowsMouse
    , borderWidth           = myBorderWidth
    , modMask               = myModMask
    , workspaces            = myWorkspaces
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , layoutHook            = myLayoutHook
    , manageHook            = myManageHook
    --, handleEventHook       = myEventHook
    , startupHook           = myStartupHook
    }) where

    showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
    showKeybindings x = addName "Show Keybindings" $ io $ do
        --h <- spawnPipe "zenity --text-info"
        h <- spawnPipe $  "cat > ~/tmp/xmonadkeys.txt " 
                       ++ " && urxvtc -e less ~/tmp/xmonadkeys.txt"
        System.IO.UTF8.hPutStr h (unlines $ showKm x)
        hClose h
        return ()

