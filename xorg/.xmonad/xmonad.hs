-- xmonad config file
-- es@ethanschoonover.com

-- sources of inspiration and code:
-- https://github.com/thomasf/dotfiles-thomasf-xmonad/
-- https://github.com/league/dot-files/blob/master/xmonad.hs
-- https://github.com/pbrisbin/xmonad-config/

-- reference:
-- coding style - http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Developing.html#g:8


-- Imports
------------------------------------------------------------------------

import Data.List
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Exit(ExitCode(ExitSuccess), exitWith)
import System.IO
import qualified System.IO.UTF8
import XMonad hiding ((|||))
import XMonad.Actions.CycleSelectedLayouts
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import XMonad.Actions.Submap
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll
import XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Decoration
import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Simplest
import XMonad.Layout.TabBarDecoration
import XMonad.ManageHook
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.NamedActionsLocal
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

--import Text.Regex.Posix ((=~))

-- Note on fullscreen related modules
-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages and require
-- X.L.Fullscreen, so we import both and reference X.H.EwmhDesktops as E


-- Keyboard configuration:
------------------------------------------------------------------------

superMask = mod4Mask
altMask   = mod1Mask
myModMask = altMask

--myNewKeys conf@(XConfig {XMonad.modMask = modm}) =
--  [ subtitle "Application launching"
--
--
--
--  , subtitle "Toggle scratchpads and workspaces"
--  , ((modm, xK_section),                  addName "Toggle smaller terminal pad"                          $ smallTerminalPad >> movePointer)
--  , ((modm.|.controlMask, xK_section),    addName "Toggle larger terminal pad"                           $ largeTerminalPad >> movePointer)
--  , ((modm, xK_1),                        addName "Toggle home workspace"                                $ rmEmptyWs $ myViewWS "home" >> movePointer)
--  , ((modm, xK_2),                        addName "Toggle chat workspace"                                $ rmEmptyWs $ myViewWS "chat" >> movePointer)
--  , ((modm, xK_3),                        addName "Toggle nodes workspace"                               $ rmEmptyWs $ myViewWS "nodes" >> movePointer)
--  , ((modm, xK_4),                        addName "Toggle mail workspace"                                $ rmEmptyWs $ myViewWS "mail" >> movePointer)
--  , ((modm, xK_0),                        addName "Toggle dashboard workspace"                           $ rmEmptyWs $ myViewWS "dash" >> movePointer)
--
--  ] where
--
--    -- | Run script with same name as "w.workspacename"
--    workspaceAction = do
--      ws <- gets (W.currentTag . windowset)
--      spawn ("w." ++ takeWhile (/='.') ws)
--
--    -- | Run script with same name as "w.workspacename" if the workspace is empty
--    maybeWorkspaceAction = do
--      wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
--      when (null wins) $ workspaceAction
--
--    -- | Remove current workpace if empty
--    rmEmptyWs = DW.removeEmptyWorkspaceAfterExcept [ "NSP", "home", "nodes", "dash"]
--
--    -- | Toggle recent workspaces ignoring some of them
--    toggleWS = toggleWS' [ "NSP" ] >> movePointer
--
--    -- | Toggle recent workspaces ignoring some of them
--    ignoredToggleWS = toggleWS' [ "NSP"
--                                , "home", "nodes", "dash", "mail"
--                                , "chat", "im" ] >> movePointer
--
--    -- | View a workspace by name
--    myViewWS wsid = do
--      DW.addHiddenWorkspace wsid
--      windows (W.greedyView wsid)
--      maybeWorkspaceAction
--
--    -- | Select workspae prompt
--    selectWorkspacePrompt = workspacePrompt myXPConfig $ \w ->
--                            do s <- gets windowset
--                               if W.tagMember w s
--                                 then windows $ W.view w
--                                 else DW.addWorkspace w
--
--    --  | Open small terminal pad
--    smallTerminalPad = namedScratchpadAction myScratchPads "smallTerminal"
--
--    -- | Open larger terminal pad
--    largeTerminalPad = namedScratchpadAction myScratchPads "largeTerminal"
--
--    -- | Select next non empty workspace
--    nextWsNonEmpty = windows . W.greedyView
--                     =<< findWorkspace getSortByTagNoSP Next HiddenNonEmptyWS 1
--
--    -- | Select previous non empty workspace
--    prevWsNonEmpty = windows . W.greedyView
--                     =<< findWorkspace getSortByTagNoSP Prev HiddenNonEmptyWS 1
--
--    -- |  Select next workspace with same prefix
--    nextWsPrefix = windows . W.greedyView
--                   =<< findWorkspace getSortByTagNoSP Next (HiddenWSTagGroup '.') 1
--
--    -- | Select previous workspac with same prefix
--    prevWsPrefix = windows . W.greedyView
--                   =<< findWorkspace getSortByTagNoSP Prev (HiddenWSTagGroup '.') 1
--
--    -- | Sort workspaces by tag name, exclude hidden scrachpad workspace.
--    getSortByTagNoSP = fmap (.namedScratchpadFilterOutWorkspace) getSortByTag


myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf@(XConfig {XMonad.modMask = modm}) =

    [ subtitle "APPLICATIONS"
    , ((superMask, xK_Return),              addName "New terminal"                                         newTerminal)
    , ((superMask, xK_b),                   addName "New browser"                                          newBrowser)
    , ((superMask .|. shiftMask, xK_b),     addName "Next existing browser"                                nextBrowser)
    , ((superMask, xK_m),                   addName "Show mail"                                            showMail)
    , ((superMask, xK_p),                   addName "Show web contacts"                                    showWebContacts)
    , ((superMask, xK_c),                   addName "Show calendar"                                        showCalendar)
    --, ((superMask, xK_t),                 addName "Show tasks"                                           showTasks)
    , ((superMask, xK_d),                   addName "Show Google drive"                                    showDrive)
    , ((superMask, xK_v),                   addName "New Vim"                                              newVim)
    , ((superMask .|. shiftMask, xK_v),     addName "Next Vim"                                             nextVim)

    , subtitle "KILL & QUIT"
    , ((modm, xK_BackSpace),                addName "Close the focused window"                             kill)
    , ((modm .|. shiftMask, xK_BackSpace),  addName "Close all workspace windows"                          killAll)
    , ((modm              , xK_q     ),     addName "Restart XMonad"                                       myRestart)
    , ((modm .|. shiftMask, xK_q     ),     addName "Quit XMonad"                                          myQuit)

    , subtitle "WINDOWS CYCLING (J/K) [+=focus] [+ctrl=keep focus] [+shift=move]"
    , ((modm, xK_j),                        addName "Focus next window on workspace"                       $ windows W.focusDown)
    , ((modm, xK_k),                        addName "Focus previous window on workspace"                   $ windows W.focusUp)
    , ((modm .|. shiftMask, xK_j),          addName "Swap focused with next on workspace"                  $ windows W.swapUp)
    , ((modm .|. shiftMask, xK_k),          addName "Swap focused with previous on workspace"              $ windows W.swapDown)
    , ((modm .|. controlMask, xK_j),        addName "Rotate windows clockwise, keeping focus"              $ rotAllUp)
    , ((modm .|. controlMask, xK_k),        addName "Rotate windows counter clockwise, keeping focus"      $ rotAllDown)

    , subtitle "WINDOW ACTIONS"
    , ((modm, xK_m),                        addName "Move focus to master window"                          $ windows W.focusMaster)
    , ((modm .|. shiftMask, xK_m),          addName "Swap the focused window and the master window"        $ windows W.swapMaster)
    , ((modm, xK_t),                        addName "Push the window into tiling mode"                     $ withFocused (windows . W.sink))
    , ((modm, xK_u),                        addName "Focus urgent winow"                                   $ focusUrgent)
    , ((modm .|. controlMask, xK_u),        addName "Clear all urgent window statuses"                     $ clearUrgents)

    , subtitle "DISPLAYS CYCLING (D/U) [+=select] [+ctrl=swap] [+shift=move window to]"
    , ((modm, xK_d),                        addName "Next screen"                                          $ nextScreen >> moveCursor)
    , ((modm, xK_u),                        addName "Previous screen"                                      $ prevScreen >> moveCursor)
    , ((modm .|. controlMask, xK_d),        addName "Swap current display witn next"                       $ swapNextScreen >> nextScreen >> moveCursor)
    , ((modm .|. controlMask, xK_u),        addName "Swap current display witn previous"                   $ swapPrevScreen >> nextScreen >> moveCursor)
    , ((modm .|. shiftMask, xK_d),          addName "Move window to next screen"                           $ shiftNextScreen >> nextScreen >> moveCursor)
    , ((modm .|. shiftMask, xK_u),          addName "Move window to previous screen"                       $ shiftPrevScreen >> prevScreen >> moveCursor)

    , subtitle "WORKSPACE ACTIONS (N/P) [mod=from prefix] [mod+ctrl=from all]"
--  , ((modm, xK_n),                        addName "Next workspace (prefix)"                              $ rmEmptyWs $ nextWsPrefix)
--  , ((modm, xK_p),                        addName "Previous workspace (prefix)"                          $ rmEmptyWs $ prevWsPrefix)
--  , ((modm.|. controlMask, xK_e),         addName "Next non empty workspace"                             $ rmEmptyWs $ nextWsNonEmpty)
--  , ((modm.|. controlMask, xK_r),         addName "Previous non empty workspace"                         $ rmEmptyWs $ prevWsNonEmpty)
--  , separator
--  , ((modm, xK_w),                        addName "Toggle previous workspace"                            $ rmEmptyWs $ toggleWS)
--  , ((modm.|. controlMask, xK_w),         addName "Toggle previous workspace skipping some workspaces"   $ rmEmptyWs $ ignoredToggleWS)
--  , ((modm, xK_q),                        addName "Run default workspace launcer script"                 $ workspaceAction)
--
--  , subtitle "Workspace prompts"
--  , ((modm, xK_n),                        addName "Create or change workspace prompt"                    $ rmEmptyWs $ selectWorkspacePrompt >> maybeWorkspaceAction >> movePointer)
--  , ((modm.|. shiftMask, xK_n),           addName "Move window to other workspace prompt"                $ DW.withWorkspace myXPConfig (windows . W.shift) >> movePointer)
--  , ((modm.|. controlMask, xK_n),         addName "Rename current workspace"                             $ DW.renameWorkspace myXPConfig >> movePointer)
--  , ((modm.|. controlMask, xK_BackSpace), addName "Remove current workspace"                             $ DW.removeWorkspace >> movePointer)
--  , ((modm, xK_o),                        addName "Goto workspace by window search prompt"               $ gotoMenuArgs ["-l 23"] >> movePointer)

    , subtitle "WORKSPACE LAYOUTS (H/L=size ,.=) [+alt=toggle]"
--  , ((modm, xK_space),                    addName "Switch to next window layout"                         $ sendMessage NextLayout)
    , ((modm, xK_space),                    addName "Switch to next window layout"                         cycleLayouts)
    , ((modm .|. shiftMask, xK_space ),     addName "Reset to default layout"                              $ setLayout $ XMonad.layoutHook conf)
    , ((modm .|. controlMask, xK_space),    addName "Refresh layout"                                       refresh)
--  , ((modm.|. altMask, xK_space),         addName "Toggle fullscreen"                                    $ sendMessage (MT.Toggle MTI.NBFULL))
--  , ((modm.|. altMask, xK_s),             addName "Toggle struts (ignore panels)"                        $ sendMessage ToggleStruts)
--  , ((modm.|. altMask, xK_b),             addName "Toggle window borders"                                $ sendMessage (MT.Toggle MTI.NOBORDERS))
    , ((modm, xK_h),                        addName "Shrink the master area"                               $ sendMessage Shrink)
    , ((modm, xK_l),                        addName "Expand the master area"                               $ sendMessage Expand)
    , ((modm, xK_comma),                    addName "Increment the number of windows in the master area"   $ sendMessage (IncMasterN 1))
    , ((modm, xK_period),                   addName "Deincrement the number of windows in the master area" $ sendMessage (IncMasterN (-1)))

    , subtitle "test submap" 
    , ((modm, xK_x), submapName $ 
    [ ((0, xK_o),                           addName "test submap"                                          $ sendMessage Shrink)
    , ((0, xK_z),                           addName "test submap"                                          $ sendMessage Shrink)]
    )

    -- c.f. http://hackage.haskell.org/cgi-bin/hackage-scripts/package/X11
    , subtitle "MEDIA KEYS" 
    , ((0, xF86XK_AudioRaiseVolume),                 addName "Volume up by 1"                              $ spawn "volume up")
    , ((0, xF86XK_AudioLowerVolume),                 addName "Volume down by 1"                            $ spawn "volume down")
    , separator
    , ((0 .|. shiftMask, xF86XK_AudioRaiseVolume),   addName "Volume up by 10"                             $ spawn "volume up 10")
    , ((0 .|. shiftMask, xF86XK_AudioLowerVolume),   addName "Volume down by 10"                           $ spawn "volume down 10")
    , separator
    , ((0 .|. controlMask, xF86XK_AudioLowerVolume), addName "Volume at 50%"                               $ spawn "volume mid")
    , ((0 .|. controlMask, xF86XK_AudioRaiseVolume), addName "Volume at maximum"                           $ spawn "volume max")
    , ((0, xF86XK_AudioMute),                        addName "Volume mute toggle"                          $ spawn "volume toggle")

    , subtitle "HOT KEYS" 
    , ((0, 0x1008ff93),                              addName "Toggle min/max power modes"                  $ spawn "power toggle") -- XF86Battery button
    , ((0 .|. shiftMask, 0x1008ff93),                addName "Toggle miv/mov power modes"                  $ spawn "power toggle pinned")
    , ((0, xF86XK_Sleep),                            addName "System sleep"                                $ spawn "system sleep")
    , ((0, xF86XK_PowerOff),                         addName "System power off"                            $ spawn "system off")
    , ((0 .|. shiftMask, xF86XK_PowerOff),           addName "System reboot"                               $ spawn "system reboot")
    , ((0, xF86XK_ScreenSaver),                      addName "Lock screen"                                 $ spawn "display lock")
    , ((0, xF86XK_Display),                          addName "Cycle display mode"                          $ spawn "display cycle")
    , ((0, xF86XK_Launch1),                          addName "Bluetooth toggle"                            $ spawn "wireless bluetooth toggle")
    , ((0, xK_Print),                                addName "Screen annotation - start/finish"            $ spawn "screendraw")
    , ((0 .|. shiftMask, xK_Print),                  addName "Screen annotation - force finish"            $ spawn "screendraw finish")
    , ((modm, xK_Print),                             addName "Screen annotation - cancel"                  $ spawn "screendraw cancel")
    , ((0, xF86XK_RotateWindows),                    addName "Screen annotation - clear"                   $ spawn "screendraw clear")
    --, ((0, 0x1008ffa7),                            addName "System Reboot"                               $ spawn "system reboot") -- XF86Suspend button
    ]

    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    ++
    subtitle "WORKSPACE SWITCHING":
    [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
        | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shiftMask, "Move client to workspace ")]
        , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3

    ++
    subtitle "SCREEN SWITCHING":
    [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shiftMask, "Move client to screen number ")]
        , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

        where

            -- | Move mouse cursor to bottom right of the current window
            moveCursor = updatePointer (Relative 0.99 0.99)

        --	cycleLayouts		= cycleThroughLayouts
        --				[ "Tab", "Marginalia"]
        --	cycleMax		= cycleThroughLayouts ["MadMax", "Tab"]
        --	printScreenFull		= do
        --				  spawn $ "/usr/local/bin/scrotupload"
        --				  showAlert "Screen Captured"
        --	testAlert		= showAlert "Test"
        --	printScreenWin		= spawn
        --				$ "import -window `xwininfo"
        --				++" | grep 'Window id:' |cut -d\" \" -f4` "
        --				++"screenshot_w_$(date +%Y%m%d_%H%M%S).png"
        -- 	printScreenWin		= spawn
        -- 				$ "import -window `xwininfo"
        -- 				++" | grep 'Window id:' |cut -d\" \" -f4` "
        -- 				++"screenshot_w_$(date +%Y%m%d_%H%M%S).png"
        -- 	recordScreenFullSilent	= spawn
        -- 				$ "recordmydesktop "
        -- 				++"-stop-shortcut \"Control+Print\" "
        -- 				++"--no-sound"
        --	recordScreenFull	= spawn $ "gtk-recordMyDesktop"
        --  videoSwitch                = spawn $ "video-switch"
        --  recordScreenFull           = spawn $ "xvidcap"
        --  banishCursor :: X ()
        --  banishCursor               = warpToWindow 1 1
        --  doJump ws                  = windows (W.view ws)
        --  layoutMaximize             = do
        --                             sendMessage $ JumpToLayout "MadMax"
        --                             spawn $ "transset-df -a -n 'statusBarL' -t 0.0"
        --                             spawn $ (
        --                             "transset-df -a -n 'statusBarR' -v -t 0.0 "
        --                             ++"| if grep -q 'Set Property to 1'; "
        --                             ++"then transset-df -a -n 'stalonetray' 0.5; "
        --                             ++"else transset-df -a -n 'stalonetray' 0.0; "
        --                             ++"fi" )
        --  manageDisplays = runOrRaise "arandr" (title =? "Screen Layout Editor")


-- Applications
------------------------------------------------------------------------

myTerminal         = "urxvtc"
myTerminalInit     = "pgrep urxvtd || urxvtd -f -o -q"
newTerminal :: X ()
newTerminal        = spawn myTerminal

newVim :: X ()
newVim             = runInTerm "" "vim"
nextVim            = raiseNextMaybe (runInTerm "" "vim") (title =? "vim")

showMail           = raiseMaybe (runInTerm "" "mutt") (title =? "mutt")
showCLICalendar    = "urxvtc -e gcal view week"

chromeClass        = "Chromium"
chromeBase         = "chromium --memory-model=low"
                   ++ "     --enable-print-preview"
                   ++ "     --enable-smooth-scrolling"
                   ++ "     --enable-sync-extensions"
                   ++ "     --enable-webgl"
                   ++ "     --ignore-gpu-blacklist"
chrome             = chromeBase ++ " --class=Chromium --name=chromium"

newBrowser :: X ()
newBrowser         = spawn $ chrome
nextBrowser        = raiseNextMaybe (spawn $ chrome) (className =? chromeClass)

showWebApp :: String -> String -> X ()
showWebApp u m     = raiseNextMaybe
                     (spawn $ (appBaseCmdURL $ u) ) 
                     (className =? "ChromiumAppMode" <&&> (fmap (\t -> isPrefixOf m t || isSuffixOf m t) title)) 

newWebApp :: String -> X ()
newWebApp u        = spawn $ (appBaseCmdURL $ u) 

appBaseCmdURL :: String -> String
appBaseCmdURL u    = chromeBase
                   ++ " --class=ChromiumAppMode"
                   ++ " --name==chromiumappmode"
                   ++ " --user-data-dir=/home/es/.config/chromium-app-mode"
                   ++ " --app=" ++ u

showWebMail        = showWebApp "https://mail.google.com" "Ethan Schoonover Mail"
showCalendar       = showWebApp "https://calendar.google.com" "Ethan Schoonover - Calendar"
showWebContacts    = showWebApp "https://www.google.com/contacts" "Google Contacts"
showDrive          = showWebApp "https://drive.google.com" "Google Drive"
newDrive           = newWebApp  "https://drive.google.com" 
showNews           = showWebApp "https://reader.google.com" "Google Reader"
showTasks          = showWebApp "https://astrid.com" "Astrid"
showVault          = showWebApp "chrome-extension://hdokiejnpimakedhajhdlcegeplioahd/homelocal.html" "My LastPass Vault"

startCoreApps :: X ()
startCoreApps      = do
                     showNews
                     showDrive
                     showWebContacts
                     showTasks
                     showCalendar
                     showMail
                     showVault

mySystemTrayInit   = "systray &"
mySystemTrayKill   = "trayer"

myCompositorInit   = "compton -f -D 6 -m 0.95 &" -- "xcompmgr -f -D 6 &"
myCompositorKill   = "compton"


myNotifierInit     = "dunst &"
myNotifierKill     = "dunst"

myStatusBarInit    = ""
myStatusBarKill    = "xmobar"


-- Scratch Pads
------------------------------------------------------------------------


-- Startup & Restarting
------------------------------------------------------------------------

myStartupHook = do
    E.ewmhDesktopsStartup
    spawn $ mySystemTrayInit
    spawn $ myCompositorInit
    spawn $ myNotifierInit
    spawn $ myTerminalInit

killProcess :: String -> String
killProcess p = "for pid in `pgrep " ++ p ++ "`; do kill -9 $pid; done && "

myRestart :: X ()
myRestart = spawn 
    $  killProcess myStatusBarKill
    ++ killProcess mySystemTrayKill
    ++ killProcess myCompositorKill
    ++ killProcess myNotifierKill
    ++ "xmonad --recompile && xmonad --restart"

myQuit = do
    io (exitWith ExitSuccess)


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
    , fullscreenManageHook
    , manageDocks
    ]


-- Event handling
------------------------------------------------------------------------

-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages and require
-- X.L.Fullscreen, hence the use of E.fullscreenEventHook and the 
-- XMonad.Layout.fullscreenEventHook below
myEventHook   = E.ewmhDesktopsEventHook
    <+> E.fullscreenEventHook
    <+> fullscreenEventHook


-- Layouts:
------------------------------------------------------------------------

-- The addition of noBorders in addition to smartBorders on all layouts
-- is to address borders showing up on full screen windows while in 
-- multimonitor mode.

myLayoutHook = smartBorders $ (tabs ||| tiledX ||| tiledY ||| full ||| float) where
    tiledX       = named "Tiled Tall" $ Tall nmaster delta thirds
    tiledY       = named "Tiled Wide" $ Mirror $ Tall nmaster delta halfs
    --full       = named "Fullscreen" $ noBorders $ Full
    full         = named "Fullscreen" $ Full
    float        = named "Floating" $ simpleFloat
    nmaster      = 1
    halfs        = 1/2
    thirds       = 1/3
    delta        = 3/100
    -- the tabs/simplest combo below doesn't need noBorders except in dual monitor spanned instances
    tabs         = named "Tabbed" $ makeTab $ noBorders $ Simplest
    makeTab l    = tabBar shrinkText myTabTheme Top $ resizeVertical (fi $ decoHeight myTabTheme) $ l

cycleLayouts = cycleThroughLayouts ["Tabbed", "Tiled Tall" ]
cycleMainLayouts = cycleThroughLayouts ["Tabbed", "Tiled Tall" ]
--cycleMax = cycleThroughLayouts ["MadMax", "Tab"]


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


myFocusFollowsMouse    = False
myBorderWidth          = 1

-- workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--myWorkspaces                = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces = [ "000", "WWW.1", "WRK", "TXT", "ORG" ]

myNormalBorderColor    = base02
myFocusedBorderColor   = green

--myFontSize s         = "xft:lettergothicmono:style=regular:pixelsize=" ++ show s
--myFontSize s         = "xft:Terminus:style=Medium:pixelsize=" ++ show s
myFontSize s           = "-*-terminus-medium-r-normal--" ++ show s ++ "-*-*-*-*-*-*-*"
myFontBig              = myFontSize 16
myFont                 = myFontSize 14
myFontSmall            = myFontSize 12

myTabTheme :: Theme
myTabTheme = defaultTheme
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

myPromptConfig :: XPConfig
myPromptConfig = defaultXPConfig
    { font                  = "xft:Terminus:style=Medium:pixelsize=18"
    , bgColor		    = base02
    , fgColor               = base0
    , fgHLight		    = base2
    , bgHLight              = base03
    , borderColor           = base03
    , promptBorderWidth     = 2
    , height                = 26
    , autoComplete          = Just 500000
    }


-- Status bars and logging
------------------------------------------------------------------------

myXmobar conf = statusBar "xmobar" myPP toggleStrutsKey conf

myPP = defaultPP
    { ppCurrent = xmobarColor base02 blue . wrap " " " "
    , ppTitle   = xmobarColor blue "" . shorten 40
    , ppVisible = wrap "(" ")"
    , ppUrgent  = xmobarColor base02 yellow . wrap " " " "
    , ppHidden          = id
    , ppHiddenNoWindows = const ""
    , ppSep             = " : "
    , ppWsSep           = " "
    , ppLayout          = id
    , ppOrder           = id
    , ppOutput          = putStrLn
  --, ppSort            = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
  --, ppSort            = fmap (namedScratchpadFilterOutWorkspace.) (ppSort defaultPP)
  --, ppSort   = do
  --xsort <- getSortByXineramaRule
  --return (xsort . namedScratchpadFilterOutWorkspace)
    , ppExtras          = []
    }

--myLogHook = dynamicLogWithPP $ myPP


-- Main
------------------------------------------------------------------------

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main = xmonad =<< myXmobar (E.ewmh 
    $ withUrgencyHook NoUrgencyHook 
 -- $ withScratchpads scratchpads 
    $ addDescrKeys' ((superMask, xK_F1), showKeybindings) myKeys
    $ defaultConfig 
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
  --, workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
  --, mouseBindings      = myMouseBindings
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , startupHook        = myStartupHook
    }) where

       showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
       showKeybindings x = addName "Show Keybindings" $ io $ do
           --h <- spawnPipe "zenity --text-info"
           h <- spawnPipe "cat > ~/tmp/xmonadkeys.txt && urxvtc -e less ~/tmp/xmonadkeys.txt"
           System.IO.UTF8.hPutStr h (unlines $ showKm x)
           hClose h
           return ()

