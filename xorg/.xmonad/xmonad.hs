-- xmonad config file
-- es@ethanschoonover.com

-- sources of inspiration and code:
-- https://github.com/thomasf/dotfiles-thomasf-xmonad/
-- https://github.com/league/dot-files/blob/master/xmonad.hs
-- https://github.com/pbrisbin/xmonad-config/

-- reference:
-- coding style - http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Developing.html#g:8

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

import Data.List
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Exit(ExitCode(ExitSuccess), exitWith)
import System.IO
import qualified System.IO.UTF8
import XMonad
import XMonad.Actions.RotSlaves
import XMonad.Actions.Submap
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.ManageHook
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.NamedActionsLocal
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import XMonad.Actions.WithAll
import XMonad.Actions.WindowGo

import XMonad.Layout.NoBorders
import XMonad.Layout.Decoration
import XMonad.Layout.Named
import XMonad.Layout.Simplest
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.SimpleFloat

--import XMonad.Layout.Decoration

-- Note on fullscreen related modules
-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages and require
-- X.L.Fullscreen, so we import both and reference X.H.EwmhDesktops as E

------------------------------------------------------------------------
-- Keyboard configuration:
------------------------------------------------------------------------

superMask = mod4Mask
altMask   = mod1Mask
myModMask = altMask

--myNewKeys conf@(XConfig {XMonad.modMask = modm}) =
--  [ subtitle "Application launching"
--  , ((modm .|. shiftMask, xK_Return),     addName "Launch Terminal"                                      $ spawn $ XMonad.terminal conf)
--
--  , subtitle "Cyclic window actions (J/K) [+=focus] [+control=cycle+keep focus] [+shift=move]"
--  , ((modm, xK_j),                        addName "Focus next window on workspace"                       $ windows W.focusDown >> movePointer)
--  , ((modm, xK_k),                        addName "Focus previous window on workspace"                   $ windows W.focusUp >> movePointer)
--  , ((modm.|. shiftMask, xK_j),           addName "Swap focused with next on workspace"                  $ windows W.swapDown >> movePointer)
--  , ((modm.|. shiftMask, xK_k),           addName "Swap focused with previous on workspace"              $ windows W.swapUp >> movePointer)
--  , ((modm.|. controlMask, xK_j),         addName "Rotate all windows forward while keeping focus"       $ rotAllUp >> movePointer)
--  , ((modm.|. controlMask, xK_k),         addName "Rotate all windows backwards while keeping focus"     $ rotAllDown >> movePointer)
--
--  , subtitle "Other window actions"
--  , ((modm, xK_m),                        addName "Move focus to master window"                          $ windows W.focusMaster >> movePointer)
--  , ((modm, xK_Return),                   addName "Swap the focused window and the master window"        $ windows W.swapMaster >> movePointer)
--  , ((modm, xK_t),                        addName "Push the window into tiling mode"                     $ withFocused (windows . W.sink) >> movePointer)
--  , ((modm.|. controlMask, xK_c),         addName "kill"                                                 $ kill)
--  , ((modm, xK_u),                        addName "Focus urgent winow"                                   $ focusUrgent >> movePointer)
--  , ((modm.|. controlMask, xK_u),         addName "Clear all urgent window statuses"                     $ clearUrgents)
--
--  , subtitle "Cyclic display actions (D/F) [+=select] [+control=swap] [+shift=move window to]"
--  , ((modm, xK_d),                        addName "Next screen"                                          $ nextScreen >> movePointer)
--  , ((modm, xK_f),                        addName "Previous screen"                                      $ prevScreen >> movePointer)
--  , ((modm.|. controlMask, xK_d),         addName "Swap current display witn next"                       $ swapNextScreen >> nextScreen >> movePointer)
--  , ((modm.|. controlMask, xK_f),         addName "Swap current display witn previous"                   $ swapPrevScreen >> nextScreen >> movePointer)
--  , ((modm.|. shiftMask, xK_d),           addName "Move window to next screen"                           $ shiftNextScreen >> nextScreen >> movePointer)
--  , ((modm.|. shiftMask, xK_f),           addName "Move window to previous screen"                       $ shiftPrevScreen >> prevScreen >> movePointer)
--
--  , subtitle "Workspace actions (E/R) [mod=select from prefix] [mod+control=select from all]"
--  , ((modm, xK_e),                        addName "Next workspace (prefix)"                              $ rmEmptyWs $ nextWsPrefix >> movePointer)
--  , ((modm, xK_r),                        addName "Previous workspace (prefix)"                          $ rmEmptyWs $ prevWsPrefix >> movePointer)
--  , ((modm.|. controlMask, xK_e),         addName "Next non empty workspace"                             $ rmEmptyWs $ nextWsNonEmpty >> movePointer)
--  , ((modm.|. controlMask, xK_r),         addName "Previous non empty workspace"                         $ rmEmptyWs $ prevWsNonEmpty >> movePointer)
--
--  , subtitle "Other workspace actions"
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
--
--  , subtitle "Modify current workspace layout... (H/L=size ,.=) [+alt=toggle]"
--  , ((modm, xK_space),                    addName "Switch to the next window layout"                     $ sendMessage NextLayout >> movePointer)
--  , ((modm.|. controlMask, xK_space),     addName "Switch to default layout"                             $ sendMessage (JumpToLayout "tall h") >> movePointer)
--  , ((modm.|. altMask, xK_space),         addName "Toggle fullscreen"                                    $ sendMessage (MT.Toggle MTI.NBFULL) >> movePointer)
--  , ((modm.|. altMask, xK_s),             addName "Toggle struts (ignore panels)"                        $ sendMessage ToggleStruts >> movePointer)
--  , ((modm.|. altMask, xK_b),             addName "Toggle window borders"                                $ sendMessage (MT.Toggle MTI.NOBORDERS) >> movePointer)
--  , ((modm, xK_h),                        addName "Shrink the master area"                               $ sendMessage Shrink >> movePointer)
--  , ((modm, xK_l),                        addName "Expand the master area"                               $ sendMessage Expand >> movePointer)
--  , ((modm, xK_comma),                    addName "Increment the number of windows in the master area"   $ sendMessage (IncMasterN 1) >> movePointer)
--  , ((modm, xK_period),                   addName "Deincrement the number of windows in the master area" $ sendMessage (IncMasterN (-1)) >> movePointer)
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
--    -- | Move mouse pointer to bottom right of the current window
--    movePointer = updatePointer (Relative 0.99 0.99)
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


--myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf@(XConfig {XMonad.modMask = modm}) =

    [ subtitle "APPLICATIONS"
    , ((superMask, xK_Return),              addName "Launch Terminal"                                      $ spawn $ XMonad.terminal conf)
    , ((superMask, xK_b),                   addName "Browser - Chrome"                                     $ newBrowser)
    , ((superMask.|. shiftMask, xK_b),      addName "Browser - Chrome"                                     $ nextBrowser)
    , ((superMask, xK_m),                   addName "Mail - Mutt"                                          $ spawn myMailApp)
    , ((superMask, xK_c),                   addName "Calendar - gcalcli"                                   $ spawn myCalendarApp)
    --, ((superMask, xK_t),                 addName "Tasks - Astrid"                                       $ spawn myTasksApp)
    , ((superMask, xK_d),                   addName "Docs - Google"                                        $ spawn myDocsApp)
    , ((superMask, xK_v),                   addName "Vim"                                                  $ spawn myTextEditorApp)

    , subtitle "KILL"
    , ((modm, xK_BackSpace),                addName "Close the focused window"                             kill)
    , ((modm .|. shiftMask, xK_BackSpace),  addName "Close all workspace windows"                          killAll)

    , subtitle "CYCLE WINDOWS (J/K) [+=focus] [+ctrl=keep focus] [+shift=move]"
    , ((modm, xK_j),                        addName "Focus next window on workspace"                       $ windows W.focusDown)
    , ((modm, xK_k),                        addName "Focus previous window on workspace"                   $ windows W.focusUp)
    , ((modm.|. shiftMask, xK_j),           addName "Swap focused with next on workspace"                  $ windows W.swapUp)
    , ((modm.|. shiftMask, xK_k),           addName "Swap focused with previous on workspace"              $ windows W.swapDown)
    , ((modm.|. controlMask, xK_j),         addName "Rotate windows clockwise, keeping focus"              $ rotAllUp)
    , ((modm.|. controlMask, xK_k),         addName "Rotate windows counter clockwise, keeping focus"      $ rotAllDown)

--  , subtitle "Other window actions"
--  , ((modm, xK_m),                        addName "Move focus to master window"                          $ windows W.focusMaster >> movePointer)
--  , ((modm, xK_Return),                   addName "Swap the focused window and the master window"        $ windows W.swapMaster >> movePointer)
--  , ((modm, xK_t),                        addName "Push the window into tiling mode"                     $ withFocused (windows . W.sink) >> movePointer)
--  , ((modm, xK_u),                        addName "Focus urgent winow"                                   $ focusUrgent >> movePointer)
--  , ((modm.|. controlMask, xK_u),         addName "Clear all urgent window statuses"                     $ clearUrgents)

    , subtitle "test submap" 
    , ((modm, xK_x), submapName $ 
    [ ((0, xK_o),                           addName "test submap"                                          $ sendMessage Shrink)
    , ((0, xK_z),                           addName "test submap"                                          $ sendMessage Shrink)]
    )

    -- c.f. http://hackage.haskell.org/cgi-bin/hackage-scripts/package/X11
    , subtitle "MEDIA" 
    , ((0, xF86XK_AudioRaiseVolume),                 addName "Volume up by 1"                              $ spawn "volume up")
    , ((0, xF86XK_AudioLowerVolume),                 addName "Volume down by 1"                            $ spawn "volume down")
    , separator
    , ((0 .|. shiftMask, xF86XK_AudioRaiseVolume),   addName "Volume up by 10"                             $ spawn "volume up 10")
    , ((0 .|. shiftMask, xF86XK_AudioLowerVolume),   addName "Volume down by 10"                           $ spawn "volume down 10")
    , separator
    , ((0 .|. controlMask, xF86XK_AudioLowerVolume), addName "Volume at 50%"                               $ spawn "volume mid")
    , ((0 .|. controlMask, xF86XK_AudioRaiseVolume), addName "Volume at maximum"                           $ spawn "volume max")
    , ((0, xF86XK_AudioMute),                        addName "Volume mute toggle"                          $ spawn "volume toggle")

    , subtitle "LAYOUTS"
    , ((modm,               xK_space ), sendMessage' NextLayout) -- %! Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), addName "Reset the layout" $ setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , separator
    , ((modm,               xK_n     ), addName "Refresh" refresh) -- %! Resize viewed windows to the correct size

--  , subtitle "move focus up or down the window stack"
--  , ((modm,               xK_Tab   ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
--  , ((modm .|. shiftMask, xK_Tab   ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
--  , ((modm,               xK_j     ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
--  , ((modm,               xK_k     ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
--  , ((modm,               xK_m     ), addName "Focus the master" $ windows W.focusMaster  ) -- %! Move focus to the master window

--    , subtitle "modifying the window order"
--    , ((modm,               xK_Return), addName "Swap with the master" $ windows W.swapMaster) -- %! Swap the focused window and the master window
--    , ((modm .|. shiftMask, xK_j     ), addName "Swap down" $ windows W.swapDown  ) -- %! Swap the focused window with the next window
--    , ((modm .|. shiftMask, xK_k     ), addName "Swap up"   $ windows W.swapUp    ) -- %! Swap the focused window with the previous window

    , subtitle "resizing the master/slave ratio"
    , ((modm,               xK_h     ), addName "Shrink the master area" $ sendMessage Shrink)
    , ((modm,               xK_l     ), addName "Expand the master area" $ sendMessage Expand)

    , subtitle "floating layer support"
    , ((modm,               xK_t     ), addName "Push floating to tiled" $ withFocused $ windows . W.sink)

    , subtitle "change the number of windows in the master area"
    , ((modm              , xK_comma ), addName "Increment the number of windows in the master area" $ sendMessage (IncMasterN 1))
    , ((modm              , xK_period), addName "Deincrement the number of windows in the master area" $ sendMessage (IncMasterN (-1)))

    , subtitle "quit, or restart"
    , ((modm .|. shiftMask, xK_q     ), addName "Quit" $ io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modm              , xK_q     ), addName "Restart" $ spawn "xmonad --recompile && xmonad --restart") -- %! Restart xmonad
    ]

    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    ++
    subtitle "switching workspaces":
    [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
        | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shiftMask, "Move client to workspace ")]
        , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3

    ++
    subtitle "switching screens" :
    [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shiftMask, "Move client to screen number ")]
        , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

        where

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
            nextBrowser = runOrRaiseNext chrome (className =? chromeClass )
            newBrowser :: X ()
            newBrowser = spawn $ chrome
            kickstartApps              :: X ()
            kickstartApps              = do
                                       myNews
                                       myDocs
                                       myContacts
                                       myTasks
                                       myCalendar
                                       myMail
                                       myVault
--            kickstartSecondary = spawn "skype"
            raiseByMatch :: String -> String -> X ()
            raiseByMatch u m = raiseNextMaybe
                (spawn $ (appBaseCommandurl $ u) ) 
                (className =? "ChromiumAppMode" 
                <&&> (fmap (\t -> isPrefixOf m t 
                || isSuffixOf m t) title)) 
            launchNew :: String -> X ()
            launchNew u = spawn $ (appBaseCommandurl $ u) 
            appBaseCommandurl :: String -> String
            appBaseCommandurl u	= chrome
                ++" --class=ChromiumAppMode "
                ++" --name==chromiumappmode "
                ++" --user-data-dir="
                ++"/home/es/.config/chromium-app-mode "
                ++" --app=" ++ u
            myMutt = raiseMaybe (spawn $ (myTerminal ++ " -e mutt")) (title =? "mutt")
            myMail = raiseByMatch "https://mail.google.com" "Ethan Schoonover Mail"
            -- myTasks = raiseByMatch "https://mail.google.com/tasks/canvas" "Tasks"
            myContacts = raiseByMatch "https://www.google.com/contacts" "Ethan Schoonover contacts"
            myCalendar = raiseByMatch "https://calendar.google.com" "Ethan Schoonover - Calendar"
            myDocs = raiseByMatch "https://docs.google.com" "Google Docs"
            myDocsNew = launchNew "https://docs.google.com" 
            myNews = raiseByMatch "https://reader.google.com" "Google Reader"
            myTasks = raiseByMatch "https://astrid.com" "Astrid"
            myVault = raiseByMatch "chrome-extension://hdokiejnpimakedhajhdlcegeplioahd/homelocal.html" "My LastPass Vault"
        
------------------------------------------------------------------------
-- Applications
------------------------------------------------------------------------

--skype           = "skype"
calendar        = ""
mail            = ""
tasks           = ""

myTerminal      = "urxvtc || (urxvtd -f -o -q && urxvtc)"
chromeClass     = "Chromium"
chrome          = "chromium --memory-model=low"
                   ++ "     --enable-print-preview"
                   ++ "     --enable-smooth-scrolling"
                   ++ "     --enable-sync-extensions"
myMailApp       = "urxvtc -e Mutt"
--myCalendarApp   = "urxvtc -e gcal view week"
myCalendarApp   = chrome ++ " --app=http://calendar.google.com/"
myDocsApp       = chrome
myTextEditorApp = "vim"

------------------------------------------------------------------------
-- Scratch Pads
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Startup & Restarting
------------------------------------------------------------------------

myStartupHook = do
    E.ewmhDesktopsStartup
    spawn $ "systray &"
    spawn $ "xcompmgr -f -D 6 &"
    spawn $ "dunst &"

myRestart :: X ()
myRestart = spawn 
    $  restartKill "xmobar"
    ++ restartKill "trayer"
    ++ restartKill "xcompmgr"
    ++ restartKill "dunst"
    ++ "xmonad --recompile && xmonad --restart"

restartKill :: String -> String
restartKill p = "for pid in `pgrep " ++ p 
    ++ "`; do kill -9 $pid; done && "

------------------------------------------------------------------------
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
    tabs         = named "Tabbed" $ makeTab $ Simplest
    makeTab l    = tabBar shrinkText myTabTheme Top $ resizeVertical (fi $ decoHeight myTabTheme) $ l

------------------------------------------------------------------------
-- Solarized Colors
------------------------------------------------------------------------

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

------------------------------------------------------------------------
-- Interface
------------------------------------------------------------------------

myFocusFollowsMouse    = False
myBorderWidth          = 1

-- workspaces = ["web", "irc", "code" ] ++ map show [4..9]
myWorkspaces                = ["1","2","3","4","5","6","7","8","9"]

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

------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------

myConfig = defaultConfig
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
    }

------------------------------------------------------------------------
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


------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main = xmonad =<< myXmobar (E.ewmh 
    $ withUrgencyHook NoUrgencyHook 
 -- $ withScratchpads scratchpads 
    $ addDescrKeys' ((superMask, xK_F1), showKeybindings) myKeys
    $ myConfig) where
        showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
        showKeybindings x = addName "Show Keybindings" $ io $ do
            --h <- spawnPipe "zenity --text-info"
            --h <- spawnPipe "cat > ~/tmp/xmonadkeys.txt && urxvtc -hold -e cat ~/tmp/xmonadkeys.txt"
            h <- spawnPipe "cat > ~/tmp/xmonadkeys.txt && urxvtc -e less ~/tmp/xmonadkeys.txt"
            System.IO.UTF8.hPutStr h (unlines $ showKm x)
            hClose h
            return ()
