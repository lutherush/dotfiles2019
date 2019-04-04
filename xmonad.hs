import XMonad
import Data.Monoid
import System.IO
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Util.Dmenu
-- import XMonad.Hooks.UrgencyHook
-- import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Layouts

import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Spacing 
import XMonad.Layout.NoBorders(smartBorders) 
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.IM
import XMonad.Layout.Circle
import XMonad.Layout.Spacing


--
-- -- end of IMPORTS }}}

myTerminal      = "xterm"
 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 0
 
myModMask       = mod4Mask
 
myWorkspaces    = ["system","term","web","media"] ++ map show [5..8 :: Int] ++ ["9:bit"]
 
-- myNormalBorderColor  = "#dddddd"
-- myFocusedBorderColor = "#dddddd"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
    , ((modm,               xK_v     ), spawn "exe=`dmenu_run` && eval \"exec $exe\"")
   
    -- PrintScreen
    , ((modm,               xK_p     ), spawn "scrot -e")
 
    -- Files manager
    , ((modm,               xK_r     ), spawn "xterm -e ranger")
   
    -- MPD
    , ((modm,               xK_u     ), spawn "xterm -e ncmpcpp")

    -- Browser
    , ((modm,               xK_f     ), spawn "firefox")

    -- IRC
    , ((modm,               xK_x     ), spawn "xterm -e irssi")

    -- Editor
    , ((modm,                xK_e    ), spawn "xterm -e vim")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster)
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_x     ), io (exitWith ExitSuccess))

    -- Halt right there, criminal scum!
    , ((modm .|. shiftMask, xK_q     ), spawn "sudo halt")

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    ]
    ++
 
    -- Switch workspaces
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    -- Switch screens
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
myLayout = tiled ||| Mirror tiled ||| Grid ||| Circle ||| spiral (6/7) ||| Full
   where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = spacing 5 $  Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
 
-- $ xprop | grep WM_CLASS
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className =? "Firefox"         --> doF (W.shift "web") ]
    , [ className =? "Xchat"           --> doF (W.shift "irc") ]
    , [ className =? "Mplayer"         --> doF (W.shift "media")]
    , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
    , [(className =? "Firefox" <&&> resource =? "Plugin-container") --> doFloat]
    , [ className =? "Gimp" --> doFloat]
    ]
 
myEventHook = mempty

-- myLogHook = return ()

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "[" "]" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myStartupHook = return ()

main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults
-- main = xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }

defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
--        normalBorderColor  = myNormalBorderColor,
--        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
--        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
