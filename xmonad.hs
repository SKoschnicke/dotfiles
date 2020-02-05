-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

import qualified Data.Map                   as M
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.Search      (SearchEngine, intelligent, multi,
                                             promptSearch, searchEngine,
                                             selectSearch, (!>))
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import qualified XMonad.StackSet            as W
import           XMonad.Util.EZConfig       (additionalKeys)
import           XMonad.Util.Run            (spawnPipe)
import XMonad.Config.Desktop


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/bin/kitty"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:term","2:web","3:editor","4:free","5:emacs"] ++ map show [6..9]


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "firefox"  --> doShift "2:web"
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kitty"       --> doShift "1:term"
    , className =? "Emacs"           --> doShift "5:emacs"
    , className =? "MPlayer"        --> doFloat
    , className =? "Qshutdown"      --> doFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]


------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)


------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#FF0000",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 3

hayoo :: SearchEngine
hayoo = searchEngine "hayoo" "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query="

dictcc :: SearchEngine
dictcc = searchEngine "dictcc" "http://www.dict.cc/?=DEEN&s="

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Lock the screen using xscreensaver.
  , ((modMask .|. shiftMask, xK_l),
     spawn "xscreensaver-command -lock")

  -- Shutdown
  , ((modMask .|. shiftMask, xK_s),
     spawn "qshutdown")

  -- switch between keyboard layouts
  , ((modMask .|. shiftMask, xK_o),
     spawn "~/.xmonad/bin/keyboard-layout")

  -- Launch dmenu via yeganesh.
  -- Use this to launch programs without a key binding.
  -- Note that you need a patched dmenu for fonts to work (Arch: dmenu2 from AUR)
  , ((modMask, xK_p),
     spawn "rofi -show run")

  , ((modMask, xK_t),
     spawn "notify-send \"$(date +'%H:%M')\" \"$(date +'%a, %d.%m.%n%B %Y w%V')\"")

  -- Select and copy an emoji
  , ((modMask .|. shiftMask, xK_r),
     spawn "~/bin/dmenuemoji")

  -- Take a screenshot using teiler
  , ((modMask .|. shiftMask, xK_p),
     spawn "teiler")

  -- Toggle screencast
  , ((modMask .|. shiftMask, xK_x),
     spawn "teiler --togglecast")

  -- Take a screenshot from an area
  , ((modMask .|. shiftMask, xK_a),
     spawn "teiler --quick image area")

  -- Translate
  , ((modMask, xK_d),
     spawn "~/development/lab/WR-translator/wordreference.py de de en")

  -- Play/Pause
  , ((modMask .|. shiftMask, xK_F8),
     spawn "playerctl play-pause")

  -- Next Track
  , ((modMask .|. shiftMask, xK_F9),
     spawn "playerctl next")

  -- Previous Track
  , ((modMask .|. shiftMask, xK_F7),
     spawn "playerctl previous")

  -- Mute volume.
  , ((modMask, xK_F1),
     spawn "amixer -D pulse set Master toggle")

  -- Decrease volume.
  , ((modMask, xK_F2),
     spawn "amixer -q set Master playback 10%-")

  -- Increase volume.
  , ((modMask, xK_F3),
     spawn "amixer -q set Master playback 10%+")

  -- Decrease screen backlight.
  , ((modMask, xK_F5),
     spawn "xbacklight -dec 10")

  -- Increase screen backlight.
  , ((modMask, xK_F6),
     spawn "xbacklight -inc 10")

  -- Toggle keyboard backlight.
  , ((modMask, xK_F11),
     spawn "/home/sven/kb-light.py")

  -- search
  , ((modMask, xK_s),
     promptSearch defaultXPConfig $ intelligent $ hayoo !> dictcc !> multi)

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask .|. shiftMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r,t}, Switch to physical/Xinerama screens 1, 2, 3 or 4
  -- mod-shift-{w,e,r,t}, Move client to screen 1, 2, 3 or 4
 [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main =
  xmonad $ desktopConfig {
    -- simple stuff
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

    -- key bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings

    -- hooks, layouts
    , layoutHook         = smartBorders $ myLayout
    , manageHook = manageDocks <+> myManageHook
    , startupHook = setWMName "LG3D"
    , handleEventHook = handleEventHook desktopConfig <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
  }
