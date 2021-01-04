import XMonad
import XMonad.Config
import System.IO
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SpawnOn
import qualified Data.Map as Map
import Data.String
import XMonad.Hooks.SetWMName
import XMonad.Actions.WindowGo
import XMonad.Actions.Minimize
import XMonad.Actions.Search (selectSearchBrowser, google)
import XMonad.Hooks.DynamicLog --xmobar
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers --composeOne
import XMonad.Hooks.RestoreMinimized
--import XMonad.Hooks.ICCCMFocus as ICCCMFocus
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W
import XMonad.Hooks.RestoreMinimized
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Run --spawnPipe, hPutStrLn
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)

newMouse x = Map.union (mouseBindings defaultConfig x) (Map.fromList (myMouse x))

main = do
  myStatusBar <- spawnPipe "xmobar"
  xmonad $ def
    {
     normalBorderColor  = myNormalBorderColor
    ,focusedBorderColor = myFocusedBorderColor
    ,borderWidth        = myBorderWidth
    ,terminal           = myTerminal
    ,workspaces         = myWorkspaces
    ,modMask            = myModMask
    ,mouseBindings      = newMouse
    ,layoutHook         = onWorkspace "9" myFloatLayout $ myLayout
    ,manageHook         = myManageHook
    ,logHook            = myLogHook myStatusBar
    ,handleEventHook    = myHandleEventHook
    ,startupHook        = myStartupHook
    }
    `removeKeys` myOverriddenKeys
    `additionalKeysP` myAdditionalKeys

-------------------------------------------------
-------------------------------------------------
myModMask             = mod4Mask
myTerminal            = "alacritty -e byobu new"
myNormalBorderColor   = "#b8bb26"
myFocusedBorderColor  = "#fb4934"
myBorderWidth         = 5
myFont                = "MigMix 1M:size=9:antialias=true"
myLayout = (smartBorders $ avoidStruts $ maximize $ spacing gapwidth $ gaps [(U, gwU), (D, gwD), (L, gwL), (R, gwR)] $ minimize (tiled ||| tritiled ||| Mirror tiled))
 ||| (noBorders Full) ||| (smartBorders $ avoidStruts $ maximize $ minimize (tiled ||| tritiled ))
  where
    gapwidth = 18
    gwU = 2
    gwD = 0
    gwL = 84
    gwR = 84

    tiled    = Tall a b c
    tritiled  = ThreeColMid a b c
    a = 1
    b = 8/1366
    c = 1/2
myFloatLayout = simplestFloat

myHandleEventHook = fullscreenEventHook <+> docksEventHook
myManageHook = composeAll
  [manageHook defaultConfig
  ,manageSpawn
  ,manageDocks
  ,namedScratchpadManageHook myScratchpads
  ,floatNextHook
  ,isFullscreen -->doFullFloat
  ,className =? "Gimp" --> doFloat
  ,className =? "copyq" --> doFloat
  ,className =? "Thunar" --> doFloat
  ,className =? "File-roller" --> doFloat
  ]

myLogHook h = dynamicLogWithPP xmobarPP {
--   ppSep    = " "
--  ,ppOutput = hPutStrLn h
--  ,ppTitle  = xmobarColor "green" "" . shorten 80
--  ,ppSort   = getSortByXineramaRule

     ppOrder           = \(ws:l:t:_)  -> [ws,t]
    ,ppCurrent         = xmobarColor colorGreen colorNormalbg . \s -> "O"
    ,ppUrgent          = xmobarColor colorfg    colorNormalbg . \s -> "O"
    ,ppVisible         = xmobarColor colorfg    colorNormalbg . \s -> "O"
    ,ppHidden          = xmobarColor colorfg    colorNormalbg . \s -> "O"
    ,ppHiddenNoWindows = xmobarColor colorfg    colorNormalbg . \s -> "X"
    ,ppTitle           = xmobarColor colorGreen colorNormalbg
    ,ppOutput          = hPutStrLn h
    ,ppSep             = "  "
  }
--  <+> ICCCMFocus.takeTopFocus
--  >> updatePointer (0.8, 0.2) (0, 0)

colorNormalbg  = "#282828"
colorfg        = "#ebdbb2"
colorGreen     = "#b8bb26"


myAdditionalKeys =  [
  ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle; if pactl list sinks | grep '^[[:space:]]Mute:' | grep -Fq \"yes\"; then volnoti-show -m; else pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \\([0-9][0-9]*\\)%.*,\\1,' | xargs -0 volnoti-show; fi")
  ,("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2% && pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \\([0-9][0-9]*\\)%.*,\\1,' | xargs -0 volnoti-show")
  ,("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2% && pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \\([0-9][0-9]*\\)%.*,\\1,' | xargs -0 volnoti-show")
  , ("<XF86AudioPrev>", spawn "playerctl --player=spotify previous")
  , ("<XF86AudioPlay>", spawn "playerctl --player=spotify play-pause")
  , ("<XF86AudioNext>", spawn "playerctl --player=spotify next")

  , ("<XF86Search>", selectSearchBrowser "/usr/bin/google-chrome-stable" google)

  , ("<XF86MonBrightnessDown>", spawn "light -U 4")
  , ("<XF86MonBrightnessUp>", spawn "light -A 4")
  , ("<Print>", spawn "xfce4-screenshooter")

  ,("<XF86AudioMicMute>", spawn "amixer set Capture toggle")

  ,("M-m", withFocused (sendMessage . maximizeRestore))
  ,("M-n", withFocused minimizeWindow)
  ,("M-S-n", withLastMinimized maximizeWindowAndFocus)
  ,("M-p", spawn ("dmenu_run -fn '" ++ myFont ++ "'"))
  ,("M-S-z", spawn "xfce4-appfinder")
  ,("M-C-q", spawn "light-locker-command -l")
  ,("M-<F1>", spawn "zsh .touchpad_toggle.sh")
  --,("M-r", floatNext True >> (spawn ("xterm -e \"zsh -c \'source .xterm_transset.sh; ranger\'\"")))

  , ("M-S-p", spawn "pavucontrol")
  -- , ("M-f", runOrRaiseNext "firefox" (className =? "Firefox"))

  , ("M-a", runOrRaiseNext "google-chrome-stable" (className =? "Google-chrome"))
  , ("M-S-a", namedScratchpadAction myScratchpads "chrome")

  , ("M-s", runOrRaiseNext "slack" (className =? "Slack"))
  , ("M-S-s", runOrRaiseNext "spotify" (className =? "Spotify"))

  , ("M-d", namedScratchpadAction myScratchpads "thunar")

  , ("M-<F4>",     namedScratchpadAction myScratchpads "htop")
  , ("M-f", runOrRaiseNext "alacritty -t nvim -e byobu new -A -s nvim" (className =? "Alacritty" <&&> title =? "nvim"))
  , ("M-S-f",     namedScratchpadAction myScratchpads "nvim")
  , ("M-g", selectSearchBrowser "/usr/bin/google-chrome-stable" google)
  , ("M-<Tab>", goToSelected defaultGSConfig)

  -- , ("M-S-<Space>", layoutScreens 2 (TwoPane 0.5 0.5))
  --, ("M-S-<Space>", layoutScreens 3 $ fixedLayout [Rectangle 0 0 1920 (1080), Rectangle 1920 0 1224 1080, Rectangle (1920 + 1224) 0 (1920 - 1224) 1080])
  -- , ("M-S-C-<Space>", rescreen)
  
  -- className は xprop で調べる
  ]

myOverriddenKeys = [
   (myModMask, xK_p)
   ]

myMouse x =
  [
  ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
  ]

myWorkspaces = ["1:main", "2:misc", "3", "4", "5"] ++ map show [6..9]

myStartupHook = do
    -- Vivado とか Java系のソフトの表示を正しくするため
    setWMName "LG3D"
    spawnOnce "bash ~/spawn.sh"
    spawnOnce "light-locker"
    spawnOnce "compton --config $HOME/.config/compton/compton.conf -b"
--  spawnOnce "xmobar &"
    spawnOnce "feh --bg-scale $HOME/Pictures/wallPaper/gam0022.jpg"
    spawnOnce "setxkbmap -option ctrl:nocaps"
--  spawnOnce "xinput set-prop 11 \"Device Enabled\" 0"
    spawnOnce "xinput map-to-output 12 eDP-1"
    spawnOnce "fcitx-autostart"
    spawnOnce "volnoti"
    spawn "start-pulseaudio-x11"
    spawnOnce "thunar --daemon"
    spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype pixel --width 190  --transparent true --alpha 0 --tint 0x282828 --heighttype pixel --height 36"
    spawnOnce "copyq"
--  spawnOnce "nm-applet"
--  spawnOn "3:mikutter" "mikutter"
    spawnOn "4" "thunderbird"
    spawnOn "4" "slack"
    spawnOn "5" "spotify"
--

myScratchpads :: NamedScratchpads
myScratchpads = [
    NS "alacritty" "alacritty -e byobu new -A -s alacritty" (appName =? "Alacritty") nonFloating
  , NS "htop" "alacritty -t htop -e htop" (title =? "htop") nonFloating
  , NS "nvim" "alacritty -t nvim -e byobu new -A -s nvim" (title =? "nvim") nonFloating
  , NS "chrome" "google-chrome-stable" (className =? "Google-chrome") nonFloating
  , NS "thunar" "thunar" (className =? "thunar") defaultFloating

  ]
