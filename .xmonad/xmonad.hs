import XMonad
import XMonad.Config
import System.IO
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SpawnOn
import qualified Data.Map as Map
import Data.String
import XMonad.Hooks.DynamicLog --xmobar
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers --composeOne
--import XMonad.Hooks.ICCCMFocus as ICCCMFocus
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as StackSet
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Run --spawnPipe, hPutStrLn
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

xF86XK_AudioMicMute     :: KeySym
xF86XK_AudioMicMute     = 269025202
xF86XK_AudioRaiseVolume :: KeySym
xF86XK_AudioRaiseVolume = 0x1008ff13
xF86XK_AudioLowerVolume :: KeySym
xF86XK_AudioLowerVolume = 0x1008ff11
xF86XK_AudioMute        :: KeySym
xF86XK_AudioMute        = 0x1008ff12

newMouse x = Map.union (mouseBindings defaultConfig x) (Map.fromList (myMouse x))

main = do
  myStatusBar <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    {
     normalBorderColor  = myNormalBorderColor
    ,focusedBorderColor = myFocusedBorderColor
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
    `additionalKeys` myAdditionalKeys

-------------------------------------------------
-------------------------------------------------
myModMask             = mod4Mask
myTerminal            = "terminator"
myNormalBorderColor   = "#00FF0F"
myFocusedBorderColor  = "#FF000F"
myFont                = "MigMix 1M:size=9:antialias=true"
myLayout = (smartBorders $ avoidStruts $ maximize $ spacing gapwidth $ gaps [(U, gwU), (D, gwD), (L, gwL), (R, gwR)] $ minimize (tiled ||| tritiled ||| Mirror tiled))
 ||| (noBorders Full) ||| (smartBorders $ avoidStruts $ maximize $ minimize (tiled ||| tritiled ))
  where
    gapwidth = 7
    gwU = 1
    gwD = 0
    gwL = 24
    gwR = 24

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
  ,floatNextHook
  ,isFullscreen -->doFullFloat
  ,className =? "Gimp" --> doFloat
  ]
--  <+> namedScratchpadManageHook myScratchpads

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

colorNormalbg  = "#121212"
colorfg        = "#9fa8b1"
colorGreen     = "#a5d6a7"


myAdditionalKeys =  [
   ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%- && amixer get Master | egrep -o \"([0-9]+)%\" | egrep -o \"[0-9]+\" | xargs -0 volnoti-show")
  ,((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+ && amixer get Master | egrep -o \"([0-9]+)%\" | egrep -o \"[0-9]+\" | xargs -0 volnoti-show")
  ,((0, xF86XK_AudioMute), spawn "zsh .volnoti_mute.sh")
  ,((0, xF86XK_AudioMicMute), spawn "amixer set Capture toggle")
  ,((myModMask, xF86XK_AudioRaiseVolume), spawn "xbacklight +5")
  ,((myModMask, xF86XK_AudioLowerVolume), spawn "xbacklight -5")
  ,((myModMask, xK_m), withFocused (sendMessage . maximizeRestore))
  -- ,((myModMask, xK_n), withFocused minimizeWindow)
  -- ,((myModMask .|. shiftMask, xK_n), sendMessage RestoreNextMinimizedWin)
  ,((myModMask, xK_p), spawn ("dmenu_run -fn '" ++ myFont ++ "'"))
  ,((myModMask, xK_a), spawn ("vivaldi-stable"))
  ,((myModMask .|. shiftMask, xK_a), spawn ("vivaldi-stable --incognito"))
  ,((myModMask .|. shiftMask, xK_z), spawn ("xfce4-appfinder"))
  ,((myModMask .|. shiftMask, 0xffff), spawn ("dm-tool lock"))
  ,((myModMask, xK_F1), spawn ("zsh .touchpad_toggle.sh"))
  ,((myModMask, xK_r), floatNext True >> (spawn ("xterm -e \"zsh -c \'source .xterm_transset.sh; ranger\'\"")))
  ,((myModMask .|. shiftMask, xK_p), spawn ("pavucontrol"))
 -- ,((myModMask, xK_r), namedScratchpadAction myScratchpads "ranger")
  ]

myOverriddenKeys = [
   (myModMask, xK_p)
   ]

myMouse x =
  [
  ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
  ]

myWorkspaces = ["1:term", "2:web", "3:mikutter", "4:mail", "5:slack"] ++ map show [6..9]

myStartupHook = do
    spawnOnce "bash ~/spawn.sh"
--  spawnOnce "compton --config $HOME/.config/compton.conf -b"
--  spawnOnce "xmobar &"
--  spawnOnce "feh --bg-scale $HOME/Pictures/wallPaper/mountain.jpg"
--  spawnOnce "setxkbmap -option ctrl:nocaps"
--  spawnOnce "xinput set-prop 11 \"Device Enabled\" 0"
--  spawnOnce "fcitx"
--  spawnOnce "volnoti"
--  spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype pixel --width 95  --transparent true --alpha 40 --tint 0x121212 --heighttype pixel --height 20 "
--  spawnOnce "nm-applet"
--  spawnOn "3:mikutter" "mikutter"
--  spawnOn "4:mail" "thunderbird"
--  spawnOn "5:slack" "slack"
--myScratchpads :: [NamedScratchpad]
--myScratchpads = [
--  NS "ranger" "terminator -e ranger" (title =? "ranger")
--    (customFloating $ StackSet.RationalRect (1/6) (1/6) (2/3) (2/3))
--  ]
