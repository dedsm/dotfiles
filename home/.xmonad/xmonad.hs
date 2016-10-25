import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.FloatKeys
import XMonad.Layout
import XMonad.Layout.Fullscreen(fullscreenFull)
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Actions.PhysicalScreens
import System.IO
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = do
    xmonad $ ewmh desktopConfig { 
        borderWidth = 3,
        terminal = "gnome-terminal",
        normalBorderColor = "#cccccc",
        focusedBorderColor = "#cd8b00",
        manageHook = composeAll
            [ insertPosition Below Newer
            , resource =? "xfce4-notifyd" --> doIgnore
            , className =? "Xfrun4" --> doFloat
            , className =? "Xfce4-appfinder" --> doFloat
            , manageDocks 
            , manageHook defaultConfig
            ],
        handleEventHook = fullscreenEventHook,
        layoutHook = myLayouts,
        modMask = mod4Mask,
        keys = myKeys <+> keys defaultConfig
        } 
        `additionalKeysP`
        [ 
        ("M1-S-w", spawn "google-chrome-stable")
        , ("M1-S-f", spawn "firefox")
        , ("M1-S-p", spawn "thunar")
        , ("M1-S-t", spawn "xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/presentation-mode -T")
        , ("M1-S-c", spawn "gnome-calculator")
        , ("<XF86Calculator>", spawn "gnome-calculator")
        , ("C-M1-l", spawn "xflock4")
        , ("M-x", spawn "playerctl play-pause")
        , ("M-z", spawn "playerctl previous")
        , ("M-c", spawn "playerctl next")
        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
        , ("<XF86AudioPrev>", spawn "playerctl previous")
        , ("<XF86AudioNext>", spawn "playerctl next")
        ]


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
             [
              ((modm,               xK_d     ), withFocused (keysResizeWindow (30,30) (1,1)))
              , ((modm,               xK_a     ), withFocused (keysResizeWindow (30,0) (1,1)))
              , ((modm .|. shiftMask, xK_a     ), withFocused (keysResizeWindow (-30,0) (1,1)))
              , ((modm,               xK_s     ), withFocused (keysResizeWindow (0,30) (1,1)))
              , ((modm .|. shiftMask, xK_s     ), withFocused (keysResizeWindow (0,-30) (1,1)))
              , ((modm .|. shiftMask, xK_d     ), withFocused (keysResizeWindow (-30,-30) (1,1)))
              , ((modm, xK_p), spawn "xfrun4")
              , ((modm .|. shiftMask, xK_p), spawn "xfce4-appfinder")
              , ((modm .|. shiftMask, xK_q), spawn "xfce4-session-logout")
              , ((modm, xK_Print), spawn "scrot -s")
              , ((0, xK_Print), spawn "scrot")
              , ((modm, xK_w), viewScreen 0 >> windows W.focusMaster)
              , ((modm, xK_e), viewScreen 1 >> windows W.focusMaster)
              , ((modm, xK_r), viewScreen 2 >> windows W.focusMaster)
              , ((modm .|. shiftMask, xK_w), sendToScreen 0)
              , ((modm .|. shiftMask, xK_e), sendToScreen 1)
              , ((modm .|. shiftMask, xK_r), sendToScreen 2)
             ]

myLayouts = avoidStruts (smartBorders tiled ||| smartBorders (Mirror tiled)) ||| noBorders (fullscreenFull Full)
            where
                tiled = ResizableTall 1 (2/100) (1/2) []
