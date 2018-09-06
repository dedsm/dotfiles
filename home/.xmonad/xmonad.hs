import XMonad
import XMonad.Config.Mate
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.FloatKeys
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen(fullscreenFull)
import XMonad.Actions.PhysicalScreens
import System.IO
import XMonad.Hooks.EwmhDesktops
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = do
    xmonad $ ewmh mateConfig { 
        borderWidth = 3,
        terminal = "gnome-terminal",
        normalBorderColor = "#cccccc",
        focusedBorderColor = "#cd8b00",
        startupHook = docksStartupHook,
        manageHook = manageDocks <+> myManageHook <+> manageHook mateConfig,
        handleEventHook = docksEventHook <+> handleEventHook mateConfig <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook,
        layoutHook = myLayouts,
        modMask = mod4Mask,
        keys = myKeys <+> keys mateConfig
        } 
        `additionalKeysP`
        [ 
        ("M1-S-f", spawn "chromium")
        , ("M1-S-w", spawn "firefox-developer-edition -p weriot")
        , ("M1-S-p", spawn "caja --no-desktop --browser ~")
        , ("M1-S-c", spawn "gnome-calculator")
        , ("<XF86Calculator>", spawn "gnome-calculator")
        , ("M-x", spawn "playerctl play-pause")
        , ("M-z", spawn "playerctl previous")
        , ("M-c", spawn "playerctl next")
        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
        , ("<XF86AudioPrev>", spawn "playerctl previous")
        , ("<XF86AudioNext>", spawn "playerctl next")
        ]

myManageHook = composeOne
    [ 
    className =? "Gnome-calculator" -?> doFloat
    , isDialog -?> doFloat
    , return True -?> insertPosition End Newer
    ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
              ((modm,               xK_d     ), withFocused (keysResizeWindow (30,30) (1,1)))
              , ((modm,               xK_a     ), withFocused (keysResizeWindow (30,0) (1,1)))
              , ((modm .|. shiftMask, xK_a     ), withFocused (keysResizeWindow (-30,0) (1,1)))
              , ((modm,               xK_s     ), withFocused (keysResizeWindow (0,30) (1,1)))
              , ((modm .|. shiftMask, xK_s     ), withFocused (keysResizeWindow (0,-30) (1,1)))
              , ((modm .|. shiftMask, xK_d     ), withFocused (keysResizeWindow (-30,-30) (1,1)))
              , ((modm, xK_p), spawn "rofi -show run")
              , ((modm .|. shiftMask, xK_p), spawn "rofi -show drun")
              , ((modm .|. shiftMask, xK_m), spawn "/home/david/local/bin/sptfy")
              , ((modm .|. shiftMask, xK_q), spawn "mate-session-save --logout-dialog")
              , ((modm, xK_Print), spawn "scrot -s")
              , ((0, xK_Print), spawn "scrot")
              ]
              ++
              [((modm .|. mask, key), f def sc)
                | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
             ]


myLayouts = avoidStruts (smartBorders tiled ||| smartBorders (Mirror tiled)) ||| noBorders (Full)
            where
                tiled = ResizableTall 1 (2/100) (1/2) []
