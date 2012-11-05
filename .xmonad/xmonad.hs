import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig (additionalKeys)

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    ])

main = xmonad $ gnomeConfig
                { manageHook = myManageHook
                , modMask = mod4Mask  -- Rebind mod to the Windows key
                } `additionalKeys`
                [  ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock")
                ]
