import { opt, mkOptions } from "lib/option"
import { icon } from "lib/utils"
import icons from "lib/icons"

const nix = JSON.parse(Utils.readFile(Utils.CACHE_DIR + "/options-nix.json") || '{}')

//https://github.com/Serpentian/AlfheimOS/blob/master/non-nix/ags/widget/bar/buttons/Launcher.ts
//https://github.com/Serpentian/AlfheimOS/blob/master/user/wm/hyprland/ags.nix

const options = mkOptions(OPTIONS, {
    wallpaper: opt(nix?.wallpaper || ""),
    font: {
        size: opt(nix?.font?.size || 12),
        name: opt(nix?.font?.name || "Ubuntu Nerd Font"),
    },


    bar: {
        launcher: {
            launcherExe: opt(nix?.launcher || ""),
            icon: {
                colored: opt(false),
                icon: opt(icon(icons.nix.nix, icons.ui.search)),
            },
        },
        workspaces: {
            workspaces: opt(10),
        },
        systray: {
            ignore: opt([
                "KDE Connect Indicator",
                "spotify-client",
            ]),
        },
        media: {
            format: opt("󰎈 {artist} - {title} 󰎈"),
            length: opt(0),
        },
    },

    powermenu: {
        sleep: opt("systemctl suspend"),
        reboot: opt("systemctl reboot"),
        logout: opt("pkill Hyprland"),
        shutdown: opt("shutdown now"),
    },

    notifications: {
        blacklist: opt(["Spotify"]),
    },
})

globalThis["options"] = options
export default options