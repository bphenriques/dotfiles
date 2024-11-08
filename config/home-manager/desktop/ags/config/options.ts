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
        layout: {
            start: opt<Array<import("widget/bar/Bar").BarWidget>>([
                "launcher",
                "workspaces",
                // "taskbar",
                "media",
                "expander",
                "cava",
            ]),
            center: opt<Array<import("widget/bar/Bar").BarWidget>>([
                "date",
            ]),
            end: opt<Array<import("widget/bar/Bar").BarWidget>>([
                "cava",
                "expander",
                "submap",
                "battery",
                "systray",
                "system",
                "powermenu",
            ]),
        },
        launcher: {
            icon: {
                colored: opt(false),
                icon: opt(icon(icons.nix.nix, icons.ui.search)),
            },
            label: {
                colored: opt(false),
                label: opt(""),
            },
            action: opt(() => App.toggleWindow("launcher")),
        },
        date: {
            format: opt("%R, %a, %d %b"),
            action: opt(() => App.toggleWindow("datemenu")),
        },
        battery: {
            bar: opt<"hidden" | "regular" | "whole">("hidden"),
            charging: opt("#00D787"),
            percentage: opt(true),
            blocks: opt(7),
            width: opt(50),
            low: opt(30),
        },
        workspaces: {
            workspaces: opt(7),
        },
        taskbar: {
            iconSize: opt(0),
            monochrome: opt(true),
            exclusive: opt(false),
        },
        systray: {
            ignore: opt([
                "KDE Connect Indicator",
                "spotify-client",
            ]),
        },
        media: {
            monochrome: opt(true),
            preferred: opt("spotify"),
            direction: opt<"left" | "right">("right"),
            format: opt("󰎈 {artist} - {title} 󰎈"),
            length: opt(0),
        },
        powermenu: {
            monochrome: opt(false),
            action: opt(() => App.toggleWindow("powermenu")),
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