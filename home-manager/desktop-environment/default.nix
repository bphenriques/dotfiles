{ config, lib, pkgs, self, osConfig, ... }:
let
  niri = lib.getExe pkgs.niri;
  terminal = lib.getExe' config.programs.foot.package "footclient";
  screen-lock = ''${niri} msg action do-screen-transition --delay-ms 750 && ${lib.getExe pkgs.hyprlock}'';
  application-launcher = pkgs.fuzzel;
  dmenu = "${lib.getExe pkgs.fuzzel} -d";
  system-monitor = ''${terminal} --title=btop-tui ${lib.getExe pkgs.btop}'';
  filebrowser = "${terminal} --title=yazi-tui ${lib.getExe config.programs.yazi.package}";

  screenshot-menu = let
    grim = lib.getExe pkgs.grim;
    slurp = lib.getExe pkgs.slurp;
    swappy = lib.getExe pkgs.swappy;
  in self.lib.builders.writeDmenuScript pkgs {
    name = "screenshot-dmenu";
    entries = [
      { label = "󰹑    Screenshot Screen";   exec = ''${grim} -o "$(${niri} msg --json focused-output | jq -r '.name')" - | ${swappy} -f -''; }
      { label = "    Screenshot Area";     exec = ''${grim} -g "${slurp}" - | ${swappy} -f -''; }

      # Saveshot: 	grim -g "$(slurp -d)" "pic-selected-$(date '+%y%m%d-%H%M-%S').png"
      # Savescreen: grim "pic-full-$(date '+%y%m%d-%H%M-%S').png"
      # Copy area: grim -g "$(slurp -d)" - | wl-copy
      # Copy screen: grim - | wl-copy
      # Edit Area: grim -g "$(slurp -d)" - | swappy -f -
      # Edit screen: grim - | swappy -f -
    ];
  };
in
{
  imports = [
    ./qt.nix        # Setup theming for some set of apps
    ./gtk.nix       # Setup theming for some set of apps
    ./niri.nix      # Window Manager
    ./waybar        # Status bar
    ./mako.nix      # Notification Daemon
    ./fuzzel.nix    # Application Launcher
    ./rofi.nix      # Alternative customizable menu
    ./swayidle.nix  # Locks/suspends the computer when idle
    ./hyprlock.nix  # Lock screend
    ./osd.nix       # On Screen Display
    ./swww.nix      # Backend to manage wallpapers
  ];

  custom.desktop-environment.apps = {
    core = {
      inherit application-launcher screen-lock terminal;

      file-browser = "${terminal} --title=yazi-tui ${lib.getExe config.programs.yazi.package}";
      window-switcher = self.pkgs.niri-window-dmenu;
      session-menu = self.lib.builders.writeDmenuScript pkgs {
        name = "session-dmenu";
        entries = [
          { label = "    Lock";               exec = screen-lock; }
          { label = "󰤄    Suspend";            exec = "systemctl suspend"; }
          { label = "    Shutdown";           exec = "systemctl poweroff"; }
          { label = "    Reboot";             exec = "systemctl reboot"; }
          { label = "    Reboot to EFI setup";     exec = "systemctl reboot --firmware-setup"; }
        ] ++ lib.optionals (osConfig.custom.boot.grub.windows.efiDevice != "") [
          { label = "    Reboot to Windows";  exec = lib.getExe osConfig.custom.boot.grub.windows.rebootPackage; }
        ] ++ [
          { label = "󰞱    System Monitor";     exec = system-monitor; }
        ];
      };
    };
    tools = {
      inherit system-monitor screenshot-menu;
      emoji-picker = pkgs.writeShellApplication {
        name = "emoji-picker";
        text = ''BEMOJI_PICKER_CMD="${dmenu}" ${lib.getExe pkgs.bemoji} --noline --type --clip'';
      };
    };
  };

  home.packages = [
    (pkgs.makeDesktopItem {
      name = "screenshot-dmenu";
      desktopName = "Open Screenshot menu";
      icon = "folder";  # FIXME
      exec = lib.getExe screenshot-menu;
    })

    (pkgs.makeDesktopItem {
      name = "system-monitor";
      desktopName = "System Monitor";
      icon = "folder";  # FIXME
      exec =  system-monitor;
    })

    (pkgs.makeDesktopItem {
      name = "File Browser";
      desktopName = "Open file browser";
      icon = "folder";  # FIXME
      exec = filebrowser;
      actions = let
        bookmarkToAction = (b: {
          name = builtins.replaceStrings [" "] ["-"] b.name;
          value = { inherit (b) name; exec = "${filebrowser} ${b.path}"; };
        });
      in builtins.listToAttrs (lib.map bookmarkToAction config.custom.desktop-environment.settings.file-bookmarks);
    })
  ];
}