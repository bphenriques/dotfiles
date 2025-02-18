{ config, lib, pkgs, self, osConfig, ... }:
let
  niri = lib.getExe pkgs.niri;
  terminal = lib.getExe' config.programs.foot.package "footclient";
  screen-lock = ''${niri} msg action do-screen-transition --delay-ms 750 && ${lib.getExe pkgs.hyprlock}'';
  application-launcher = "${lib.getExe pkgs.fuzzel} --show-actions";
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
    ./rofi          # Application Launcher
    ./swayidle.nix  # Locks/suspends the computer when idle
    ./hyprlock.nix  # Lock screend
    ./osd.nix       # On Screen Display
    ./swww.nix      # Backend to manage wallpapers
  ];

  custom.desktop-environment = {
    inherit application-launcher system-monitor screen-lock terminal screenshot-menu;

    window-switcher = self.pkgs.niri-window-dmenu;
    file-browser = "${filebrowser} ~";

    session-menu = self.lib.builders.writeDmenuScript pkgs {
      name = "session-dmenu";
      entries = [
        { label = "    Shutdown";           exec = "systemctl poweroff"; }
        { label = "    Reboot";             exec = "systemctl reboot"; }
        { label = "    Lock";               exec = screen-lock; }
        { label = "󰤄    Suspend";            exec = "systemctl suspend"; }
      ] ++ lib.optionals (osConfig.custom.boot.grub.windowsEfiDevice != "") [
        { label = "    Reboot to Windows";  exec = lib.getExe osConfig.custom.boot.grub.windowsRebootPackage; }
      ] ++ [
        { label = "󰞱    System Monitor";     exec = system-monitor; }
      ];
    };

    emoji-picker = pkgs.writeShellApplication {
      name = "emoji-picker";
      text = ''BEMOJI_ECHO_NEWLINE=false BEMOJI_PICKER_CMD=${dmenu} ${lib.getExe pkgs.bemoji}'';
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
      name = "File Browser";
      desktopName = "Open file browser";
      icon = "folder";  # FIXME
      exec = filebrowser;
      actions = builtins.listToAttrs
        (lib.map
          (b: {
            name = builtins.replaceStrings [" "] ["-"] b.name;
            value = { inherit (b) name; exec = "${filebrowser} ${b.path}"; };
          })
          config.custom.desktop-environment.file-bookmarks
        );
    })
  ];
}




/*
{ name = "documents"; path = config.xdg.userDirs.documents; }

 {
        "documents" = { name = "documents"; exec = "${filebrowser} ${config.xdg.userDirs.documents}"; }; # FIXME: Icon
        "pictures" = { name = "Pictures"; exec = "${filebrowser} ${config.xdg.userDirs.pictures}"; }; # FIXME: Icon
        "music" = { name = "music"; exec = "${filebrowser} ${config.xdg.userDirs.music}"; }; # FIXME: Icon
        "downloads" = { name = "Downloads"; exec = "${filebrowser} ${config.xdg.userDirs.download}"; }; # FIXME: Icon

        "nas-private" = { name = "Pictures"; exec = "${filebrowser} ${config.xdg.userDirs.pictures}"; }; # FIXME: Icon
        "pictures" = { name = "Pictures"; exec = "${filebrowser} ${config.xdg.userDirs.pictures}"; }; # FIXME: Icon
      };
  gtk.gtk3.bookmarks = [
    "file://${config.xdg.userDirs.documents}"
    "file://${config.xdg.userDirs.pictures}"
    "file://${config.xdg.userDirs.music}"
    "file://${config.xdg.userDirs.download}"
    "file://${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR}"

    # Other
    "file://${config.home.homeDirectory}/nas-private"
    "file://${config.home.homeDirectory}/nas-media"
    "file://${config.home.homeDirectory}/games"
    "file://${config.home.homeDirectory}/.config Config"
  ];*/
