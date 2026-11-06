{ lib, config, self, pkgs, ... }:
let
  wallpapers = "${self.packages.wallpapers}/share/wallpapers";

  browseWallpapers = pkgs.writeShellApplication {
    name = "browse-wallpapers";
    runtimeInputs = [ config.programs.imv.package self.packages.awww-util ];
    text = ''
      # shellcheck disable=SC2016
      imv -r -c 'bind <Return> exec awww-util one "$imv_current_file"' ${wallpapers}
    '';
  };
in
{
  services.awww.enable = true;
  home.packages = [
    self.packages.awww-util
    (pkgs.makeDesktopItem {
      name = "browse-wallpapers";
      desktopName = "Browse Wallpapers";
      icon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; } "wallpaper" "󰸉";
      exec = lib.getExe browseWallpapers;
    })
  ];

  systemd.user.services.set-wallpaper = {
    Unit = {
      Description = "Sets the wallpaper";
      PartOf = [ "awww.service" ];
      After = [ "awww.service" ];
    };
    Install.WantedBy = [ config.wayland.systemd.target ];
    Service = {
      Type = "oneshot";
      # awww-daemon signals no readiness, so the first attempt can beat its socket.
      Restart = "on-failure";
      RestartSec = 1;
      ExecStart = lib.escapeShellArgs [ "${lib.getExe self.packages.awww-util}" "random" wallpapers ];
    };
  };

  my.programs.niri.layerRules.base = [
    ''
      layer-rule {
        match namespace="^awww-daemon$"
        place-within-backdrop true
      }
    ''
  ];
}
