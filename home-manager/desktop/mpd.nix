{ lib, pkgs, self, config, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  services.mpd = {
    enable = true;
    musicDirectory = "${config.xdg.userDirs.music}/library";
    network.startWhenNeeded = true;
    extraConfig = ''
      audio_output {
        type "pipewire"
        name "PipeWire Sound Server"
      }
    '';
  };

  # Bridges MPD with mpris for notifications and media keys
  services.mpdris2 = {
    enable = true;
    notifications = true;
    multimediaKeys = true;
  };

  custom.programs.mpc-util.enable = true;
  home.packages = [
    pkgs.rmpc   # MPD client: TUI with artwork
    (pkgs.makeDesktopItem {
      name = "rmpc";
      desktopName = "rmpc";
      icon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } "rmpc" "";
      exec = ''${lib.getExe' pkgs.foot "footclient"} --title=rmpc-tui ${lib.getExe pkgs.rmpc}'';
    })
  ];
}