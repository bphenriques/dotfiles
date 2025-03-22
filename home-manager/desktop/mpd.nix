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

  custom.programs.mpc-plus.enable = true; # Extension of the known mpc client
  custom.services.mpc-plus.enable = true; # Provides player/mixer events notifications
  services.mpdris2 = {
    enable = true;
    multimediaKeys = true;  # Integration with multimedia keys. Nicer as keys control whathever is playing
    notifications = false;  # Disabling as I prefer my own for finer grain control.
  };
}