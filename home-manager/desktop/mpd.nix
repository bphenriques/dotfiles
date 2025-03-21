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

  # Custom mpc extension that is binded to my keys and sends notifications.
  # Downside: notifications only work through this. Alternative: mpdris2 or build my own event loop.
  custom.programs.mpc-plus.enable = true;
}