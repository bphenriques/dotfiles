{ lib, pkgs, config, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  services.mpd = {
    enable = true;
    musicDirectory = "${config.xdg.userDirs.music}/library";
    network.startWhenNeeded = true;
    extraConfig = ''
      auto_update "yes"
      audio_output {
        type "pipewire"
        name "PipeWire Sound Server"
      }

      audio_output {
      	type        "fifo"
      	name        "Visualizer"
      	format      "44100:16:2"
      	path        "/tmp/mpd.fifo"
      }
    '';
  };

  # TODO: https://wiki.archlinux.org/title/Music_Player_Daemon/Tips_and_tricks#Adding_a_separate_volume_control_(ALSA)
  # To ensure I can adjust the volume of music individually

  services.mpd-mpris = {
    enable = true;
    mpd.useLocal = true;
  };
}