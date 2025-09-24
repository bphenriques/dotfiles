{ lib, pkgs, config, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  services.mpd = {
    enable = true;
    musicDirectory = "${config.xdg.userDirs.music}/library";
    playlistDirectory = "${config.xdg.userDirs.music}/playlists";
    network.startWhenNeeded = true;
    extraConfig = ''
      audio_output {
        type "pipewire"
        name "PipeWire Sound Server"
      }

      playlist_plugin {
        name "m3u"
        as_directory "yes"
      }

      playlist_plugin {
        name "extm3u"
        as_directory "yes"
      }

      input {
        plugin "curl"
      }
    '';
  };

  custom.programs.mpc-plus = {
    enable = true; # Extension of the known mpc client
    devices = {
      "default" = { host = config.services.mpd.network.listenAddress; port = config.services.mpd.network.port; };
      "pixel" = { };
    };
  };
  custom.services.mpc-plus.enable = true; # Provides player/mixer events notifications
  services.mpdris2 = {
    enable = true;
    multimediaKeys = true;  # Integration with multimedia keys. Nicer as keys control whatever is playing
    notifications = false;  # Disabling as I prefer my own for finer grain control.
  };
}
