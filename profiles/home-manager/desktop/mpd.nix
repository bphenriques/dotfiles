{ lib, pkgs, config, osConfig, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  services.mpd = {
    enable = true;

    musicDirectory = osConfig.custom.paths.media.music.library;
    playlistDirectory = osConfig.custom.paths.media.music.playlists;
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
    enable = true;
    devices = {
      "default" = { host = config.services.mpd.network.listenAddress; port = config.services.mpd.network.port; };
      "pixel" = { };
    };
  };
  custom.services.mpc-plus.enable = true; # notifications
  services.mpdris2 = {
    enable = true;
    multimediaKeys = true;  # Integration with multimedia keys.
    notifications = false;  # Disabling as I prefer my own for finer grain control.
  };
}
