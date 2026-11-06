{ lib, pkgs, osConfig, ... }:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  services.mpd = {
    enable = true;

    musicDirectory = "${osConfig.fleet.shares.media.root}/music/library";
    playlistDirectory = "${osConfig.fleet.shares.media.root}/music/playlists";
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

  # Upstream mkdirs playlistDirectory and fails while the share is unreachable; a user unit cannot wait on the system mount.
  systemd.user.services.mpd.Service.ExecStartPre = lib.mkForce [ ];

  services.mpdris2 = {
    enable = true;
    settings.Bling = {
      mmkeys = true;
      notify = false;  # Disabling as I prefer my own for finer grain control.
    };
  };
}
