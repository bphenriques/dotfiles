{ config, ... }:
let
  paths = config.custom.homelab.paths;
in {
  services.mpd = {
    enable = true;

    musicDirectory = paths.media.music.library;
    playlistDirectory = paths.media.music.playlists;
    extraConfig = ''
      audio_output {
        type "alsa"
        name "MAX98357A"
        device "hw:sndrpihifiberry,0"
        mixer_type "software"
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

  # MPD needs access to SMB mount
  users.users.mpd.extraGroups = [ config.custom.homelab.smb.mounts.media.group ];
  custom.homelab.smb.mounts.media.systemd.dependentServices = [ "mpd" ];

  # Allow MPD control from network
  networking.firewall.allowedTCPPorts = [ 6600 ];
}
