{ config, ... }:
let
  paths = config.custom.homelab.paths;
in {
  services.mpd = {
    enable = true;
    settings = {
      music_directory = paths.media.music.library;
      playlist_directory = paths.media.music.playlists;
      audio_output = [{
        type = "alsa";
        name = "MAX98357A";
        device = "hw:sndrpihifiberry,0";
        mixer_type = "software";
      }];
    };
  };

  # MPD needs access to SMB mount
  users.users.mpd.extraGroups = [ config.custom.homelab.smb.mounts.media.group ];
  custom.homelab.smb.mounts.media.systemd.dependentServices = [ "mpd" ];

  # Allow MPD control from network
  networking.firewall.allowedTCPPorts = [ 6600 ];
}
