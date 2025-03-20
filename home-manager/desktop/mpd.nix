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

  # Custom tool that is binded to my keys and sends notifications.
  # Downside? Notifications only work _when_ I use the cli-tool. I am ok with that. Alternative: mpdris2.
  custom.programs.mpc-util.enable = true;
}