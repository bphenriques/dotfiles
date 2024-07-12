{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.custom.profiles.development;
in
{
  options.custom.profiles.development = with types; {
    enable = mkEnableOption "Whether to set-up a development environment.";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      jetbrains.idea-community
      filezilla

      # Raspberry Pi
      rpi-imager
    ];

    virtualisation.docker.enable = true;
    custom.impermanence.root.config.directories = [ "/var/lib/docker" ];
  };
}

