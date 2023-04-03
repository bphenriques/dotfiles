{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.jellyfin;
in
{
  options.modules.services.jellyfin = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = mdDoc ''Whether to enable jellyfin service.'';
    };
  };

  config = mkIf cfg.enable {
    services.jellyfin = {
      enable = true;
      openFirewall = true;
    };
    environment.systemPackages = with pkgs; [ jellyfin-ffmpeg ];
  };
}
