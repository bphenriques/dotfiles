{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.qbittorrent;
in
{
  options.services.qbittorrent = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = lib.mdDoc ''
        Whether to enable qbittorrent.
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.qbittorrent ];
    # FIXME: user.extraGroups = [ "qbittorrent" ];
  };
}
