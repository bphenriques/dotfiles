{ lib, config, ... }:
let
  inherit (lib) mkOption types;
in {
  options.custom.paths = {
    PUID = mkOption { type = types.int; default = 1028; };
    PGID = mkOption { type = types.int; default = 65541; };

    # Private Media
    BPHENRIQUES_PHONE_BACKUP = mkOption { type = types.str; default = "/volume1/bphenriques/backups/phone"; };
    BPHENRIQUES_BACKUPS = mkOption { type = types.str; default = "/volume1/bphenriques/backups"; };
    GAMING_ROMS_DIR = mkOption { type = types.str; default = "/volume1/media/gaming/emulation/roms"; };
    MUSIC_DIR = mkOption { type = types.str; default = "/volume1/media/music/library"; };
  };
}