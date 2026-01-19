{ lib, config, ... }:
let
  inherit (lib) mkOption types;
  
  cfg = config.custom.paths;
  homelabCfg = config.custom.fileSystems.homelab;
in {
  options.custom.paths = {
    # Private Media - Not recoverable.
    bphenriques = {
      root = mkOption {
        type = types.str;
        description = "Base mount point for bphenriques storage";
      };
      backups = {
        root = mkOption { type = types.str; default = "${cfg.bphenriques.root}/backups"; };
        phone = mkOption { type = types.str; default = "${cfg.bphenriques.root}/backups/phone"; };
      };
      
      photos = {
        root = mkOption { type = types.str; default = "${cfg.bphenriques.root}/photos"; };
        library = mkOption { type = types.str; default = "${cfg.bphenriques.root}/photos/library"; };
        inbox = mkOption { type = types.str; default = "${cfg.bphenriques.root}/photos/inbox"; };
      };
      
      documents = {
        root = mkOption { type = types.str; default = "${cfg.bphenriques.root}/documents"; };
        consume = mkOption { type = types.str; default = "${cfg.bphenriques.root}/inbox/documents"; };
      };
      
      notes = mkOption { type = types.str; default = "${cfg.bphenriques.root}/notes"; };
      private = mkOption { type = types.str; default = "${cfg.bphenriques.root}/private"; };
    };
    
    # Shared Media - Painfully recoverable.
    media = {
      root = mkOption {
        type = types.str;
        description = "Base mount point for media storage";
      };
      music = {
        root = mkOption { type = types.str; default = "${cfg.media.root}/music"; };
        library = mkOption { type = types.str; default = "${cfg.media.root}/music/library"; };
        playlists = mkOption { type = types.str; default = "${cfg.media.root}/music/playlists"; };
        inbox = mkOption { type = types.str; default = "${cfg.media.root}/music/inbox"; };
      };

      books = {
        root = mkOption { type = types.str; default = "${cfg.media.root}/books"; };
        library = mkOption { type = types.str; default = "${cfg.media.root}/books/library"; };
        inbox = mkOption { type = types.str; default = "${cfg.media.root}/books/inbox"; };
      };
      
      comics = mkOption { type = types.str; default = "${cfg.media.root}/comics"; };
      
      gaming = {
        emulation = {
          root = mkOption { type = types.str; default = "${cfg.media.root}/gaming/emulation"; };
          roms = mkOption { type = types.str; default = "${cfg.media.root}/gaming/emulation/roms"; };
        };
      };
      
      movies = mkOption { type = types.str; default = "${cfg.media.root}/movies"; };
      tv = mkOption { type = types.str; default = "${cfg.media.root}/tv"; };
      
      downloads = {
        root = mkOption { type = types.str; default = "${cfg.media.root}/downloads"; };
        incomplete = mkOption { type = types.str; default = "${cfg.media.root}/downloads/incomplete"; };
        torrents = mkOption { type = types.str; default = "${cfg.media.root}/torrents"; };
      };
    };
  };

  config = lib.mkIf homelabCfg.enable {
    custom.paths = {
      bphenriques.root = lib.mkDefault homelabCfg.mounts.bphenriques.localMount;
      media.root = lib.mkDefault homelabCfg.mounts.media.localMount;
    };
  };
}
