{ lib, config, ... }:
let
  inherit (lib) mkOption types;
  
  cfg = config.custom.paths;
  homelabCfg = config.custom.fileSystems.homelab;
in {
  options.custom.paths = {
    # Base mount point
    bphenriquesMount = mkOption {
      type = types.str;
      default = homelabCfg.mounts.bphenriques.localMount;
      description = "Base mount point for bphenriques storage";
    };
    
    mediaMount = mkOption {
      type = types.str;
      default = homelabCfg.mounts.media.localMount;
      description = "Base mount point for media storage";
    };
    
    # Private Media - Not recoverable.
    bphenriques = {
      backups = {
        root = mkOption { type = types.str; default = "${cfg.bphenriquesMount}/backups"; };
        phone = mkOption { type = types.str; default = "${cfg.bphenriques.backups.root}/phone"; };
      };
      
      photos = {
        library = mkOption { type = types.str; default = "${cfg.bphenriquesMount}/photos/library"; };
        inbox = mkOption { type = types.str; default = "${cfg.bphenriquesMount}/photos/inbox"; };
      };
      
      documents = {
        root = mkOption { type = types.str; default = "${cfg.bphenriquesMount}/documents"; };
        consume = mkOption { type = types.str; default = "${cfg.bphenriquesMount}/inbox/documents"; };
      };
      
      notes = mkOption { type = types.str; default = "${cfg.bphenriquesMount}/notes"; };
      private = mkOption { type = types.str; default = "${cfg.bphenriquesMount}/private"; };
    };
    
    # Shared Media - Painfully recoverable.
    media = {
      music = {
        library = mkOption { type = types.str; default = "${cfg.mediaMount}/music/library"; };
        inbox = mkOption { type = types.str; default = "${cfg.mediaMount}/music/inbox"; };
      };

      books = {
        library = mkOption { type = types.str; default = "${cfg.mediaMount}/books/library"; };
        inbox = mkOption { type = types.str; default = "${cfg.mediaMount}/books/inbox"; };
      };
      
      comics = mkOption { type = types.str; default = "${cfg.mediaMount}/comics"; };
      
      gaming = {
        emulation = {
          root = mkOption { type = types.str; default = "${cfg.mediaMount}/gaming/emulation"; };
          roms = mkOption { type = types.str; default = "${cfg.media.gaming.emulation.root}/roms"; };
        };
      };
      
      movies = mkOption { type = types.str; default = "${cfg.mediaMount}/movies"; };
      tv = mkOption { type = types.str; default = "${cfg.mediaMount}/tv"; };
      
      downloads = {
        root = mkOption { type = types.str; default = "${cfg.mediaMount}/downloads"; };
        incomplete = mkOption { type = types.str; default = "${cfg.mediaMount}/downloads/incomplete"; };
        torrents = mkOption { type = types.str; default = "${cfg.media.downloads.root}/torrents"; };
      };
    };
  };
}