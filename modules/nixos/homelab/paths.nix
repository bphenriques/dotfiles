{ lib, config, ... }:
let
  inherit (lib) mkOption types;
  
  cfg = config.custom.homelab.paths;
  smbCfg = config.custom.homelab.smb;

  userPathsOpt = types.submodule ({ config, ... }: {
    options = {
      root = mkOption {
        type = types.str;
        description = "Base mount point for this user's storage";
      };

      backups = {
        root = mkOption { type = types.str; default = "${config.root}/backups"; };
        phone = mkOption { type = types.str; default = "${config.root}/backups/phone"; };
      };
      
      photos = {
        root = mkOption { type = types.str; default = "${config.root}/photos"; };
        library = mkOption { type = types.str; default = "${config.root}/photos/library"; };
        inbox = mkOption { type = types.str; default = "${config.root}/photos/inbox"; };
      };
      
      documents = {
        root = mkOption { type = types.str; default = "${config.root}/documents"; };
        consume = mkOption { type = types.str; default = "${config.root}/inbox/documents"; };
      };
      
      notes = mkOption { type = types.str; default = "${config.root}/notes"; };
      private = mkOption { type = types.str; default = "${config.root}/private"; };
    };
  });
in {
  options.custom.homelab.paths = {
    users = mkOption {
      type = types.attrsOf userPathsOpt;
      default = { };
      description = "Per-user storage paths";
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

      comics = {
        root = mkOption { type = types.str; default = "${cfg.media.root}/comics"; };
        library = mkOption { type = types.str; default = "${cfg.media.root}/comics/library"; };
        inbox = mkOption { type = types.str; default = "${cfg.media.root}/comics/inbox"; };
      };

      manga = {
        root = mkOption { type = types.str; default = "${cfg.media.root}/manga"; };
        library = mkOption { type = types.str; default = "${cfg.media.root}/manga/library"; };
        inbox = mkOption { type = types.str; default = "${cfg.media.root}/manga/inbox"; };
      };

      gaming = {
        emulation = {
          root = mkOption { type = types.str; default = "${cfg.media.root}/gaming/emulation"; };
          roms = mkOption { type = types.str; default = "${cfg.media.root}/gaming/emulation/roms"; };
          bios = mkOption { type = types.str; default = "${cfg.media.root}/gaming/emulation/bios"; };
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

  config = lib.mkIf smbCfg.enable {
    custom.homelab.paths = {
      media.root = lib.mkDefault smbCfg.mounts.media.localMount;
      users = lib.mapAttrs
        (_: m: { root = lib.mkDefault m.localMount; })
        (lib.removeAttrs smbCfg.mounts [ "media" ]);
    };
  };
}
