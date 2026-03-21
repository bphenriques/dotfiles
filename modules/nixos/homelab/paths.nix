{ lib, config, ... }:
let
  inherit (lib) mkOption types;
  
  cfg = config.custom.homelab.paths;
  smbCfg = config.custom.homelab.smb;

  # Reusable submodule for media categories with a common root/library/inbox layout
  mkMediaCategoryOpt = parent: name: {
    root = mkOption { type = types.str; default = "${parent}/${name}"; };
    library = mkOption { type = types.str; default = "${parent}/${name}/library"; };
    inbox = mkOption { type = types.str; default = "${parent}/${name}/inbox"; };
  };

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

      music = mkMediaCategoryOpt cfg.media.root "music" // {
        playlists = mkOption { type = types.str; default = "${cfg.media.root}/music/playlists"; };
      };

      books  = mkMediaCategoryOpt cfg.media.root "books";
      comics = mkMediaCategoryOpt cfg.media.root "comics";
      manga  = mkMediaCategoryOpt cfg.media.root "manga";

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
