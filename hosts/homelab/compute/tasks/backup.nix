{ config, self, ... }:
let
  paths = config.custom.homelab.paths;
  backupCfg = config.custom.homelab.tasks.backup;
in
{
  custom.homelab.tasks.backup = {
    enable = true;
    package = self.pkgs.rustic-manage;
    bindings = {
      "/nas/bphenriques/backups"              = paths.bphenriques.backups.root;
      "/nas/bphenriques/notes"                = paths.bphenriques.notes;
      "/nas/bphenriques/private"              = paths.bphenriques.private;
      "/nas/bphenriques/documents"            = paths.bphenriques.documents.root;
      "/nas/bphenriques/photos/library"       = paths.bphenriques.photos.library;
      "/nas/bphenriques/photos/inbox"         = paths.bphenriques.photos.inbox;
      "/nas/media/music/library"              = paths.media.music.library;
      "/nas/media/music/playlists"            = paths.media.music.playlists;
      "/nas/media/gaming/emulation/roms"      = paths.media.gaming.emulation.roms;
      "/nas/media/gaming/emulation/bios"      = paths.media.gaming.emulation.bios;

      # TODO: Configure HA backup scheduler (via UI) to write backups to /var/lib/hass/backups/, then uncomment:
      # "/services/home-assistant"            = "${config.services.home-assistant.configDir}/backups";
    };

    hooks.github = {
      description = "Download GitHub repository tarballs";
      script = ./github-backup.sh;
      environment = {
        GITHUB_TOKEN_FILE = config.sops.secrets."backup/github/token".path;
        GITHUB_REPOS = builtins.concatStringsSep " " [
          "bphenriques/self-hosted"
          "bphenriques/dotfiles"
          "bphenriques/dotfiles-private"
          "bphenriques/curriculum-vitae"
          "bphenriques/retro-handhelds"
        ];
        OUTPUT_DIR = "${backupCfg.extrasDir}/github";
      };
    };
  };

  sops.secrets."backup/github/token" = { };
}
