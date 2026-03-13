{ config, self, ... }:
let
  paths = config.custom.homelab.paths;
in
{
  custom.homelab.tasks.backup.integrations.ntfy = {
    enable = true;
    topic = "backups";
  };

  custom.homelab.backup = {
    package = self.pkgs.rustic-manage;
    bindings = {
      "/nas/bphenriques/backups"              = paths.users.bphenriques.backups.root;
      "/nas/bphenriques/notes"                = paths.users.bphenriques.notes;
      "/nas/bphenriques/private"              = paths.users.bphenriques.private;
      #"/nas/bphenriques/documents"            = paths.users.bphenriques.documents.root;
      #"/nas/bphenriques/photos/library"       = paths.users.bphenriques.photos.library;
      #"/nas/bphenriques/photos/inbox"         = paths.users.bphenriques.photos.inbox;
      #"/nas/media/music/library"              = paths.media.music.library;
      #"/nas/media/music/playlists"            = paths.media.music.playlists;
      #"/nas/media/gaming/emulation/roms"      = paths.media.gaming.emulation.roms;
      #"/nas/media/gaming/emulation/bios"      = paths.media.gaming.emulation.bios;

      # TODO: Configure HA backup scheduler (via UI) to write backups to /var/lib/hass/backups/, then uncomment:
      # "/services/home-assistant"            = "${config.services.home-assistant.configDir}/backups";
    };
  };
}
