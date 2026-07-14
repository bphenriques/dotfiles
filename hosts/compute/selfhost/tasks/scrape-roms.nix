{ config, self, lib, utils, ... }:
let
  romsDir = config.custom.paths.media.gaming.emulation.roms;
  stateDir = "/var/lib/skyscraper";
  skyscraperConfig = config.sops.templates."skyscraper-config.ini".path;

  # dir = ROM directory name, scraper = Skyscraper platform name (when different)
  platforms = [
    { dir = "megadrive"; }
    { dir = "nes"; }
    { dir = "snes"; }
    { dir = "psx"; }
    { dir = "ps2"; }
    { dir = "psp"; }
    { dir = "wii"; }
    { dir = "gb"; }
    { dir = "gbc"; }
    { dir = "gba"; }
    { dir = "nds"; }
    { dir = "fbneo"; scraper = "arcade"; }
    { dir = "dreamcast"; }
  ];

  romArtworkArgs = map (p: "${p.scraper or p.dir}:${romsDir}/${p.dir}") platforms;
in
{
  sops.secrets."romm/screenscraper/user" = { };
  sops.secrets."romm/screenscraper/password" = { };
  sops.templates."skyscraper-config.ini" = {
    owner = "root";
    group = "root";
    mode = "0400";
    content = ''
      [main]
      cacheFolder="${stateDir}/.skyscraper/cache"

      [screenscraper]
      userCreds="${config.sops.placeholder."romm/screenscraper/user"}:${config.sops.placeholder."romm/screenscraper/password"}"
    '';
  };

  selfhost.tasks.scrape-roms = {
    systemdServices = [ "scrape-roms" ];
    storage.smb = [ "media" ];
  };

  systemd.services.scrape-roms = {
    description = "Scrape ROM artwork from ScreenScraper";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      StateDirectory = "skyscraper";
      ExecStart = utils.escapeSystemdExecArgs ([ (lib.getExe self.packages.rom-artwork) ] ++ romArtworkArgs);
      Environment = [
        "HOME=${stateDir}"
      ];

      Restart = "no";
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
      ReadWritePaths = [ stateDir romsDir ];
    };
  };

  # Skyscraper looks for config at $HOME/.skyscraper/config.ini (not XDG)
  systemd.tmpfiles.rules = [
    "d ${stateDir}                       0750 root root -"
    "d ${stateDir}/.skyscraper           0750 root root -"
    "L+ ${stateDir}/.skyscraper/config.ini - - - - ${skyscraperConfig}"
  ];

  systemd.timers.scrape-roms = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "weekly";
      RandomizedDelaySec = "6h";
      Persistent = true;
    };
  };
}
