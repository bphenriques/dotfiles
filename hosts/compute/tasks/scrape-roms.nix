{ config, self, lib, pkgs, utils, ... }:
let
  romsDir = config.custom.homelab.paths.media.gaming.emulation.roms;
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

  scrapeAndGenerate = pkgs.writeShellApplication {
    name = "scrape-and-generate-pegasus-artwork";
    runtimeInputs = [ pkgs.skyscraper ];
    text = ''
      ROMS_DIR="${romsDir}"
      ${lib.concatMapStringsSep "\n" (p: let platform = p.scraper or p.dir; in ''
        if [[ -d "$ROMS_DIR/${p.dir}" ]]; then
          printf '=== Scraping ${p.dir} (platform: ${platform}) ===\n'
          Skyscraper -p "${platform}" -i "$ROMS_DIR/${p.dir}" \
            --flags "unattend,onlymissing,nohints" \
            -s screenscraper || printf 'Warning: scraping failed for ${p.dir}\n' >&2

          printf '=== Generating artwork for ${p.dir} ===\n'
          mkdir -p "$HOME/.skyscraper/gamelists/${p.dir}"
          Skyscraper -p "${platform}" -i "$ROMS_DIR/${p.dir}" \
            --flags "unattend" \
            -g "$HOME/.skyscraper/gamelists/${p.dir}" \
            -o "$ROMS_DIR/${p.dir}/media" || printf 'Warning: generate failed for ${p.dir}\n' >&2
        fi
      '') platforms}
    '';
  };
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

  custom.homelab.tasks.scrape-roms = {
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
      ExecStart = utils.escapeSystemdExecArgs [ (lib.getExe scrapeAndGenerate) ];
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
