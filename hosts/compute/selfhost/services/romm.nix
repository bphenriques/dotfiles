{ config, lib, ... }:
let
  cfg = config.selfhost;
  shares = config.fleet.shares;

  wg = cfg.apps.wireguard;
  turn = config.services.coturn;
  peerRange = cidr: let p = lib.removeSuffix ".0/24" cidr; in "${p}.0-${p}.255";

  relayAddresses = [ config.fleet.lan.hosts.compute (lib.removeSuffix "/24" wg.address) ];

  dataDir = config.services.romm.dataDir;
  romsDir = "${shares.media.root}/gaming/emulation/roms";
  biosDir = "${shares.media.root}/gaming/emulation/bios";
in
{
  sops = {
    secrets = {
      "romm/screenscraper/user" = { };
      "romm/screenscraper/password" = { };
      "romm/screenscraper/dev-user" = { };
      "romm/screenscraper/dev-password" = { };
    };

    templates."romm-scrapers.env" = {
      owner = "root";
      group = "root";
      mode = "0400";
      content = ''
        SCREENSCRAPER_USER=${config.sops.placeholder."romm/screenscraper/user"}
        SCREENSCRAPER_PASSWORD=${config.sops.placeholder."romm/screenscraper/password"}

        # Application credentials, distinct from the account ones: upstream bakes its own into the image
        SCREENSCRAPER_DEV_ID=${config.sops.placeholder."romm/screenscraper/dev-user"}
        SCREENSCRAPER_DEV_PASSWORD=${config.sops.placeholder."romm/screenscraper/dev-password"}
      '';
    };
  };

  selfhost = {
    apps.coturn = {
      enable = true;
      advertisedAddresses = relayAddresses;
      allowedPeerRanges = [ (peerRange wg.clientSubnet) (peerRange config.fleet.lan.subnet) ];
    };

    apps.romm = {
      enable = true;
      settings = {
        # Curation artefacts beside the ROMs; handheld-sync mirrors these for its own pushes.
        exclude.roms = {
          single_file.names = [
            ".stignore" "README.md" "systeminfo.txt" # RomM skips dot folders but not files
            # Each disc of a set, so the set lists once through its .m3u. Handheld-sync keeps these.
            "*(Disc [0-9]*"
          ];
          multi_file.names = [ "patches" "archive" ];
        };

        system.platforms = {
          megadrive = "genesis";
          dreamcast = "dc";
          fbneo = "arcade";
          pico8 = "pico";
          gc = "ngc";
        };

        # Its own layout so each homebrew file in modern/ is a game, not the whole folder.
        filesystem.structure = lib.genAttrs [ "gb" "gbc" "gba" "nes" ] (_: [
          "roms/{platform}/{game}"
          "roms/{platform}/modern/{game}"
        ]);

        emulatorjs.settings.dosbox_pure.dosbox_pure_conf = "inside"; # autorun the bundled DOSBOX.conf

        # After each scan, writes metadata.pegasus.txt and media beside the ROMs; each host adds its launch command.
        scan = {
          media = [ "box2d" "screenshot" "manual" "logo" "marquee" ];
          pegasus.export = true;
          # Hand-made gamelist.xml corrections outrank the scrapers; it lists only exceptions.
          priority = {
            metadata = [ "gamelist" "ss" "launchbox" "hasheous" ];
            artwork = [ "gamelist" "ss" "libretro" "launchbox" "hasheous" ];
          };
        };
      };
    };

    services.romm = {
      access.allowedGroups = [ cfg.groups.users cfg.groups.admin ]; # guests browse anonymously through KIOSK_MODE
      storage.mounts = [ "media" ];
      extraConfig.landingPage = {
        enable = true;
        listed = false;
      };
    };

    tasks.romm-export-refresh.storage.mounts = [ "media" ];
  };

  # Above the ephemeral range (32768-60999), or opening the relay range would expose unrelated sockets.
  services.coturn = {
    listening-ips = relayAddresses; # not the podman/microvm bridges or the routable IPv6 it would bind otherwise
    min-port = 61000;
    max-port = 61999;
  };

  services.romm = {
    watcher.enable = false; # inotify doesn't fire on the CIFS-mounted library; the nightly rescan covers it
    environmentFile = config.sops.templates."romm-scrapers.env".path;

    extraEnvironment = {
      HASHEOUS_API_ENABLED = "true";
      # Keyless, but reads a local copy of LaunchBox's database that only this task fetches; before the rescan.
      LAUNCHBOX_API_ENABLED = "true";
      ENABLE_SCHEDULED_UPDATE_LAUNCHBOX_METADATA = "true";
      SCHEDULED_UPDATE_LAUNCHBOX_METADATA_CRON = "0 2 * * *";
      KIOSK_MODE = "true";
      ENABLE_SCHEDULED_RESCAN = "true";
      SCHEDULED_RESCAN_CRON = "0 3 * * *";
    };
  };

  # js-dos needs SharedArrayBuffer. `=404` keeps /index.html a try_files file, or the redirect drops these headers.
  services.nginx.virtualHosts.${config.services.romm.nginx.virtualHost}.locations."~ ^/rom/.*/jsdos$" = {
    tryFiles = "$uri $uri/ /index.html =404";
    extraConfig = ''
      add_header Cache-Control "no-cache";
      add_header Cross-Origin-Embedder-Policy "require-corp";
      add_header Cross-Origin-Opener-Policy "same-origin";
    '';
  };

  # Symlinks into the fixed library dir keep the automount's idle unmount. The worker writes the Pegasus export, so it gets CAP_FOWNER.
  systemd.tmpfiles.settings."20-romm-library" = {
    "${dataDir}/library/roms"."L+".argument = romsDir;
    "${dataDir}/library/bios"."L+".argument = biosDir;
  };
  systemd.services.romm-worker.serviceConfig = {
    ReadWritePaths = [ romsDir ];
    AmbientCapabilities = [ "CAP_FOWNER" ];
  };

  # The export only adds, so clear it before the 03:00 rescan rewrites it from current state.
  systemd.services.romm-export-refresh = {
    description = "Clear RomM's Pegasus export before the nightly rescan";
    script = ''
      find ${romsDir} -mindepth 2 -maxdepth 2 \( \
        -type d \( -name covers -o -name screenshots -o -name marquees \) -o \
        -type f -name metadata.pegasus.txt \
      \) -exec rm -r {} +
    '';
    serviceConfig = {
      Type = "oneshot";
      User = config.services.romm.user;
      ProtectSystem = "strict";
      ReadWritePaths = [ romsDir ];
      NoNewPrivileges = true;
    };
  };
  systemd.timers.romm-export-refresh = {
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "02:45";
  };

  # LAN and full-access peers only: the relay egresses locally, which would bypass the forward chain for restricted peers.
  networking.firewall = {
    interfaces.bond0 = {
      allowedUDPPorts = [ turn.listening-port ];
      allowedUDPPortRanges = [{ from = turn.min-port; to = turn.max-port; }];
    };

    extraInputRules = ''
      iifname "wg0" ip saddr ${wg.fullAccessSubnet} udp dport { ${toString turn.listening-port}, ${toString turn.min-port}-${toString turn.max-port} } accept
    '';
  };
}
