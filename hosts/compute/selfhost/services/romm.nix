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
            # Each disc of a set, so the set lists once, through its .m3u. Not an artefact: handheld-sync keeps
            # these. A folder per set would also list once, but the Pegasus export points at the folder.
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

        # Homebrew sits in modern/ for the handhelds' sake. Listing that level as a layout of its own makes each
        # file a game, where the default layout alone would read the whole folder as one.
        filesystem.structure = lib.genAttrs [ "gb" "gbc" "gba" "nes" ] (_: [
          "roms/{platform}/{game}"
          "roms/{platform}/modern/{game}"
        ]);

        emulatorjs.settings.dosbox_pure.dosbox_pure_conf = "inside"; # autorun the bundled DOSBOX.conf

        # After each scan, write <platform>/metadata.pegasus.txt and copy this media beside the ROMs
        # (covers/, screenshots/, marquees/). Pegasus reads both; each host adds its own launch command.
        scan = {
          media = [ "box2d" "screenshot" "manual" "logo" "marquee" ];
          pegasus.export = true;
          # gamelist.xml holds hand-made corrections, so it outranks the scrapers. It lists only the exceptions,
          # so every other game resolves as before. Sources left out here follow these.
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

  # Relay sockets above the ephemeral range: the firewall has to open the whole range, and the kernel
  # hands out 32768-60999, so anything inside it would expose unrelated sockets on bond0/wg0.
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

  # The module's cross-origin isolation map predates the js-dos player, whose DOSBox-X backend is a
  # threaded build and refuses to start without SharedArrayBuffer. The trailing `=404` keeps
  # /index.html a try_files *file*: as the fallback URI it would internally redirect into the
  # module's `/` location, whose own add_header set replaces everything below.
  services.nginx.virtualHosts.${config.services.romm.nginx.virtualHost}.locations."~ ^/rom/.*/jsdos$" = {
    tryFiles = "$uri $uri/ /index.html =404";
    extraConfig = ''
      add_header Cache-Control "no-cache";
      add_header Cross-Origin-Embedder-Policy "require-corp";
      add_header Cross-Origin-Opener-Policy "same-origin";
    '';
  };

  # Upstream fixes the library to `${dataDir}/library`, so the NAS directories are linked in. Symlinks
  # rather than binds: the automount keeps its idle unmount and self-heals on a NAS reboot. They stay
  # read-only through the units' ProtectSystem=strict, matching the container's `:ro` volumes, except the
  # ROMs for the worker: its scans write the Pegasus export there. RomM chmods every file it writes, and the
  # share presents files as root's, so the worker needs CAP_FOWNER for that to succeed.
  systemd.tmpfiles.settings."20-romm-library" = {
    "${dataDir}/library/roms"."L+".argument = romsDir;
    "${dataDir}/library/bios"."L+".argument = biosDir;
  };
  systemd.services.romm-worker.serviceConfig = {
    ReadWritePaths = [ romsDir ];
    AmbientCapabilities = [ "CAP_FOWNER" ];
  };

  # RomM's export only adds: it never replaces an exported image, and keeps the entries of games it no longer
  # has. Clearing it before the 03:00 rescan has it written again from RomM's current state.
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

  # LAN, and only the VPN peers that already reach the LAN: the relay originates its traffic locally,
  # so a restricted peer using it would get the UDP into the LAN that the forward chain denies them.
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
