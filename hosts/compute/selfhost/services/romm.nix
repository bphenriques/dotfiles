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
      "romm/mobygames/api-key" = { };
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
        MOBYGAMES_API_KEY=${config.sops.placeholder."romm/mobygames/api-key"}
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
        # Curation artefacts beside the ROMs; handheld-sync mirrors this list for its own pushes.
        exclude.roms = {
          single_file.names = [ ".stignore" "README.md" "systeminfo.txt" ]; # RomM skips dot folders but not files
          multi_file.names = [ "media" "patches" "archive" ];
        };

        system.platforms = {
          megadrive = "genesis";
          dreamcast = "dc";
          fbneo = "arcade";
          pico8 = "pico";
          gc = "ngc";
        };

        emulatorjs.settings.dosbox_pure.dosbox_pure_conf = "inside"; # autorun the bundled DOSBOX.conf
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
      KIOSK_MODE = "true";
      ENABLE_SCHEDULED_RESCAN = "true";
      SCHEDULED_RESCAN_CRON = "0 3 * * *";
    };
  };

  # Upstream fixes the library to `${dataDir}/library`, so the NAS directories are linked in. Symlinks
  # rather than binds: the automount keeps its idle unmount and self-heals on a NAS reboot. They stay
  # read-only through the units' ProtectSystem=strict, matching the container's `:ro` volumes.
  systemd.tmpfiles.settings."20-romm-library" = {
    "${dataDir}/library/roms"."L+".argument = romsDir;
    "${dataDir}/library/bios"."L+".argument = biosDir;
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
