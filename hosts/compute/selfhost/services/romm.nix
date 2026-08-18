{ config, lib, pkgs, ... }:
let
  cfg = config.selfhost;
  selfhostMounts = cfg.storage.mounts.smb.shares;
  shares = config.custom.shares;

  turn = {
    listenPort = 3478;
    minPort = 49152;
    maxPort = 49999;
    # Static credentials, shared by the coturn user and the EmulatorJS client below so they cannot
    # desync. Plaintext is acceptable: the relay is LAN/WG-only (firewall-scoped below).
    username = "romm";
    credential = "romm-netplay";
  };

  dataDir = config.services.romm.dataDir;
  romsDir = "${shares.media.root}/gaming/emulation/roms";
  biosDir = "${shares.media.root}/gaming/emulation/bios";

  # Example: https://github.com/rommapp/romm/blob/master/examples/config.example.yml
  yamlFormat = pkgs.formats.yaml { };
  configFile = yamlFormat.generate "romm-config.yml" {
    exclude.roms = {
      single_file = {
        extensions = [ "stfolder" ];
        names = [ ".stignore" ];
      };
      multi_file.names = [
        ".stfolder"
        ".idea"
        "media"
      ];
    };
    system.platforms = {
      megadrive = "genesis";
      dreamcast = "dc";
      fbneo = "arcade";
      pico8 = "pico";
      gc = "ngc";
    };

    # Works somewhat but the display is buggy and laggy and only works Chrome<->Chrome. I do not recommend yet.
    emulatorjs.netplay = {
      enabled = true;
      ice_servers =
        let
          port = toString config.services.coturn.listening-port;
          lanIP = config.custom.fleet.lan.hosts.compute;
          wgIP = "10.100.0.1";
          turnCreds = { inherit (turn) username credential; };
        in
        lib.concatMap (ip: [
          { urls = "stun:${ip}:${port}"; }
          ({ urls = "turn:${ip}:${port}?transport=udp"; } // turnCreds)
        ]) [ lanIP wgIP ];
    };

    emulatorjs.settings = {
      dosbox_pure.dosbox_pure_conf = "inside"; # autorun the bundled DOSBOX.conf
    };
  };
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

    # RomM reads credentials from the environment: the `_FILE` suffix its docs mention is a feature of the
    # container entrypoint, which the package does not ship.
    templates."romm-scrapers.env" = {
      owner = "root";
      group = "root";
      mode = "0400";
      content = ''
        MOBYGAMES_API_KEY=${config.sops.placeholder."romm/mobygames/api-key"}
        SCREENSCRAPER_USER=${config.sops.placeholder."romm/screenscraper/user"}
        SCREENSCRAPER_PASSWORD=${config.sops.placeholder."romm/screenscraper/password"}
        # Application credentials, distinct from the account ones: upstream bakes its own into the image
        # from CI secrets, so a from-source package sends an empty devid and ScreenScraper 403s.
        SCREENSCRAPER_DEV_ID=${config.sops.placeholder."romm/screenscraper/dev-user"}
        SCREENSCRAPER_DEV_PASSWORD=${config.sops.placeholder."romm/screenscraper/dev-password"}
      '';
    };
  };

  selfhost = {
    apps.romm.enable = true;

    services.romm = {
      access.allowedGroups = with cfg.groups; [ guests users admin ];
      storage.mounts = [ "media" ];
      extraConfig.landingPage = { enable = true; listed = false; };
    };

    # coturn serves no HTTP, so only its socket is registered, for the port-collision guard.
    internal.listeningPorts = [
      {
        name = "coturn";
        host = "0.0.0.0";
        port = turn.listenPort;
        protocol = "udp";
      }
    ];
  };

  services.romm = {
    watcher.enable = false; # inotify doesn't fire on the CIFS-mounted library; the nightly rescan covers it
    environmentFile = config.sops.templates."romm-scrapers.env".path;

    extraEnvironment = {
      DISABLE_SETUP_WIZARD = "true";
      HASHEOUS_API_ENABLED = "true";
      KIOSK_MODE = "true";
      ENABLE_SCHEDULED_RESCAN = "true";
      SCHEDULED_RESCAN_CRON = "0 3 * * *";
    };
  };

  users.users = {
    romm.extraGroups = [ selfhostMounts.media.group ];
    nginx.extraGroups = [ selfhostMounts.media.group ]; # nginx serves the ROM bytes, and the share is mounted 0660
  };

  # Upstream fixes the library to `${dataDir}/library`, so the NAS directories are linked in. Symlinks
  # rather than binds: the automount keeps its idle unmount and self-heals on a NAS reboot. They stay
  # read-only through the units' ProtectSystem=strict, matching the container's `:ro` volumes.
  systemd = {
    tmpfiles.settings."20-romm-library" = {
      "${dataDir}/library/roms"."L+".argument = romsDir;
      "${dataDir}/library/bios"."L+".argument = biosDir;
      "${dataDir}/config/config.yml"."L+".argument = "${configFile}";
    };

    # Every unit of the service reads it: the API serves it, the worker scans with it.
    services = lib.genAttrs cfg.services.romm.systemdServices (_: { restartTriggers = [ configFile ]; });
  };

  # Minimal STUN/TURN for the EmulatorJS netplay above. Accepted risk: static credentials in the Nix
  # store, mitigated by the LAN/WG-scoped firewall below.
  services.coturn = {
    enable = true;
    listening-port = turn.listenPort;
    lt-cred-mech = true;
    no-cli = true;
    no-tcp-relay = true;
    min-port = turn.minPort;
    max-port = turn.maxPort;
    extraConfig = ''
      no-multicast-peers
      no-loopback-peers
      user=${turn.username}:${turn.credential}

      # Only allow relaying to LAN/VPN peers
      allowed-peer-ip=10.100.0.0-10.100.0.255
      allowed-peer-ip=192.168.1.0-192.168.1.255
    '';
  };

  # Interface-scoped firewall: only LAN (bond0) and VPN (wg0), not WAN
  networking.firewall.interfaces =
    let
      relay = {
        allowedUDPPorts = [ turn.listenPort ];
        allowedUDPPortRanges = [{ from = turn.minPort; to = turn.maxPort; }];
      };
    in
    {
      bond0 = relay;
      wg0 = relay;
    };
}
