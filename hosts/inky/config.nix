{ config, pkgs, lib, self, ... }:
{
  imports = [
    # Base
    ./hardware
    ./services
    ../../profiles/nixos
    ../../profiles/nixos/headless

    # Users
    ./bphenriques
  ];

  networking.hostName = "inky";

  # Homelab integration
  networking.hosts = lib.foldlAttrs (acc: name: ip: acc // { ${ip} = (acc.${ip} or []) ++ [ name ]; }) {} self.shared.networks.main.hosts;
  custom.homelab.smb = {
    enable = true;
    hostname = self.shared.networks.main.hosts.bruno-home-nas;
    credentialsPath = config.sops.templates."homelab-samba-credentials".path;
    mounts = {
      bphenriques = { gid = 5000; };
      media = { gid = 5001; };
    };
  };
  sops.secrets."homelab/samba/username" = { };
  sops.secrets."homelab/samba/password" = { };
  sops.templates."homelab-samba-credentials" = {
    owner = "root";
    group = "root";
    mode = "0400";
    content = ''
      username=${config.sops.placeholder."homelab/samba/username"}
      password=${config.sops.placeholder."homelab/samba/password"}
    '';
  };

  # Media paths (derived from SMB mount)
  custom.homelab.paths = {
    media.root = config.custom.homelab.smb.mounts.media.localMount;
  };

  # WiFi configuration - Pi Zero 2W uses wireless only
  networking.nameservers = [ self.shared.dns.cloudflare ];
  networking.wireless = {
    enable = true;
    interfaces = [ "wlan0" ];
    secretsFile = config.sops.secrets."wifi/env".path;
    networks = {
      "@WIFI_SSID@" = {
        pskRaw = "ext:WIFI_PASSWORD";
      };
    };
  };

  # Don't block boot waiting for ethernet (WiFi only)
  systemd.network.wait-online.anyInterface = true;

  # Memory optimization for Pi Zero 2W (512MB RAM)
  zramSwap = {
    enable = true;
    memoryPercent = 50;
    algorithm = "zstd";
  };

  # SD card wear reduction - volatile logs
  services.journald.extraConfig = ''
    Storage=volatile
    RuntimeMaxUse=16M
  '';

  # Disable rtkit (not needed without PipeWire/PulseAudio)
  security.rtkit.enable = false;

  # Power management - scale CPU frequency based on load
  powerManagement.cpuFreqGovernor = "ondemand";

  # Watchdog for automatic recovery from hangs
  systemd.watchdog.rebootTime = "10min";

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";
  sops.secrets."wifi/env" = { };

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11";
}
