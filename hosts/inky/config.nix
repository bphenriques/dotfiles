{ config, pkgs, lib, ... }:
let
  shared = import ../shared.nix;
in
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
  networking.hosts = lib.mapAttrs' (name: ip: lib.nameValuePair ip [ name ]) shared.networks.main.hosts;
  custom.homelab.cifs = {
    enable = true;
    hostname = shared.networks.main.hosts.bruno-home-nas;
    mounts = {
      bphenriques = { gid = 5000; };
      media = { gid = 5001; };
    };
  };

  # WiFi configuration - Pi Zero 2W uses wireless only
  networking.nameservers = [ shared.dns.cloudflare ];
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
  systemd.watchdog = {
    runtimeTime = "30s";
    rebootTime = "10min";
  };

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";
  sops.secrets."wifi/env" = { };

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11";
}
