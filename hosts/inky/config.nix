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
  networking.hosts = lib.mapAttrs' (name: ip: lib.nameValuePair ip [ name ]) shared.networks.homelab.hosts;
  custom.homelab.cifs = {
    enable = true;
    hostname = shared.networks.homelab.hosts.bruno-home-nas;
    mounts = {
      bphenriques = { gid = 5000; };
      media = { gid = 5001; };
    };
  };

  # WiFi configuration - Pi Zero 2W uses wireless only
  networking.useDHCP = true;
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

  # Memory optimization for Pi Zero 2W (512MB RAM)
  zramSwap = {
    enable = true;
    memoryPercent = 50;
    algorithm = "zstd";
  };

  # Sound support for I2S DAC
  security.rtkit.enable = true;

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";
  sops.secrets."wifi/env" = { };

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11";
}
