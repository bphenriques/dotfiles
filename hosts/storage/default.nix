{
  config,
  inputs,
  lib,
  pkgs,
  private,
  ...
}:
{
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./hardware
    ./disko
    ./shares
    ./users.nix
    ./selfhost
    ./monitoring.nix
    ./ups.nix
    ../../profiles/nixos/base.nix
    ../../profiles/nixos/headless.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_6_18;
    supportedFilesystems.zfs = true;
    extraModprobeConfig = "options zfs zfs_arc_max=10737418240";
    zfs.forceImportRoot = false;
    loader.systemd-boot = {
      enable = true;
      editor = false;
      configurationLimit = 10;
    };
  };

  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "monthly";
    };
    trim.enable = true;
  };

  # Disko declares every dataset mountpoint, so systemd owns the mounts. `zfs mount -a` then races
  # them and loses with "mountpoint or dataset is busy".
  systemd.services.zfs-mount.enable = false;

  networking = {
    hostId = "192a778f";
    useDHCP = false;
    useNetworkd = true;
  };

  # Only the RTL8126A 5GbE port is configured; the i226-V stays down as a fallback to wire by hand.
  systemd.network.networks."10-lan" = {
    matchConfig.MACAddress = private.settings.network.lanMAC;
    networkConfig = {
      DHCP = "ipv4";
      IPv6AcceptRA = true;
    };
  };

  assertions = [
    {
      assertion = !lib.hasInfix "REPLACE_WITH" private.settings.network.lanMAC;
      message = "Storage 5GbE MAC is still a placeholder; fill in the private host settings.";
    }
  ];

  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.keyFile = "/var/lib/sops-nix/system-keys.txt";
  };

  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  environment.systemPackages = [ inputs.disko.packages.${pkgs.stdenv.hostPlatform.system}.disko ];

  system.stateVersion = "26.05";
}
