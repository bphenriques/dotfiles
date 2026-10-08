{ config, pkgs, inputs, private, ... }:
let
  rootDisk = "/dev/disk/by-path/pci-0000:05:00.0-nvme-1";
in
{
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./hardware
    ./disko.nix
    ../../profiles/nixos/home-manager.nix
    ../../profiles/nixos/graphical
    ../../profiles/nixos/development.nix
    ../../profiles/nixos/gaming
    ../../profiles/nixos/selfhost-smb-client.nix

    # Users
    ./bphenriques
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_7_2;
    loader = {
      timeout = 0;  # The menu can be shown by pressing and holding a key before systemd-boot is launched.
      systemd-boot = {
        enable = true;
        editor = false;
        consoleMode = "max";
        configurationLimit = 10;
        windows."Windows" = {
          title = "Windows";
          efiDeviceHandle = "HD0b";
        };
      };
    };

    # Hibernation: resume from btrfs swapfile on the root partition. To get the offset: sudo btrfs inspect-internal map-swapfile -r /.swapvol/swapfile
    initrd.systemd.enable = true;
    resumeDevice = "${rootDisk}-part2";
    kernelParams = [ "boot.shell_on_fail" "resume_offset=533760" ];
  };

  # Networking: rank ethernet below wifi for the default route (NetworkManager's wifi metric is 600).
  networking.networkmanager.ensureProfiles.profiles.lan = {
    connection = { id = "lan"; type = "ethernet"; };
    ipv4 = { method = "auto"; route-metric = 700; dns-priority = 200; };
    ipv6 = { method = "auto"; route-metric = 700; dns-priority = 200; };
  };

  # Homelab integration
  selfhost.storage.mounts.smb.shares = {
    bphenriques = { uid = config.users.users.bphenriques.uid; gid = 5000; };
    media = { uid = config.users.users.bphenriques.uid; gid = 5001; };
  };
  # Secrets
  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.keyFile = "/var/lib/sops-nix/system-keys.txt";
  };

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
