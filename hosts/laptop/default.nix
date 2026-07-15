{ config, pkgs, inputs, private, ... }:
let
  primaryUser = "bphenriques";
  rootDisk = "/dev/disk/by-path/pci-0000:05:00.0-nvme-1";
in
{
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./hardware
    ./disko.nix
    ../../profiles/nixos/base.nix
    ../../profiles/nixos/home-manager.nix
    ../../profiles/nixos/graphical
    ../../profiles/nixos/capabilities/development.nix
    ../../profiles/nixos/capabilities/gaming
    ../../profiles/nixos/capabilities/selfhost-smb-client.nix
    ./share-mount.nix
    # Users
    ./bphenriques
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_7_1;
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

  # Homelab integration
  selfhost.storage.smb.mounts = {
    bphenriques = { uid = config.users.users.bphenriques.uid; gid = 5190; };
    media = { uid = config.users.users.bphenriques.uid; gid = 5512; };
  };

  # Secrets
  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.keyFile = "/var/lib/sops-nix/system-keys.txt";
  };

  # Users
  nix.settings.trusted-users = [ config.users.users.${primaryUser}.name ];

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
