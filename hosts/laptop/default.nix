{ config, pkgs, private, ... }:
let
  primaryUser = "bphenriques";
  rootDisk = "/dev/disk/by-path/pci-0000:05:00.0-nvme-1";
in
{
  imports = [
    ./hardware
    ./disko.nix
    ../../profiles/nixos
    ../../profiles/nixos/desktop
    ../../profiles/nixos/development
    ../../profiles/nixos/gaming
    ../../profiles/nixos/selfhost-smb-client.nix
    # Users
    ./bphenriques
  ];

  custom.fleet = import ../shared.nix;

  networking.hostName = "bphenriques-laptop";

  boot = {
    kernelPackages = pkgs.linuxPackages_7_0; # Intentionally not LTS as I want to keep up at my own pace

    initrd.systemd.enable = true;

    # Hibernation: resume from btrfs swapfile on the root partition.
    # To get the offset: sudo btrfs inspect-internal map-swapfile -r /.swapvol/swapfile
    resumeDevice = "${rootDisk}-part2";
    kernelParams = [ "boot.shell_on_fail" "resume_offset=533760" ];
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
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.${primaryUser}.name ];

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
