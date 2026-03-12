{ config, pkgs, lib, self, ... }:
# TODO: Fuzzel supports message now
{
  imports = [
    ./hardware
    ./disko.nix
    ../../profiles/nixos
    ../../profiles/nixos/desktop

    # Users
    ./bphenriques

    # Temporary
    ../homelab/compute/datastores
    ../homelab/compute/services
    #../homelab/compute/tasks
  ];

  networking.hostName = "bphenriques-laptop";

  boot = {
    kernelPackages = pkgs.linuxPackages_6_18;

    initrd.systemd.enable = true; # Hibernation
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
  networking.hosts = lib.mapAttrs' (name: ip: lib.nameValuePair ip [ name ]) self.shared.networks.main.hosts;
  custom.homelab.smb = {
    enable = true;
    hostname = self.shared.networks.main.hosts.bruno-home-nas;
    mounts = {
      bphenriques = { gid = 5190; };
      media = { gid = 5512; };
    };
  };

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
