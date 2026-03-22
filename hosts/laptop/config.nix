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
  networking.hosts = lib.foldlAttrs (acc: name: ip: acc // { ${ip} = (acc.${ip} or []) ++ [ name ]; }) {} self.shared.networks.main.hosts;
  custom.homelab.paths = {
    media.root = config.custom.homelab.smb.mounts.media.localMount;
    users.bphenriques.root = config.custom.homelab.smb.mounts.bphenriques.localMount;
  };
  custom.homelab.smb = {
    enable = true;
    hostname = self.shared.networks.main.hosts.bruno-home-nas;
    credentialsPath = config.sops.templates."homelab-samba-credentials".path;
    mounts = {
      bphenriques = { gid = 5190; };
      media = { gid = 5512; };
    };
  };
  sops = {
    secrets."homelab/samba/username" = { };
    secrets."homelab/samba/password" = { };
    templates."homelab-samba-credentials" = {
      owner = "root";
      group = "root";
      mode = "0400";
      content = ''
        username=${config.sops.placeholder."homelab/samba/username"}
        password=${config.sops.placeholder."homelab/samba/password"}
      '';
    };
  };

  # Secrets
  sops.defaultSopsFile = self.private.hosts.laptop.sopsSecretsFile;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
