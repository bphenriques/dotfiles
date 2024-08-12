{ lib, pkgs, config, ... }:
let
  mkHomeServerCifsFs = remoteFolder: let
    networkSplitProtectionOpts = [
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=15"
      "x-systemd.device-timeout=5s"
      "x-systemd.mount-timeout=5s"
    ];
    userOpts = [ "uid=${toString 1000}" "gid=${toString 100}" ];
    credsOpts = [ "credentials=${config.sops.templates."smb-credentials".path}" ];
  in {
    device = remoteFolder;
    fsType = "cifs"; # See https://nixos.wiki/wiki/Samba
    options = (userOpts ++ credsOpts ++ networkSplitProtectionOpts);
  };
in
{
  # ZFS
  networking.hostId = "5b318853";
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };
    #  services.sanoid = lib.mkIf cfg.snapshots {
    #    enable = true;
    #    datasets = {
    #      "zroot/persist" = {
    #        hourly = 50;
    #        daily = 15;
    #        weekly = 3;
    #        monthly = 1;
    #      };
    #    };
    #  };

  services.fstrim.enable = true;  # Trim SSD because for some reason is not a default :shrug:
  zramSwap.enable = true;         # Run zramctl to check how good memory is compressed

  custom.impermanence = {
    enable = true;
    rootBlankSnapshot = "zroot/system/root@blank";
    dataLocation = "/persist/data/system";
    cacheLocation = "/persist/cache/system";
  };

  sops = {
    secrets.samba_server_username = { };
    secrets.samba_server_password = { };
    templates."smb-credentials" = {
      owner = "bphenriques";
      content = ''
        username=${config.sops.placeholder.samba_server_username}
        password=${config.sops.placeholder.samba_server_password}
      '';
    };
  };

  environment.systemPackages = [ pkgs.cifs-utils ]; # Samba Server
  fileSystems = {
    # Disko sets boot.loader.grub.devices automatically.
    "/".neededForBoot = true;
    "/nix".neededForBoot = true;
    "/boot".neededForBoot = true;

    "/mnt/nas-bphenriques"  = mkHomeServerCifsFs "//192.168.68.53/bphenriques";
    "/mnt/nas-media"        = mkHomeServerCifsFs "//192.168.68.53/media";
    "/mnt/nas-shared"       = mkHomeServerCifsFs "//192.168.68.53/shared";
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.tmpfiles.rules = [
    "z /mnt/games 0775 root users"                          # Owned by root but can be used by any regular user

    "z /persist/data/bphenriques 0700 bphenriques users"    # Private to bphenriques
    "z /persist/cache/bphenriques 0700 bphenriques users"   # Private to bphenriques
    "z /home/bphenriques/workdir 0700 bphenriques users"    # Private to bphenriques
  ];
}
