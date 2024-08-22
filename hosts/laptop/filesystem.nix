{ lib, pkgs, config, ... }:
let
  homeServerIp = "192.168.68.53";
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
    device = "//${homeServerIp}/${remoteFolder}";
    fsType = "cifs"; # See https://nixos.wiki/wiki/Samba
    options = (userOpts ++ credsOpts ++ networkSplitProtectionOpts);
  };
  bphenriquesData = config.home-manager.users.bphenriques.custom.impermanence.dataLocation;
  bphenriquesCache = config.home-manager.users.bphenriques.custom.impermanence.cacheLocation;
in
{
  # ZFS
  networking.hostId = "5b318853";
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };
    # https://github.com/prescientmoon/everything-nix/blob/5247160b4367f37a775d7278a52412f3cb0886ac/hosts/nixos/lapetus/services/zfs.nix#L10
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

  services.fstrim.enable = true;  # Trim SSD because it is not set by default :shrug:
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

    # ZFS Mounts
    "${bphenriquesData}".neededForBoot = true;
    "${bphenriquesCache}".neededForBoot = true;

    # NFS
    "/mnt/nas-bphenriques"  = mkHomeServerCifsFs "bphenriques";
    "/mnt/nas-media"        = mkHomeServerCifsFs "media";
    "/mnt/nas-shared"       = mkHomeServerCifsFs "shared";
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.tmpfiles.rules = [
    # Fix permissions regarding root user
    "z ${config.sops.age.keyFile} 0700 root  root" # nixos-anywhere sets the wrong permissions

    # Move permissions from root to the users group
    "z /mnt/games 0775 root users"

    # Move permissions from root to solely bphenriques
    "z /mnt/bphenriques     0700 bphenriques users"
    "z ${bphenriquesData}   0700 bphenriques users"
    "z ${bphenriquesCache}  0700 bphenriques users"
  ];
}
