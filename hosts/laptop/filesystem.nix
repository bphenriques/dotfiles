{ lib, pkgs, config, ... }:
let
  homeServerIp = "192.168.68.53";

  # Single-user mount for now. This should be reviewed to support multi-user as some folders are private.. but only when I have a new user.
  # See: https://4sysops.com/archives/linux-smb-mount-for-multiple-users/ and https://docs.redhat.com/en/documentation/red_hat_enterprise_linux/7/html/storage_administration_guide/mounting_an_smb_share#performing_a_multi-user_smb_mount
  mkHomeServerCifsFs = remoteFolder: user: group: let
    networkSplitProtectionOpts = [
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=15"
      "x-systemd.device-timeout=5s"
      "x-systemd.mount-timeout=5s"
    ];
    userOpts = [ "uid=${toString user.uid}" "gid=${toString group.gid}" ];
    credsOpts = [ "credentials=${config.sops.templates."smb-credentials".path}" ];
  in {
    device = "//${homeServerIp}/${remoteFolder}";
    fsType = "cifs"; # See https://nixos.wiki/wiki/Samba
    options = (userOpts ++ credsOpts ++ networkSplitProtectionOpts);
  };

  rootsGroup = config.users.groups.root;
  usersGroup = config.users.groups.users;
  bphenriquesPersistData = config.home-manager.users.bphenriques.custom.impermanence.dataLocation;
  bphenriquesPersistCache = config.home-manager.users.bphenriques.custom.impermanence.cacheLocation;
  bphenriquesUser = config.users.users.bphenriques;
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
      #owner = bphenriquesUser.name;
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
    "${bphenriquesPersistData}".neededForBoot = true;
    "${bphenriquesPersistCache}".neededForBoot = true;

    # NFS
    "/mnt/nas-bphenriques"  = mkHomeServerCifsFs "bphenriques" bphenriquesUser rootsGroup;  # Private
    "/mnt/nas-media"        = mkHomeServerCifsFs "media"       bphenriquesUser usersGroup;  # Shared with others
    "/mnt/nas-shared"       = mkHomeServerCifsFs "shared"      bphenriquesUser usersGroup;  # Shared with others
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.tmpfiles.rules = [
    # Fix permissions regarding root user
    "z ${config.sops.age.keyFile} 0700 root root" # nixos-anywhere sets the wrong permissions

    # Review CIFS permissions
    "z /mnt/nas-bphenriques   0700 ${bphenriquesUser.name}  ${usersGroup.name}"
    "z /mnt/nas-media         0775 root                     ${usersGroup.name}"
    "z /mnt/nas-shared        0775 root                     ${usersGroup.name}"

    # Review ZFS datasets permissions
    "z /mnt/games                   0775 root                     ${usersGroup.name}"
    "z /mnt/bphenriques             0700 ${bphenriquesUser.name}  ${usersGroup.name}"
    "z ${bphenriquesPersistData}    0700 ${bphenriquesUser.name}  ${usersGroup.name}"
    "z ${bphenriquesPersistCache}   0700 ${bphenriquesUser.name}  ${usersGroup.name}"
  ];
}
