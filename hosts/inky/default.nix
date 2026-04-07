{ lib, pkgs }:
let
  shared = import ../shared.nix;

  # SMB mounts: share name → gid (mirrors modules/nixos/homelab/smb.nix conventions)
  nasIP = shared.networks.main.hosts.bruno-home-nas;

  mounts = {
    media = { gid = 5001; };
  };

  smbOpts = [
    "vers=3.0" "credentials=/root/.smb-credentials" "uid=0"
    "nosuid" "nodev" "noexec"
    "_netdev" "x-systemd.automount" "noauto"
    "x-systemd.device-timeout=5s" "x-systemd.mount-timeout=5s"
    "file_mode=0660" "dir_mode=0770"
  ];

  smbFstabFragment = lib.concatStringsSep "\n" (lib.mapAttrsToList (name: cfg:
    let opts = smbOpts ++ [ "gid=${toString cfg.gid}" ];
    in "//${nasIP}/${name} /mnt/homelab-${name} cifs ${lib.concatStringsSep "," opts} 0 0"
  ) mounts);

  groupsEnv = lib.concatStringsSep "\n" (lib.mapAttrsToList (name: cfg:
    "HOMELAB_${lib.toUpper (builtins.replaceStrings ["-"] ["_"] name)}_GID=${toString cfg.gid}"
  ) mounts);

in
pkgs.runCommand "inky-setup" { } ''
  mkdir -p $out/config/generated

  cp -r --no-preserve=mode ${./files}/. $out/config/

  cat > $out/config/generated/smb.fstab.fragment <<'FSTAB'
${smbFstabFragment}
FSTAB

  cat > $out/config/generated/groups.env <<'GROUPS'
${groupsEnv}
GROUPS

  install -Dm755 ${./setup.sh} $out/setup.sh
''
