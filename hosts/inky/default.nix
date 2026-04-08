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

  # InkyPi device.json defaults
  inkypi = {
    timezone = "Europe/Lisbon";
    timeFormat = "24h";
    inkySaturation = 0;
  };

  setupEnv = {
    HOMELAB_MEDIA_GID = toString mounts.media.gid;
    INKYPI_TIMEZONE = inkypi.timezone;
    INKYPI_TIME_FORMAT = inkypi.timeFormat;
    INKYPI_INKY_SATURATION = toString inkypi.inkySaturation;
  };

  setupEnvFile = lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "${k}=${v}") setupEnv);

in
pkgs.runCommand "inky-setup" { } ''
  mkdir -p $out/config/generated

  cp -r --no-preserve=mode ${./files}/. $out/config/

  cat > $out/config/generated/smb.fstab.fragment <<'FSTAB'
${smbFstabFragment}
FSTAB

  cat > $out/config/generated/setup.env <<'ENV'
${setupEnvFile}
ENV

  install -Dm755 ${./setup.sh} $out/setup.sh
''
