{ config, pkgs, ... }:
let
  cfg = config.selfhost;
  selfhostMounts = cfg.storage.mounts.smb.shares;
  img = pkgs.containerImages.livesync-cli;

  stateDir = "/var/lib/livesync-cli";
  vault = import ./vault.nix;
  vaultDir = "${config.fleet.shares.${vault.owner}.root}/${vault.subpath}";

  livesyncUser = {
    name = "livesync-cli";
    uid = 1984;
    group = "livesync-cli";
    gid = 1984;
  };
in
{
  imports = [ ./configure.nix ./setup-uri.nix ];

  selfhost = {
    # No port: the daemon speaks out to CouchDB and has no backend of its own. The entry is here for
    # the automount ordering and the failure notification.
    services.livesync-cli = {
      displayName = "LiveSync";
      meta.homepage = "https://github.com/vrtmrz/obsidian-livesync";
      meta.description = "Obsidian vault sync daemon";
      storage.mounts = [ vault.owner ];
      storage.users = [ livesyncUser.name ];
    };

    storage.mounts.smb.shares.${vault.owner}.systemd.dependentServices = [ "podman-livesync-cli" ];
  };

  users.groups.${livesyncUser.group} = { inherit (livesyncUser) gid; };
  users.users.${livesyncUser.name} = {
    inherit (livesyncUser) uid group;
    isSystemUser = true;
  };

  systemd.tmpfiles.rules = [
    "d ${stateDir} 0750 ${livesyncUser.name} ${livesyncUser.group} -"
  ];

  systemd.services.podman-livesync-cli = {
    after = [ "livesync-cli-configure.service" ];
    requires = [ "livesync-cli-configure.service" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  };

  virtualisation.oci-containers.containers.livesync-cli = {
    image = "${img.image}:${img.version}-cli";
    autoStart = true;
    # The entrypoint prepends the database path, so this reads `/data --vault /vault daemon`.
    cmd = [ "--vault" "/vault" "daemon" ];
    volumes = [
      "${stateDir}:/data"
      "${vaultDir}:/vault"
    ];
    user = "${toString livesyncUser.uid}:${toString livesyncUser.gid}";
    extraOptions = [
      "--memory=2g" # PouchDB holds the whole vault; the mirror scan at start is the peak
      "--pids-limit=128"
      # CIFS forces root:homelab-bphenriques 0660, so the vault is reachable by group alone.
      "--group-add=${toString selfhostMounts.${vault.owner}.gid}"
      "--add-host=${cfg.services.couchdb.publicHost}:${config.fleet.lan.hosts.compute}"
    ];
  };
}
