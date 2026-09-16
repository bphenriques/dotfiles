{ config, pkgs, self, ... }:
let
  cfg = config.selfhost;
  img = pkgs.containerImages.livesync-cli;

  livesyncUser = config.users.users.livesync-cli;
  stateDir = "/var/lib/livesync-cli";

  vault = import ./vault.nix;
in
{
  systemd.services.livesync-cli-configure = {
    description = "LiveSync CLI setup";
    wantedBy = [ "podman-livesync-cli.service" ];
    before = [ "podman-livesync-cli.service" ];
    restartTriggers = [ ./livesync-cli-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 600;
      Restart = "on-failure";
      RestartSec = 10;
      UMask = "0077";
    };
    environment = {
      LIVESYNC_SETTINGS_FILE = "${stateDir}/.livesync/settings.json";
      LIVESYNC_STATE_DIR = stateDir;
      LIVESYNC_IMAGE = "${img.image}:${img.version}-cli";
      LIVESYNC_PODMAN = "${pkgs.podman}/bin/podman";
      LIVESYNC_USER = livesyncUser.name;
      LIVESYNC_GROUP = livesyncUser.group;
      LIVESYNC_UID = toString livesyncUser.uid;
      LIVESYNC_GID = toString config.users.groups.livesync-cli.gid;
      LIVESYNC_COUCHDB_URI = cfg.services.couchdb.publicUrl;
      LIVESYNC_COUCHDB_USER = vault.owner;
      LIVESYNC_COUCHDB_DATABASE = vault.database;
      LIVESYNC_COUCHDB_PASSWORD_FILE = cfg.runtimeSecrets."couchdb-password-${vault.owner}".path;
      LIVESYNC_PASSPHRASE_FILE = cfg.runtimeSecrets.livesync-vault-passphrase.path;
    };
    path = [ pkgs.nushell pkgs.coreutils ];
    script = ''nu ${self.lib.builders.writeNushellScript "livesync-cli-configure" ./livesync-cli-configure.nu}'';
  };

  selfhost.runtimeSecrets = {
    # Never silently replaced: a new value would not decrypt what CouchDB already holds. If this and
    # the daemon's database are both lost, reseed from the vault copy in B2.
    livesync-vault-passphrase = {
      bytes = 32;
      generateOnce = "${stateDir}/.livesync";
      restartUnits = [ "livesync-cli-configure.service" ];
    };
    "couchdb-password-${vault.owner}".restartUnits = [ "livesync-cli-configure.service" ];
  };
}
