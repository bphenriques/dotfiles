{ config, pkgs, ... }:
let
  cfg = config.selfhost;
  img = pkgs.containerImages.livesync-cli;

  vault = import ./vault.nix;

  livesync-setup-uri = pkgs.writeShellApplication {
    name = "livesync-setup-uri";
    runtimeInputs = [ pkgs.deno ];
    text = ''
      export LIVESYNC_VERSION="${img.version}"
      export LIVESYNC_COUCHDB_URI="${cfg.services.couchdb.publicUrl}"
      export LIVESYNC_COUCHDB_USER="${vault.owner}"
      export LIVESYNC_COUCHDB_DATABASE="${vault.database}"
      export LIVESYNC_COUCHDB_PASSWORD_FILE="${cfg.runtimeSecrets."couchdb-password-${vault.owner}".path}"
      export LIVESYNC_PASSPHRASE_FILE="${cfg.runtimeSecrets.livesync-vault-passphrase.path}"
      # shellcheck disable=SC1091
      source ${./setup-uri.sh}
    '';
    meta.description = "Mint an Obsidian LiveSync Setup URI for a new device";
  };
in
{
  environment.systemPackages = [ livesync-setup-uri ];
}
