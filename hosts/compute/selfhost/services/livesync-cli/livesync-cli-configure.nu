#!/usr/bin/env nu
# Reconciles the LiveSync CLI settings file from generated secrets.
#
# Built on `init-settings`, the CLI's own ~170-key baseline, so upstream's defaults stay upstream's.

let settings_file = $env.LIVESYNC_SETTINGS_FILE
let state_dir = $env.LIVESYNC_STATE_DIR
let owner = $"($env.LIVESYNC_USER):($env.LIVESYNC_GROUP)"

def main [] {
  if not ($settings_file | path exists) {
    print "Creating the settings baseline..."
    mkdir ($settings_file | path dirname)
    chown -R $owner $state_dir
    ^$env.LIVESYNC_PODMAN run --rm --pids-limit=128 --user $"($env.LIVESYNC_UID):($env.LIVESYNC_GID)" $"-v=($state_dir):/data" $env.LIVESYNC_IMAGE init-settings /data/.livesync/settings.json
  }

  print "Applying connection and encryption settings..."
  open $settings_file
  | upsert couchDB_URI $env.LIVESYNC_COUCHDB_URI
  | upsert couchDB_USER $env.LIVESYNC_COUCHDB_USER
  | upsert couchDB_PASSWORD (open --raw $env.LIVESYNC_COUCHDB_PASSWORD_FILE | str trim)
  | upsert couchDB_DBNAME $env.LIVESYNC_COUCHDB_DATABASE
  | upsert encrypt true
  | upsert passphrase (open --raw $env.LIVESYNC_PASSPHRASE_FILE | str trim)
  | upsert usePathObfuscation true
  # The baseline ships every trigger off, so without these two the daemon never reaches CouchDB.
  | upsert liveSync true
  | upsert syncOnStart true
  # Setup URIs give devices 60; a mismatch has both sides storing their own chunks of one file.
  | upsert customChunkSize 60
  | upsert isConfigured true
  | to json
  | save --force $settings_file

  chown -R $owner $state_dir
  chmod 600 $settings_file
  print "LiveSync CLI settings reconciled"
}
