# The calendars Alfredo reads and writes, kept as a vdir. See ../README.md for the one-time Google sign-in.
{ config, lib, pkgs, alfredoVm, private, fleetFacts, ... }:
let
  state = "${alfredoVm.stateRoot}/vdirsyncer";   # the Google token and the sync status
  secret = name: config.sops.secrets."calendar/google/${name}".path;

  conf = pkgs.writeText "vdirsyncer.conf" ''
    [general]
    status_path = "${state}/status"

    [pair radicale]
    a = "radicale_remote"
    b = "radicale_local"
    collections = ["from a"]
    metadata = ["displayname"]
    conflict_resolution = "a wins"
    implicit = "create"

    # The owner's home: the account's own is empty, and its grant reaches only the owner's collections.
    [storage radicale_remote]
    type = "caldav"
    url = "${fleetFacts.publicUrls.radicale-dav}/${private.calendars.radicale}/"
    username = "personal-agent"
    password.fetch = ["command", "cat", "${config.sops.secrets."radicale/password".path}"]

    [storage radicale_local]
    type = "filesystem"
    path = "${alfredoVm.calendarsRoot}"
    fileext = ".ics"

    [pair google]
    a = "google_remote"
    b = "google_local"
    collections = ${builtins.toJSON private.calendars.google}
    metadata = ["displayname"]
    conflict_resolution = "a wins"
    implicit = "create"

    [storage google_remote]
    type = "google_calendar"
    token_file = "${state}/google-token.json"
    client_id.fetch = ["command", "cat", "${secret "clientId"}"]
    client_secret.fetch = ["command", "cat", "${secret "clientSecret"}"]

    [storage google_local]
    type = "filesystem"
    path = "${alfredoVm.calendarsRoot}"
    fileext = ".ics"
  '';

  vdirsyncer = pkgs.writeShellApplication {
    name = "vdirsyncer-alfredo";
    text = ''exec ${lib.getExe pkgs.vdirsyncer} -c ${conf} "$@"'';
  };

  # One sync at a time, the timer's or Alfredo's after a change: vdirsyncer has no lock.
  sync = pkgs.writeShellApplication {
    name = "calendars-sync";
    runtimeInputs = [ pkgs.util-linux vdirsyncer ];
    text = ''
      # Before the sign-in, sync would wait for a browser.
      [ -e "${state}/google-token.json" ] || { echo "Not signed in to Google yet"; exit 0; }
      exec 9>"${state}/lock"
      flock 9
      vdirsyncer-alfredo discover
      vdirsyncer-alfredo sync
      vdirsyncer-alfredo metasync
    '';
  };
in
{
  _module.args.calendarsSync = sync;

  environment.systemPackages = [ vdirsyncer ];   # `sudo -u hermes vdirsyncer-alfredo discover` signs in once

  sops.secrets."calendar/google/clientId".owner = "hermes";
  sops.secrets."calendar/google/clientSecret".owner = "hermes";
  sops.secrets."radicale/password".owner = "hermes";

  systemd.services.calendars-sync = {
    description = "Sync Alfredo's calendars with their servers";
    serviceConfig = {
      Type = "oneshot";
      User = "hermes";
      ExecStart = lib.getExe sync;
      ProtectSystem = "strict";
      ReadWritePaths = [ alfredoVm.stateRoot ];
      PrivateTmp = true;
      NoNewPrivileges = true;
    };
  };
  systemd.timers.calendars-sync = {
    wantedBy = [ "timers.target" ];
    # Counted from boot: the guest forgets timer state on every restart, so a calendar time missed while down never ran.
    timerConfig = { OnBootSec = "2min"; OnUnitActiveSec = "30min"; };
  };
}
