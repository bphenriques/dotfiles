# Shared constants + the access model, imported by default.nix / funnel.nix / services
# (same pattern as the grafana dashboards' lib.nix).
{ lib }:
rec {
  dataRoot = "/var/lib/share";   # state volume: creds + SSH host key (sops age identity)
  filesRoot = "/srv/share";      # VM-owned data volume (see microvm.nix)
  credsDir = "${dataRoot}/.credentials";
  htpasswd = "${credsDir}/htpasswd";
  sshHostKey = "${dataRoot}/.ssh-host-keys/ssh_host_ed25519_key";
  fbDb = "/var/lib/filebrowser/filebrowser.db";  # ephemeral (tmpfs): re-seeded each boot, so removed scopes drop

  fbPort = 8085;     # FileBrowser, localhost only
  proxyPort = 8080;  # public edge (Traefik web entrypoint): Funnel forwards here (PROXY protocol)

  # Nightly dark window (local time, time.timeZone = Europe/Lisbon): the Funnel is closed,
  # shrinking the public attack surface to waking hours. HHMM, no midnight wrap.
  darkStart = "0200";
  darkEnd = "0700";

  # Folders under /srv/share, created on boot. Independent of users: a folder can be
  # shared by several users, and a username need not match a folder.
  folders = [ "bob" "mary" "family" "grandparents" ];

  # BasicAuth accounts, each mapped to a scope + access level (decoupled from folder
  # names — same shape compute's FileBrowser emits, just sourced locally not from OIDC):
  #   folder = "<name>"   scope to /srv/share/<name>   (must be in `folders` above)
  #   folder = null       scope to "/" — sees every folder
  #   readOnly = true     download only (no upload/rename/delete)
  users = {
    bob          = { folder = "bob"; };
    mary         = { folder = "mary"; };
    family       = { folder = "family"; };
    grandparents = { folder = "grandparents"; };
    partner      = { folder = null; readOnly = true; };
  };

  fbUsers = lib.mapAttrsToList (name: u: {
    inherit name;
    scope = if (u.folder or null) == null then "/" else "/${u.folder}";
    readOnly = u.readOnly or false;
  }) users;

  publicNames = lib.concatStringsSep " " (lib.attrNames users);
}
