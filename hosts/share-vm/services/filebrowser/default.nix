# FileBrowser: a single multi-user instance, localhost only — Traefik is the sole reverse
# proxy and the only thing that sets the trusted Remote-User header. Files live on the
# VM-owned /srv/share. Curation is done over sshfs as this user (see ../../default.nix),
# so the upstream module keeps its own 0700/0077 hardening untouched.
{ lib, ... }:
let
  inherit (import ../../lib.nix { inherit lib; }) filesRoot fbPort fbDb folders;
in
{
  imports = [ ./configure.nix ];

  # noexec/nosuid/nodev: the data volume holds others' uploads — data, never an
  # execution path, even inside the sealed VM (device/fsType come from microvm.nix).
  fileSystems."/srv/share".options = [ "noexec" "nosuid" "nodev" ];

  services.filebrowser = {
    enable = true;
    settings = {
      address = "127.0.0.1";
      port = fbPort;
      root = filesRoot;
      database = fbDb;
    };
  };
  # localhost-only egress: a compromised FileBrowser can't exfiltrate or reach tailnet/LAN.
  systemd.services.filebrowser.serviceConfig = {
    IPAddressDeny = "any";
    IPAddressAllow = "localhost";
  };

  # Pre-create the shared folders, owner-only like the module's root.
  systemd.tmpfiles.rules = map (f: "d ${filesRoot}/${f} 0700 filebrowser filebrowser -") folders;
}
