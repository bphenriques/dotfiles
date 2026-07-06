{ config, inputs, private, shareVm, ... }:
let
  inherit (shareVm) filesRoot;
in
{
  imports = [ inputs.selfhost-nix.nixosModules.filebrowser-multiuser ];

  services.filebrowser = {
    enable = true;
    settings = {
      address = "127.0.0.1";
      port = 8085;
      root = filesRoot;
      branding = {
        files = ./branding;
        disableExternal = true;
        disableUsedPercentage = true;
      };
      viewMode = "mosaic";
      singleClick = true;
      hideDotfiles = true;
      sorting = { by = "modified"; asc = false; };
    };
  };

  services.filebrowser-multiuser = {
    enable = true;
    inherit (private.settings.filebrowser) users;
    unlistedScope = "/.unlisted"; # moot — BasicAuth admits only listed users; safe sentinel if not
  };

  # Host-created storage (folders declared privately); content is curated via laptop sshfs.
  systemd.tmpfiles.rules = builtins.map
    (f: "d ${filesRoot}/${f} 0700 ${config.services.filebrowser.user} ${config.services.filebrowser.group} -")
    private.settings.filebrowser.folders;

  # the data volume holds others' uploads — data, never an execution path
  fileSystems.${filesRoot}.options = [ "noexec" "nosuid" "nodev" ];
  # localhost-only egress: a compromised FileBrowser can't exfiltrate or reach tailnet/LAN
  systemd.services.filebrowser.serviceConfig = { IPAddressDeny = "any"; IPAddressAllow = "localhost"; };
}
