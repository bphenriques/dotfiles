{ config, pkgs, lib, ... }:
let
  nuLib = import ../lib/nu.nix { inherit pkgs lib; };
  serviceCfg = config.custom.home-server.services.pocket-id;
  apiKeyFile = config.sops.templates."pocket-id-api-key".path;

  enabledUsers = lib.filterAttrs (_: u: u.services.pocket-id.enable) config.custom.home-server.users;
  users = lib.mapAttrsToList (_: u: {
    inherit (u) username email;
    firstName = u.name;
    lastName = "";
    groups = u.services.pocket-id.groups;
    isAdmin = builtins.elem "admins" u.services.pocket-id.groups;
  }) enabledUsers;

  allGroups = lib.unique (lib.concatLists (lib.mapAttrsToList (_: u: u.services.pocket-id.groups) enabledUsers));
  groups = map (name: { inherit name; }) allGroups;

  configFile = pkgs.writeText "pocket-id-config.json" (builtins.toJSON { inherit users groups; });

  initScript = nuLib.checkedScript "pocket-id-init" ./pocket-id-init.nu;
  inviteScript = nuLib.checkedScript "pocket-id-invite" ./pocket-id-invite.nu;
in
{
  systemd.services.pocket-id-init = {
    description = "Initialize Pocket ID users and groups";
    wantedBy = [ "multi-user.target" ];
    after = [ "pocket-id.service" ];
    requires = [ "pocket-id.service" ];
    partOf = [ "pocket-id.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = config.services.pocket-id.user;
      Group = config.services.pocket-id.group;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      POCKET_ID_URL = serviceCfg.internalUrl;
      POCKET_ID_API_KEY_FILE = apiKeyFile;
      POCKET_ID_CONFIG_FILE = configFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${initScript}'';
  };

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "pocket-id-invite" ''
      export POCKET_ID_URL="${serviceCfg.internalUrl}"
      export POCKET_ID_API_KEY_FILE="${apiKeyFile}"
      exec ${lib.getExe pkgs.nushell} ${inviteScript} "$@"
    '')
  ];
}
