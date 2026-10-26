# The Cookidoo collection, copied into the recipe library daily; Alfred can also run it on request.
{ lib, alfred, alfredVm, ... }:
{
  systemd.services.cookidoo-sync = {
    description = "Copy the Cookidoo collection into the recipe library";
    unitConfig.RequiresMountsFor = [ alfredVm.recipesRoot ];
    serviceConfig = {
      Type = "oneshot";
      User = "hermes";
      ExecStart = "${lib.getExe alfred.cookidoo} sync";
      ProtectSystem = "strict";
      ReadWritePaths = [ alfredVm.stateRoot alfredVm.recipesRoot ];   # the token rotates; the copies land in the library
      PrivateTmp = true;
      NoNewPrivileges = true;
    };
  };
  systemd.timers.cookidoo-sync = {
    wantedBy = [ "timers.target" ];
    timerConfig = { OnCalendar = "daily"; RandomizedDelaySec = "1h"; Persistent = true; };
  };
}
