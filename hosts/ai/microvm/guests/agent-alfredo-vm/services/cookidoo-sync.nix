# The Cookidoo collection, copied into the recipe library daily; Alfredo can also run it on request.
{ lib, alfredo, alfredoVm, ... }:
{
  systemd.services.cookidoo-sync = {
    description = "Copy the Cookidoo collection into the recipe library";
    serviceConfig = {
      Type = "oneshot";
      User = "hermes";
      ExecStart = "${lib.getExe alfredo.cookidoo} sync";
      ProtectSystem = "strict";
      ReadWritePaths = [ alfredoVm.stateRoot alfredoVm.recipesRoot ];   # the token rotates; the copies land in the library
      PrivateTmp = true;
      NoNewPrivileges = true;
    };
  };
  systemd.timers.cookidoo-sync = {
    wantedBy = [ "timers.target" ];
    # Counted from boot: the guest forgets timer state on every restart, so a calendar time missed while down never ran.
    timerConfig = { OnBootSec = "15min"; OnUnitActiveSec = "1d"; };
  };
}
