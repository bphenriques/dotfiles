{ lib, pkgs, ... }:
let
  fanIdleDuty = pkgs.writeShellApplication {
    name = "fan-idle-duty";
    runtimeInputs = [ pkgs.coreutils pkgs.gawk ];
    runtimeEnv.IDLE_DUTY = "10";
    text = builtins.readFile ./fan-curve.sh;
  };
in
{
  boot = {
    kernelModules = [ "ec_sys" ];
    extraModprobeConfig = "options ec_sys write_support=1";
  };

  environment.systemPackages = [ fanIdleDuty ];

  systemd.services.fan-idle-duty = {
    description = "Lower the EC fan duty floor below 45C";
    wantedBy = [ "multi-user.target" ];
    unitConfig.RequiresMountsFor = "/sys/kernel/debug";
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;   # the EC reverts to its stock curve on power cycle, not on daemon exit
      ExecStart = lib.getExe fanIdleDuty;
    };
  };
}
