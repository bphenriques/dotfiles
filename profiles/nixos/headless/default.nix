{ pkgs, lib, config, ... }: {
  # Auto-reboot on failure
  boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];
  systemd.settings.Manager.RuntimeWatchdogSec = "30s";

  # Allow remote deployment via `nixos-rebuild --target-host root@...`
  users.users.root.openssh.authorizedKeys.keys = config.custom.fleet.authorizedSSHKeys;

  # Prevent accidental suspend/hibernate on headless server
  systemd.sleep.settings.Sleep = {
    AllowSuspend = "no";
    AllowHibernation = "no";
    AllowHybridSleep = "no";
    AllowSuspendThenHibernate = "no";
  };

  networking.useDHCP = false;

  environment.systemPackages = [ pkgs.nvd ]; # Remote changelog diffing
}
