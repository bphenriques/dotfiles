{ pkgs, lib, config, ... }: {
  # Auto-reboot on failure
  boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];
  systemd.settings.Manager.RuntimeWatchdogSec = "30s";

  # Allow remote deployment via `nixos-rebuild --target-host root@...`
  users.users.root.openssh.authorizedKeys.keys = config.custom.fleet.ssh.authorizedKeys;

  # Prevent accidental suspend/hibernate on headless server
  systemd.sleep.settings.Sleep = {
    AllowSuspend = "no";
    AllowHibernation = "no";
    AllowHybridSleep = "no";
    AllowSuspendThenHibernate = "no";
  };

  # Auto-GC under disk pressure
  nix.settings = {
    min-free = 10 * 1024 * 1024 * 1024;  # start collecting under 10 GiB free
    max-free = 50 * 1024 * 1024 * 1024;  # ...until 50 GiB is free
  };

  environment.systemPackages = [ pkgs.nvd ]; # Remote changelog diffing
}
