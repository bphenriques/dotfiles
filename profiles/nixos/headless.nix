{ pkgs, lib, config, ... }: {
  imports = [ ./standalone.nix ];

  # Auto-reboot on failure, and fall back to the previous generation after repeated failed boots
  boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];
  systemd.settings.Manager.RuntimeWatchdogSec = "30s";
  boot.loader.systemd-boot.bootCounting.enable = true;

  # Allow remote deployment via `nixos-rebuild --target-host root@...`. Still gated behind SSH key.
  services.openssh.settings.PermitRootLogin = lib.mkForce "prohibit-password";
  users.users.root.openssh.authorizedKeys.keys = config.fleet.ssh.authorizedKeys;

  # Prevent accidental suspend/hibernate
  systemd.sleep.settings.Sleep = {
    AllowSuspend = "no";
    AllowHibernation = "no";
    AllowHybridSleep = "no";
    AllowSuspendThenHibernate = "no";
  };

  environment.systemPackages = [ pkgs.nvd ]; # Remote changelog diffing
}
