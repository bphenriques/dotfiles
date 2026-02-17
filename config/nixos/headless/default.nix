_: {
  # Auto-reboot in case something wrong happens and ensure watchdog is enabled.
  boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];
  systemd.settings.Manager.RuntimeWatchdogSec = "30s";

  networking.firewall.enable = true;
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";  # Allow root with key only (needed for management/sync)
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      X11Forwarding = false;
      AllowAgentForwarding = false;
      AllowTcpForwarding = false;
    };
  };
}
