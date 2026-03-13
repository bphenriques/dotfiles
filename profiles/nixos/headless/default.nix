{ lib, ... }: {
  # Auto-reboot on failure — override boot.shell_on_fail from base profile (would hang headless)
  boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];
  systemd.settings.Manager.RuntimeWatchdogSec = "30s";

  # Allow remote deployment via `nixos-rebuild --target-host root@...`
  users.users.root.openssh.authorizedKeys.keys = self.shared.authorizedSSHKeys;

  # Prevent accidental suspend/hibernate on headless server
  systemd.sleep.settings.Sleep = {
    AllowSuspend = "no";
    AllowHibernation = "no";
    AllowHybridSleep = "no";
    AllowSuspendThenHibernate = "no";
  };

  networking.useDHCP = false;
  networking.firewall.enable = true;
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      X11Forwarding = false;
      AllowAgentForwarding = false;
      AllowTcpForwarding = false;
      MaxAuthTries = 3;
      LoginGraceTime = "30s";
    };
  };
}

# TODO: https://blog.aldnav.com/blog/going-headless-with-nixos/
