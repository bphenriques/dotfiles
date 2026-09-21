{ lib, config, self, ... }:
{
  imports = [ ../settings.nix ];

  users.mutableUsers = false;

  # Security
  networking.firewall.enable = true;
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;   # PAM reaches the password stack through it despite the line above
      X11Forwarding = false;
      AllowAgentForwarding = false;
      AllowTcpForwarding = false;
      MaxAuthTries = 3;
      LoginGraceTime = "30s";
    };
  };
  services.resolved.settings.Resolve.LLMNR = false;   # nothing resolves through it; /etc/hosts + DNS cover the fleet
  security.sudo.extraConfig = "Defaults lecture=never";

  # Boot
  boot.tmp.cleanOnBoot = true;
  system.nixos.label =
    let date = self.lastModifiedDate or "00000000000000";
    in "${builtins.substring 2 6 date}-${builtins.substring 8 6 date}"; # Format: YYMMdd-HHmmss (e.g. 250415-194532)

  # Localization
  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_GB.UTF-8";

  # Secrets: Ensure the secret sops file's permission do not diverge
  systemd.tmpfiles.rules = lib.optional (config.sops.age.keyFile != null)
    "z ${config.sops.age.keyFile} 0600 root root -";

  # Disable defaults that are not required
  programs.nano.enable = false;
  documentation.nixos.enable = false;
}
