# Universal floor: policy every machine in the fleet gets, sealed microVM guests included.
# Anything needing a writable store, its own boot, persistent logs or an operator belongs in full-host.nix.
{ ... }:
{
  imports = [ ../settings.nix ];

  users.mutableUsers = false;

  # Security. PermitRootLogin is left to the consumer: hosts allow key-only root for remote
  # deployment, guests forbid it outright.
  services.openssh = {
    enable = true;
    settings = {
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

  # Localization. The pt_PT LC_* set is an operator concern and lives in full-host.nix.
  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_GB.UTF-8";

  # Disabled defaults
  programs.nano.enable = false;
  documentation.nixos.enable = false;  # Disable generating NixOS configuration options
}
