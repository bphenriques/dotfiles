{ pkgs, config, lib, ... }:
let
  # tuigreet 0.11 no longer falls back to XDG_DATA_DIRS, so NixOS sessions must be pointed at explicitly.
  sessions = "${config.services.displayManager.sessionData.desktops}/share/wayland-sessions";
  session = {
    command = ''${lib.getExe pkgs.tuigreet} --asterisks --time --remember --sessions ${sessions}'';
    user = config.users.users.greeter.name;
  };
in
{
  services.greetd = {
    enable = true;
    settings = {
      terminal.vt = 1;
      default_session = session;
      initial_session = session;
    };
  };

  security.pam.services.greetd.enableGnomeKeyring = true; # unlock GPG keyring on login
}
