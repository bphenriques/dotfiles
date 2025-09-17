{ pkgs, config, lib, ... }:
let
  session = {
    command = ''${lib.getExe pkgs.tuigreet} --asterisks --time --remember'';
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
