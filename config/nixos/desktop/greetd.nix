{ pkgs, config, lib, ... }:
{
  services.greetd = let
    options = ''--asterisks --time --remember'';
    session = {
      command = ''${lib.getExe pkgs.greetd.tuigreet} ${options}'';
      user = config.users.users.greeter.name;
    };
  in {
    enable = true;
    settings = {
      terminal.vt = 1;
      default_session = session;
      initial_session = session;
    };
  };

  security.pam.services.greetd.enableGnomeKeyring = true; # unlock GPG keyring on login
}
