{ pkgs, config, lib, ... }:
{
  services.greetd = let
    # FIXME: actually set a theme
    theme = "--theme border=magenta;text=cyan;prompt=green;time=red;action=blue;button=yellow;container=black;input=red";
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
