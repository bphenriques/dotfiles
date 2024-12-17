{ pkgs, config, lib, ... }:
{
  services.greetd = let
    theme = "--theme border=magenta;text=cyan;prompt=green;time=red;action=blue;button=yellow;container=black;input=red";
    options = ''--user-menu --asterisks --time --asterisks --greeting "Hi!" --remember --remember-session'';
    session = {
      command = ''${lib.getExe pkgs.greetd.tuigreet} ${theme} ${options} --cmd niri-session''; # FIXME: Hardcoded?
      user = config.users.users.bphenriques.name; # FIXME: Hardcoded?
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
