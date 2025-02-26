{ pkgs, config, lib, ... }:
let
  theme = lib.concatStringsSep ";" [
    "text=white"
    "time=white"
    "container=darkgray"
    "border=green"
    "title=magenta"
    "greet=magenta"
    "prompt=white"
    "input=gray"
    "action=gray"
    "button=cyan"
  ];
in {
  services.greetd = let
    options = ''--asterisks --time --remember --theme ${theme}'';
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
