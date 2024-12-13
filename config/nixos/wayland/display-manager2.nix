{ pkgs, lib, self, ... }:
let
  wallpapers = self.private.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "whale-sunset" "watch-tower" ];
  };
in
{
  # greetd display manager
  services.greetd = let
    session = {
      command = "${lib.getExe pkgs.greetd.tuigreet} --time --cmd niri-session";
    };
  in {
    enable = true;
    settings = {
      terminal.vt = 1;
      default_session = session;
      initial_session = session;
    };
  };

  # unlock GPG keyring on login
  security.pam.services.greetd.enableGnomeKeyring = true;
}
