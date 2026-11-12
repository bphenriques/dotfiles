{ lib, pkgs, config, self, osConfig, ... }:
let
  inherit (builtins) listToAttrs;
  inherit (lib) nameValuePair attrNames imap0;

  cfg = config.my.programs.session;

  mkAppOpt = default: lib.mkOption { type = lib.types.str; inherit default; };
  menuKey = "q";

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  systemctl = lib.getExe' pkgs.systemd "systemctl";
  windows = osConfig.boot.loader.systemd-boot.windows;
  # Shift letters, as everything that ends the session (W first); nixpkgs names each entry's file after its attribute.
  systemd-boot-windows = lib.optionals osConfig.boot.loader.systemd-boot.enable
    (imap0 (i: name: {
      id = "session-${name}";
      key = lib.elemAt [ "W" "V" "X" "Y" "Z" ] i;
      symbol = self.lib.glyphs."md-microsoft_windows";
      label = "Reboot to ${windows.${name}.title}";
      exec = ''${systemctl} reboot --boot-loader-entry="windows_${name}.conf"''; # systemctl reboot --boot-loader-entry=help
    }) (attrNames windows));
  sessionActions = [
    { id = "session-lock";      key = "l"; symbol = self.lib.glyphs."md-lock"; label = "Lock";                 exec = cfg.exec.lock; }
    { id = "session-suspend";   key = "s"; symbol = self.lib.glyphs."md-power_sleep"; label = "Suspend";              exec = cfg.exec.suspend; }
    { id = "session-shutdown";  key = "S"; symbol = self.lib.glyphs."md-power"; label = "Shutdown";             exec = cfg.exec.shutdown; }
    { id = "session-reboot";    key = "R"; symbol = self.lib.glyphs."md-restart"; label = "Reboot";               exec = cfg.exec.reboot; }
  ] ++ systemd-boot-windows ++ [
    { id = "session-efi";       key = "E"; symbol = self.lib.glyphs."md-chip"; label = "Reboot to EFI setup";  exec = cfg.exec.reboot-efi; }
  ];
in
{
  options.my.programs.session = {
    enable = lib.mkEnableOption "custom-session";
    exec = {
      menu       = mkAppOpt "${config.my.programs.desktop-shell.exec.ipc} menu toggleEntry ${menuKey}";
      lock       = lib.mkOption { type = lib.types.str; };   # comes from the lock-screen profile
      suspend    = mkAppOpt "${systemctl} suspend";
      shutdown   = mkAppOpt "${systemctl} poweroff";
      reboot     = mkAppOpt "${systemctl} reboot";
      reboot-efi = mkAppOpt "${systemctl} reboot --firmware-setup";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "my.programs.session" pkgs lib.platforms.linux) ];
    home.packages = [
      (pkgs.makeDesktopItem {
        name = "session-menu";
        desktopName = "Session";
        icon = mkIcon "session" self.lib.glyphs."md-account";
        exec = cfg.exec.menu;
        actions = let
          toAction = b: nameValuePair b.id {
            name = b.label;
            icon = mkIcon b.id b.symbol;
            inherit (b) exec;
          };
        in listToAttrs (lib.map toAction sessionActions);
      })
    ];

    my.programs.desktop-shell.menu.entries = lib.mkOrder 2000 [
      { key = menuKey; desc = "Session"; width = 640; icon = (lib.findFirst (a: a.id == "session-shutdown") null sessionActions).symbol; submenu = map (a: { inherit (a) key; desc = a.label; cmd = a.exec; icon = a.symbol; }) sessionActions; }
    ];
  };
}
