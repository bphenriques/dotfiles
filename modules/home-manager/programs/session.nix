{ lib, pkgs, config, self, osConfig, ... }:
let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map nameValuePair;

  cfg = config.custom.programs.session;

  mkAppOpt = lib.mkOption {
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };
  mkAppOpt' = default: mkAppOpt // { inherit default; };

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };
  mkIcon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; };

  systemctl = lib.getExe' pkgs.systemd "systemctl";
  sessionActions = [
    { id = "session-lock";      symbol = ""; label = "Lock";                 exec = cfg.exec.lock; }
    { id = "session-suspend";   symbol = "󰤄"; label = "Suspend";              exec = cfg.exec.suspend; }
    { id = "session-shutdown";  symbol = ""; label = "Shutdown";             exec = cfg.exec.shutdown; }
    { id = "session-reboot";    symbol = ""; label = "Reboot";               exec = cfg.exec.reboot; }
    { id = "session-efi";       symbol = ""; label = "Reboot to EFI setup";  exec = cfg.exec.reboot-efi; }
  ] ++ lib.optionals (osConfig.custom.boot.grub.windows.efiDevice != "") [
    { id = "session-windows";   symbol = ""; label = "Reboot to Windows";    exec = cfg.exec.reboot-windows; }
  ];

  dmenu = self.lib.builders.writeDmenuApplication pkgs {
    name = "session-dmenu";
    entries = lib.map (e: { inherit (e) exec; label = "${e.symbol}     ${e.label}"; }) sessionActions;
  };
in
{
  options.custom.programs.session = {
    enable = lib.mkEnableOption "custom-session";
    exec = {
      menu              = mkAppOpt' ''${lib.getExe dmenu}'';
      lock              = mkAppOpt;
      suspend           = mkAppOpt' "${systemctl} suspend";
      shutdown          = mkAppOpt' "${systemctl} poweroff";
      reboot            = mkAppOpt' "${systemctl} reboot";
      reboot-efi        = mkAppOpt' "${systemctl} reboot --firmware-setup";
      reboot-windows    = mkAppOpt' osConfig.custom.boot.grub.windows.rebootPackage;
    };
  };

  config = lib.mkIf cfg.enable {
   assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.session" pkgs lib.platforms.linux) ];
    home.packages = [
      dmenu
      (pkgs.makeDesktopItem {
        name = "session-menu";
        desktopName = "Session";
        icon = mkIcon "session" "";
        exec = lib.getExe dmenu;
        actions = let
          toAction = b: nameValuePair b.id {
            name = b.label;
            icon = mkIcon b.id b.symbol;
            exec = b.exec;
          };
        in listToAttrs (lib.map toAction sessionActions);
      })
    ];
  };
}
