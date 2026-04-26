{ lib, pkgs, config, self, osConfig, ... }:
let
  inherit (builtins) listToAttrs;
  inherit (lib) nameValuePair mapAttrsToList;

  cfg = config.custom.programs.session;

  mkAppOpt = lib.mkOption {
    type = lib.types.str;
  };
  mkAppOpt' = default: mkAppOpt // { inherit default; };

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  systemctl = lib.getExe' pkgs.systemd "systemctl";
  rebootToWindowsCmd = value: ''${systemctl} reboot --boot-loader-entry="windows_${value.title}.conf"'';

  systemd-boot-windows = lib.optionals osConfig.boot.loader.systemd-boot.enable
    (mapAttrsToList (name: value: {
      id = "session-${name}";
      symbol = "";
      label = "Reboot to ${value.title}";
      exec = rebootToWindowsCmd value; # systemctl reboot --boot-loader-entry=help
    }) osConfig.boot.loader.systemd-boot.windows);
  sessionActions = [
    { id = "session-lock";      symbol = ""; label = "Lock";                 exec = cfg.exec.lock; }
    { id = "session-suspend";   symbol = "󰤄"; label = "Suspend";              exec = cfg.exec.suspend; }
    { id = "session-shutdown";  symbol = ""; label = "Shutdown";             exec = cfg.exec.shutdown; }
    { id = "session-reboot";    symbol = ""; label = "Reboot";               exec = cfg.exec.reboot; }
  ] ++ systemd-boot-windows ++ [
    { id = "session-efi";       symbol = ""; label = "Reboot to EFI setup";  exec = cfg.exec.reboot-efi; }
  ];

  dmenu = self.lib.builders.writeFuzzelDmenuApplication {
    name = "session-dmenu";
    entries = lib.map (e: { inherit (e) exec; label = "${e.symbol}     ${e.label}"; }) sessionActions;
    extraArgs = ''--minimal-lines --hide-prompt'';
  };
in
{
  options.custom.programs.session = {
    enable = lib.mkEnableOption "custom-session";
    exec = {
      dmenu             = mkAppOpt' ''${lib.getExe dmenu}'';
      lock              = mkAppOpt;
      suspend           = mkAppOpt' "${systemctl} suspend";
      shutdown          = mkAppOpt' "${systemctl} poweroff";
      reboot            = mkAppOpt' "${systemctl} reboot";
      reboot-efi        = mkAppOpt' "${systemctl} reboot --firmware-setup";
      reboot-windows    = mkAppOpt' (rebootToWindowsCmd (builtins.head (lib.attrValues osConfig.boot.loader.systemd-boot.windows)));
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
        exec = cfg.exec.dmenu;
        actions = let
          toAction = b: nameValuePair b.id {
            name = b.label;
            icon = mkIcon b.id b.symbol;
            inherit (b) exec;
          };
        in listToAttrs (lib.map toAction sessionActions);
      })
    ];
  };
}
