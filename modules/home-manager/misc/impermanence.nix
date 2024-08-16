{ lib, config, inputs, pkgs, ... }:

let
  cfg = config.custom.impermanence;
  mkImpermanenceOption = default: lib.mkOption {
    inherit default;
    type = lib.types.bool;
  };
in
{
  options.custom.impermanence = {
    enable = lib.mkEnableOption "Whether to enable home-manager impermanence. Only usable in Nixos";
    dataLocation = lib.mkOption {
      type = with lib.types; str;
      description = "Location of the users's configuration persist directory";
    };

    cacheLocation = lib.mkOption {
      type = with lib.types; str;
      description = "Location of the users's configuration persist directory";
    };

    lutris = mkImpermanenceOption   config.custom.lutris.enable;
    heroic = mkImpermanenceOption   false;
    steam = mkImpermanenceOption    false;
    sunshine = mkImpermanenceOption config.custom.sunshine.enable;
    firefox = mkImpermanenceOption  config.programs.firefox.enable;
    fish = mkImpermanenceOption     config.programs.fish.enable;
    direnv = mkImpermanenceOption   config.programs.direnv.enable;
    zoxide = mkImpermanenceOption   config.programs.zoxide.enable;
    zellij = mkImpermanenceOption   config.programs.zellij.enable;
    vlc = mkImpermanenceOption      false;
    discord = mkImpermanenceOption  false;
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
         assertion = pkgs.stdenv.isLinux;
         message = "custom.impermanence is only available in NixOS.";
      }
    ];

    home.persistence."${cfg.dataLocation}" = {
      allowOther = true;
      directories = [
        ".dotfiles"

        "Downloads"
        "Music"
        "Pictures"
        "Videos"

        ".ssh"
        ".gnupg"
        ".config/systemd" # git maintenance systemd timers
        ".local/share/nix" # trusted settings and repl history
      ] ++ lib.optionals cfg.lutris [ "${config.xdg.configHome}/lutris" "${config.xdg.dataHome}/lutris" ]
        ++ lib.optionals cfg.heroic [ "${config.xdg.configHome}/heroic" ]
        ++ lib.optionals cfg.steam [ ".steam" ".local/share${config.xdg.dataHome}/Steam" ]
        ++ lib.optionals cfg.sunshine [ "${config.xdg.configHome}/sunshine/credentials" ]
        ++ lib.optionals cfg.firefox [ ".mozilla" ]
        ++ lib.optionals cfg.vlc [ "${config.xdg.configHome}/vlc" ]
        ++ lib.optionals cfg.discord [ "${config.xdg.configHome}/discord" ];

      files = [ ]
        ++ lib.optionals cfg.sunshine [
          "${config.xdg.configHome}/sunshine/sunshine.conf"
          "${config.xdg.configHome}/sunshine/sunshine_state.json"
        ];
    };

    home.persistence."${cfg.cacheLocation}" = {
      allowOther = true;

      directories = [
        "${config.xdg.cacheHome}/nix"
      ] ++ lib.optionals cfg.firefox  [ "${config.xdg.cacheHome}/mozilla" ]
        ++ lib.optionals cfg.fish     [ "${config.xdg.dataHome}/fish" ]
        ++ lib.optionals cfg.zoxide   [ "${config.xdg.dataHome}/zoxide" ]
        ++ lib.optionals cfg.direnv   [ "${config.xdg.dataHome}/direnv" ]
        ++ lib.optionals cfg.zellij   [ "${config.xdg.cacheHome}/zellij" ];

      files = [ ".bash_history" ];
    };
  };
}
