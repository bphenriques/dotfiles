{ lib, config, inputs, pkgs, ... }:
# TODO: Consider using a pattern that applies for all users for some bits and bobs. See https://github.com/prescientmoon/everything-nix/blob/develop/hosts/nixos/common/global/persistence.nix#L20
# I could define each state/
# https://github.com/prescientmoon/everything-nix/blob/develop/home/features/persistence.nix
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

    # Gaming
    lutris = mkImpermanenceOption       config.custom.lutris.enable;
    heroic = mkImpermanenceOption       false;
    steam = mkImpermanenceOption        false;
    gog = mkImpermanenceOption          false;
    protontricks = mkImpermanenceOption false;
    sunshine = mkImpermanenceOption     config.custom.sunshine.enable;

    # Desktop
    firefox = mkImpermanenceOption      config.programs.firefox.enable;
    discord = mkImpermanenceOption      false;
    qbittorrent = mkImpermanenceOption  false;
    jetbrains = mkImpermanenceOption    false;  # FIXME: we only need to persist the version that is installed, not the previous one.
    g4music = mkImpermanenceOption      false;
    vlc = mkImpermanenceOption          false;
    signal = mkImpermanenceOption       false;
    filezilla = mkImpermanenceOption    false;
    museeks = mkImpermanenceOption      false;

    # Drivers
    nvidia = mkImpermanenceOption       false;
    mesa = mkImpermanenceOption         false;
    wine = mkImpermanenceOption         false;
    winetricks = mkImpermanenceOption   false;
    solaar = mkImpermanenceOption       false;

    # Security
    gpg = mkImpermanenceOption          config.programs.gpg.enable;
    sops = mkImpermanenceOption         false;

    # CLI Apps
    helix = mkImpermanenceOption        config.programs.helix.enable;
    fish = mkImpermanenceOption         config.programs.fish.enable;
    direnv = mkImpermanenceOption       config.programs.direnv.enable;
    zoxide = mkImpermanenceOption       config.programs.zoxide.enable;
    zellij = mkImpermanenceOption       config.programs.zellij.enable;
    bitwarden = mkImpermanenceOption    false;
    beets = mkImpermanenceOption        config.programs.beets.enable;

    # Dev - Scala
    scalacli = mkImpermanenceOption    false;
    metals = mkImpermanenceOption      false;
    coursier = mkImpermanenceOption    false;

    # TODO: local/share/mime dir?
    # TODO: local/share/mimeapps.list file?
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
        "${config.xdg.dataHome}/nix"        # trusted settings and repl history
        "${config.xdg.configHome}/systemd"  # systemd timers
        ".ssh"
      ] ++ lib.optionals cfg.gpg    [ ".gnupg" ]
        ++ lib.optionals cfg.lutris [ "${config.xdg.configHome}/lutris" "${config.xdg.dataHome}/lutris" ]
        ++ lib.optionals cfg.heroic [ "${config.xdg.configHome}/heroic" ]
        ++ lib.optionals cfg.steam [
          ".steam"
          { directory = "${config.xdg.dataHome}/Steam"; method = "symlink"; } # Some games don't play well with bindfs
        ]
        ++ lib.optionals cfg.filezilla    [ "${config.xdg.configHome}/filezilla" ]
        ++ lib.optionals cfg.sunshine [ "${config.xdg.configHome}/sunshine/credentials" ]
        ++ lib.optionals cfg.firefox [ ".mozilla" ]
        ++ lib.optionals cfg.vlc [ "${config.xdg.configHome}/vlc" ]
        ++ lib.optionals cfg.discord [ "${config.xdg.configHome}/discord" ]
        ++ lib.optionals cfg.qbittorrent [ "${config.xdg.configHome}/qBittorrent" ]
        ++ lib.optionals cfg.signal [ "${config.xdg.configHome}/Signal" ]
        ++ lib.optionals cfg.bitwarden [ "${config.xdg.configHome}/Bitwarden CLI" ]
        ++ lib.optionals cfg.solaar [ "${config.xdg.configHome}/solaar" ]
        ++ lib.optionals cfg.gog  [ "${config.xdg.dataHome}/GOG.com" ] # cache?
        ++ lib.optionals cfg.scalacli [ "${config.xdg.dataHome}/scalacli" ]
        ++ lib.optionals cfg.sops [ "${config.xdg.configHome}/sops" ]
        ++ lib.optionals cfg.museeks [ "${config.xdg.configHome}/Museeks" ]; # A Electron mess of config+cache.

      files = [ ]
        ++ lib.optionals cfg.sunshine [
          "${config.xdg.configHome}/sunshine/sunshine.conf"
          "${config.xdg.configHome}/sunshine/sunshine_state.json"
        ]
        ++ lib.optionals cfg.beets  [ "${config.xdg.dataHome}/beets/beets.db" ]; # my personal setup
    };

    home.persistence."${cfg.cacheLocation}" = {
      allowOther = true;

      directories = [
        "${config.xdg.cacheHome}/nix"
      ] ++ lib.optionals cfg.firefox      [ "${config.xdg.cacheHome}/mozilla" ]
        ++ lib.optionals cfg.fish         [ "${config.xdg.dataHome}/fish" ]
        ++ lib.optionals cfg.zoxide       [ "${config.xdg.dataHome}/zoxide" ]
        ++ lib.optionals cfg.direnv       [ "${config.xdg.dataHome}/direnv" ]
        ++ lib.optionals cfg.zellij       [ "${config.xdg.cacheHome}/zellij" ]
        ++ lib.optionals cfg.helix        [ "${config.xdg.cacheHome}/helix" ]
        ++ lib.optionals cfg.wine         [ "${config.xdg.cacheHome}/wine" ]
        ++ lib.optionals cfg.winetricks   [ "${config.xdg.cacheHome}/winetricks" ]
        ++ lib.optionals cfg.qbittorrent  [ "${config.xdg.dataHome}/qBittorrent" ] # TODO: I can likely drill down this better
        ++ lib.optionals cfg.lutris       [
          "${config.xdg.cacheHome}/lutris/banners"  # Game banners
          "${config.xdg.cacheHome}/lutris/coverart" # Game cover art
        ]
        ++ lib.optionals cfg.protontricks [ "${config.xdg.cacheHome}/protontricks" ]
        ++ lib.optionals cfg.nvidia       [ "${config.xdg.cacheHome}/nvidia" ]
        ++ lib.optionals cfg.jetbrains    [ "${config.xdg.cacheHome}/JetBrains" ]
        ++ lib.optionals cfg.g4music      [ "${config.xdg.cacheHome}/com.github.neithern.g4music" ]
        ++ lib.optionals cfg.scalacli     [ "${config.xdg.cacheHome}/scalacli" ]
        ++ lib.optionals cfg.metals       [ "${config.xdg.cacheHome}/metals" ]
        ++ lib.optionals cfg.filezilla    [ "${config.xdg.cacheHome}/filezilla" ]
        ++ lib.optionals cfg.scalacli     [ "${config.xdg.cacheHome}/bloop" ] # TODO: or metals?
        ++ lib.optionals cfg.coursier     [ "${config.xdg.cacheHome}/coursier" ]
        ++ lib.optionals cfg.mesa         [ "${config.xdg.cacheHome}/mesa_shader_cache" ];

      files = [ ".bash_history" ];
    };
  };
}
