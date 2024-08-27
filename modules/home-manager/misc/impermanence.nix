{ lib, config, inputs, pkgs, ... }:
# TODO: Consider using a pattern that applies for all users for some bits and bobs. See https://github.com/prescientmoon/everything-nix/blob/develop/hosts/nixos/common/global/persistence.nix#L20
# https://github.com/prescientmoon/everything-nix/blob/develop/home/features/persistence.nix
# TODO: Consider reading the configDir if set by home-manager
# TODO: ensure we are not trying to persist .config/systemd
let
  inherit (lib.strings) removePrefix;
  cfg = config.custom.impermanence;
  mkImpermanenceOption = default: lib.mkOption {
    inherit default;
    type = lib.types.bool;
  };

  relToHome = path: removePrefix "${config.home.homeDirectory}/" path;

  # Must be relative to the user directory
  xdgConfigHomeRel = relToHome config.xdg.configHome;
  xdgDataHomeRel = relToHome config.xdg.dataHome;
  xdgCacheHomeRel = relToHome config.xdg.cacheHome;
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
        "${xdgDataHomeRel}/nix"        # trusted settings and repl history
        ".ssh"
      ] ++ lib.optionals cfg.gpg    [ ".gnupg" ]
        ++ lib.optionals cfg.lutris [ "${xdgConfigHomeRel}/lutris" "${xdgDataHomeRel}/lutris" ]
        ++ lib.optionals cfg.heroic [ "${xdgConfigHomeRel}/heroic" ]
        ++ lib.optionals cfg.steam [
          ".steam"
          { directory = "${xdgDataHomeRel}/Steam"; method = "symlink"; } # Some games don't play well with bindfs
        ]
        ++ lib.optionals cfg.filezilla    [ "${xdgConfigHomeRel}/filezilla" ]
        ++ lib.optionals cfg.sunshine [ "${xdgConfigHomeRel}/sunshine/credentials" ]
        ++ lib.optionals cfg.firefox [ ".mozilla" ]
        ++ lib.optionals cfg.vlc [ "${xdgConfigHomeRel}/vlc" ]
        ++ lib.optionals cfg.discord [ "${xdgConfigHomeRel}/discord" ]
        ++ lib.optionals cfg.qbittorrent [ "${xdgConfigHomeRel}/qBittorrent" ]
        ++ lib.optionals cfg.signal [ "${xdgConfigHomeRel}/Signal" ]
        ++ lib.optionals cfg.bitwarden [ "${xdgConfigHomeRel}/Bitwarden CLI" ]
        ++ lib.optionals cfg.solaar [ "${xdgConfigHomeRel}/solaar" ]
        ++ lib.optionals cfg.gog  [ "${xdgDataHomeRel}/GOG.com" ] # cache?
        ++ lib.optionals cfg.scalacli [ "${xdgDataHomeRel}/scalacli" ]
        ++ lib.optionals cfg.sops [ "${xdgConfigHomeRel}/sops" ]
        ++ lib.optionals cfg.museeks [ "${xdgConfigHomeRel}/Museeks" ]; # A Electron mess of config+cache.

      files = lib.optionals cfg.sunshine [
        "${xdgConfigHomeRel}/sunshine/sunshine.conf"
        "${xdgConfigHomeRel}/sunshine/sunshine_state.json"
      ]
        ++ lib.optionals cfg.beets  [ "${xdgDataHomeRel}/beets/beets.db" ]; # my personal setup
    };

    home.persistence."${cfg.cacheLocation}" = {
      allowOther = true;

      directories = [
        "${xdgCacheHomeRel}/nix"
      ] ++ lib.optionals cfg.firefox      [ "${xdgCacheHomeRel}/mozilla" ]
        ++ lib.optionals cfg.fish         [ "${xdgDataHomeRel}/fish" ]
        ++ lib.optionals cfg.zoxide       [ "${xdgDataHomeRel}/zoxide" ]
        ++ lib.optionals cfg.direnv       [ "${xdgDataHomeRel}/direnv" ]
        ++ lib.optionals cfg.zellij       [ "${xdgCacheHomeRel}/zellij" ]
        ++ lib.optionals cfg.helix        [ "${xdgCacheHomeRel}/helix" ]
        ++ lib.optionals cfg.wine         [ "${xdgCacheHomeRel}/wine" ]
        ++ lib.optionals cfg.winetricks   [ "${xdgCacheHomeRel}/winetricks" ]
        ++ lib.optionals cfg.qbittorrent  [ "${xdgDataHomeRel}/qBittorrent" ] # TODO: I can likely drill down this better
        ++ lib.optionals cfg.lutris       [
          "${xdgCacheHomeRel}/lutris/banners"  # Game banners
          "${xdgCacheHomeRel}/lutris/coverart" # Game cover art
        ]
        ++ lib.optionals cfg.protontricks [ "${xdgCacheHomeRel}/protontricks" ]
        ++ lib.optionals cfg.nvidia       [ "${xdgCacheHomeRel}/nvidia" ]
        ++ lib.optionals cfg.jetbrains    [ "${xdgCacheHomeRel}/JetBrains" ]
        ++ lib.optionals cfg.g4music      [ "${xdgCacheHomeRel}/com.github.neithern.g4music" ]
        ++ lib.optionals cfg.scalacli     [ "${xdgCacheHomeRel}/scalacli" ]
        ++ lib.optionals cfg.metals       [ "${xdgCacheHomeRel}/metals" ]
        ++ lib.optionals cfg.filezilla    [ "${xdgCacheHomeRel}/filezilla" ]
        ++ lib.optionals cfg.scalacli     [ "${xdgCacheHomeRel}/bloop" ] # TODO: or metals?
        ++ lib.optionals cfg.coursier     [ "${xdgCacheHomeRel}/coursier" ]
        ++ lib.optionals cfg.mesa         [ "${xdgCacheHomeRel}/mesa_shader_cache" ];

      files = [ ".bash_history" ];
    };
  };
}
