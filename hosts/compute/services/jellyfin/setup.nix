{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.routes.jellyfin;
  pathsCfg = config.custom.paths;
  oidcClient = config.custom.home-server.oidc.clients.jellyfin;
  oidcCfg = config.custom.home-server.oidc;

  initScript = self.lib.builders.writeNushellScript "jellyfin-init" ./jellyfin-init.nu;

  plugins = [
    (rec {
      name = "SSO-Auth";
      version = "4.0.0.3";
      src = pkgs.fetchzip {
        url = "https://github.com/9p4/jellyfin-plugin-sso/releases/download/v${version}/sso-authentication_${version}.zip";
        hash = "sha256-Jkuc+Ua7934iSutf/zTY1phTxaltUkfiujOkCi7BW8w=";
        stripRoot = false;
      };
    })
  ];

  jellyfinConfig = {
    serverName = "Jellyfin";
    libraries = [
      { Name = "Movies";    CollectionType = "movies";  Locations = [ pathsCfg.media.movies ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = true; }
      { Name = "TV Shows";  CollectionType = "tvshows"; Locations = [ pathsCfg.media.tv ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = true; }
      { Name = "Music";     CollectionType = "music";   Locations = [ pathsCfg.media.music.library ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = false; }
    ];
    trickplayConfig = {
      EnableHwAcceleration = true;
      EnableHwEncoding = true;
      EnableHwDecoding = true;
    };
    brandingConfig = {
      LoginDisclaimer = ''
        <a href="${serviceCfg.publicUrl}/sso/OID/start/${oidcCfg.provider.internalName}" class="raised emby-button">
          Sign in with ${oidcCfg.provider.displayName}
        </a>
      '';
      SplashscreenEnabled = false;
      CustomCss = ''
        @import url("https://cdn.jsdelivr.net/gh/lscambo13/ElegantFin@main/Theme/ElegantFin-jellyfin-theme-build-latest-minified.css");
        a.raised.emby-button { padding: 0.9em 1em; color: inherit !important; }
        .disclaimerContainer { display: block; }
      '';
    };
    ssoProviderName = oidcCfg.provider.internalName;
    ssoConfig = {
      oidEndpoint = oidcCfg.provider.url;
      oidClientId = "__OIDC_CLIENT_ID__";
      oidSecret = "__OIDC_CLIENT_SECRET__";
      enabled = true;
      enableAuthorization = true;
      enableAllFolders = true;
      enabledFolders = [ ];
      roles = [ "users" "admins" ];
      adminRoles = [ "admins" ];
      enableFolderRoles = false;
      folderRoleMapping = [ ];
      roleClaim = "groups";
      oidScopes = [ "openid" "profile" "groups" ];
      defaultUsernameClaim = "preferred_username";
      schemeOverride = "https";
    };
    userConfigs = lib.mapAttrsToList (_: u: {
      username = u.username;
      policy = {
        IsHidden = false;
        EnableSubtitleManagement = true;
      };
    }) (lib.filterAttrs (_: u: u.services.jellyfin.enable) config.custom.home-server.users);
  };
in
{
  config = lib.mkIf config.services.jellyfin.enable {
    sops = {
      secrets."jellyfin/admin/username" = { };
      secrets."jellyfin/admin/password" = { };
    };

    systemd.services.jellyfin.serviceConfig.ExecStartPre = let
      installPlugin = plugin: ''
        pluginDir="/var/lib/jellyfin/plugins/${plugin.name}_${plugin.version}"
        if [ ! -f "$pluginDir/.installed" ]; then
          rm -rf "$pluginDir"
          mkdir -p "$pluginDir"
          cp -r ${plugin.src}/* "$pluginDir/"
          touch "$pluginDir/.installed"
        fi
        chmod -R u+rw "/var/lib/jellyfin/plugins"
        chown -R jellyfin:jellyfin "/var/lib/jellyfin/plugins"
      '';
      script = pkgs.writeShellScript "jellyfin-install-plugins" ''
        ${lib.concatMapStringsSep "\n" installPlugin plugins}
      '';
    in [ "+${script}" ]; # The plus signals to run as root as we need to set permissions

    systemd.services.jellyfin-init = {
      description = "Initialize Jellyfin with declarative configuration";
      wantedBy = [ "jellyfin.target" ];
      after = [ "jellyfin.service" oidcCfg.systemd.provisionUnit ];
      requires = [ "jellyfin.service" ];
      wants = [ oidcCfg.systemd.readyUnit ];
      serviceConfig = {
        Type = "oneshot";
        Restart = "on-failure";
        RestartSec = 10;
        StartLimitBurst = 3;
      };
      environment = {
        JELLYFIN_URL = serviceCfg.internalUrl;
        JELLYFIN_ADMIN_USERNAME_FILE = config.sops.secrets."jellyfin/admin/username".path;
        JELLYFIN_ADMIN_PASSWORD_FILE = config.sops.secrets."jellyfin/admin/password".path;
        JELLYFIN_CONFIG_FILE = pkgs.writeText "jellyfin-config.json" (builtins.toJSON jellyfinConfig);
        OIDC_CLIENT_ID_FILE = oidcClient.idFile;
        OIDC_CLIENT_SECRET_FILE = oidcClient.secretFile;
        OIDC_USERS_FILE = oidcCfg.credentials.usersFile;
      };
      path = [ pkgs.nushell ];
      script = ''nu ${initScript}'';
    };
  };
}
