{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyfin;
  oidcClient = config.custom.homelab.oidc.clients.jellyfin;
  oidcCfg = config.custom.homelab.oidc;

  themeCSS = ''@import url("https://cdn.jsdelivr.net/gh/lscambo13/ElegantFin@main/Theme/ElegantFin-jellyfin-theme-build-latest-minified.css");'';
  plugins = {
    sso = {
      name = "SSO-Auth";
      version = "4.0.0.3";
      src = pkgs.fetchzip {
        url = "https://github.com/9p4/jellyfin-plugin-sso/releases/download/v4.0.0.3/sso-authentication_4.0.0.3.zip";
        hash = "sha256-Jkuc+Ua7934iSutf/zTY1phTxaltUkfiujOkCi7BW8w=";
        stripRoot = false;
      };
    };
  };

  # SSO plugin configuration
  brandingConfig = {
    LoginDisclaimer = ''
      <a href="${serviceCfg.publicUrl}/sso/OID/start/${oidcCfg.provider.internalName}" class="raised emby-button">
        Sign in with ${oidcCfg.provider.displayName}
      </a>
    '';
    SplashscreenEnabled = false;
    CustomCss = ''
      ${themeCSS}
      a.raised.emby-button { padding: 0.9em 1em; color: inherit !important; }
      .disclaimerContainer { display: block; }
    '';
  };

  ssoConfig = {
    enabled = true;
    providerName = oidcCfg.provider.internalName;
    oidEndpoint = oidcCfg.provider.url;
    oidClientId = "__OIDC_CLIENT_ID__";
    oidSecret = "__OIDC_CLIENT_SECRET__";
    enableAuthorization = true;
    enableAllFolders = true;
    enabledFolders = [ ];
    roles = [ config.custom.homelab.groups.users config.custom.homelab.groups.admin ];
    adminRoles = [ config.custom.homelab.groups.admin ];
    enableFolderRoles = false;
    folderRoleMapping = [ ];
    roleClaim = "groups";
    oidScopes = [ "openid" "profile" "groups" ];
    defaultUsernameClaim = "preferred_username";
    schemeOverride = "https";
  };

  ssoConfigFile = pkgs.writeText "sso-config.json" (builtins.toJSON { inherit brandingConfig ssoConfig; });
in
{
  systemd.services.jellyfin.serviceConfig.ExecStartPre = let
    installPlugin = plugin: ''
      pluginDir="/var/lib/jellyfin/plugins/${plugin.name}_${plugin.version}"
      if [ ! -f "$pluginDir/.installed" ]; then
        rm -rf "$pluginDir"
        mkdir -p "$pluginDir"
        cp -r ${plugin.src}/* "$pluginDir/"
        touch "$pluginDir/.installed"
      fi
    '';
    installScript = pkgs.writeShellScript "jellyfin-install-plugins" ''
      ${lib.concatMapStringsSep "\n" installPlugin (lib.attrValues plugins)}
      chmod -R u+rw "/var/lib/jellyfin/plugins"
      chown -R jellyfin:jellyfin "/var/lib/jellyfin/plugins"
    '';
  in lib.mkAfter [ "+${installScript}" ];

  # SSO plugin configuration service - runs after jellyfin-configure
  systemd.services.jellyfin-sso-configure = {
    description = "Configure Jellyfin SSO plugin";
    wantedBy = [ "multi-user.target" ];
    after = [ "jellyfin-configure.service" ];
    requires = [ "jellyfin-configure.service" ];
    partOf = [ "jellyfin.service" ];
    restartTriggers = [ ssoConfigFile ./sso-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      JELLYFIN_URL = serviceCfg.internalUrl;
      JELLYFIN_ADMIN_USERNAME_FILE = config.sops.secrets."jellyfin/admin/username".path;
      JELLYFIN_ADMIN_PASSWORD_FILE = config.sops.secrets."jellyfin/admin/password".path;
      SSO_CONFIG_FILE = ssoConfigFile;
      OIDC_CLIENT_ID_FILE = oidcClient.idFile;
      OIDC_CLIENT_SECRET_FILE = oidcClient.secretFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "jellyfin-sso-configure" ./sso-configure.nu}'';
  };
}
