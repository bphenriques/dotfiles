{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyfin;
  oidcCfg = config.custom.homelab.oidc;

  adminUsernameFile = pkgs.writeText "jellyfin-admin-username" "admin";

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
    # Post-deploy: configure API credentials in Jellyfin UI → Dashboard → Plugins → Open Subtitles
    opensubtitles = {
      name = "Open Subtitles";
      version = "23.0.0.0";
      src = pkgs.fetchzip {
        url = "https://repo.jellyfin.org/releases/plugin/open-subtitles/open-subtitles_23.0.0.0.zip";
        hash = "sha256-+5gwpkZliE5Kb3JKqcUDAAZDZ0UXNue4NkUgdn0fYMA=";
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
    oidEndpoint = oidcCfg.provider.issuerUrl;
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
      # Remove old versions of this plugin (avoids duplicate loads after upgrades)
      find "/var/lib/jellyfin/plugins" -maxdepth 1 -name "${plugin.name}_*" -not -name "${plugin.name}_${plugin.version}" -exec rm -rf {} +

      pluginDir="/var/lib/jellyfin/plugins/${plugin.name}_${plugin.version}"
      marker="$pluginDir/.installed-src"

      # Reinstall if missing or if the Nix store path changed (hash bump, repack, etc.)
      if [ ! -f "$marker" ] || [ "$(cat "$marker")" != "${plugin.src}" ]; then
        rm -rf "$pluginDir"
        mkdir -p "$pluginDir"
        cp -r ${plugin.src}/. "$pluginDir/"
        echo "${plugin.src}" > "$marker"
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
    description = "Jellyfin SSO setup";
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
      JELLYFIN_URL = serviceCfg.url;
      JELLYFIN_ADMIN_USERNAME_FILE = adminUsernameFile;
      JELLYFIN_ADMIN_PASSWORD_FILE = serviceCfg.secrets.files.admin-password.path;
      SSO_CONFIG_FILE = ssoConfigFile;
      OIDC_CLIENT_ID_FILE = serviceCfg.oidc.id.file;
      OIDC_CLIENT_SECRET_FILE = serviceCfg.oidc.secret.file;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "jellyfin-sso-configure" ./sso-configure.nu}'';
  };
}
