{ lib, pkgs, config, self, community, ... }:
{
  programs.firefox = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    profiles = {
      default = {
        name = "Bruno";
        settings = lib.attrsets.mergeAttrsList [ (import ./basic.nix) (import ./telemetry.nix) ];

        # Require manual activation once installed: compute list with  nix flake show "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons"
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          ublock-origin
          bitwarden
          keepa
          linkding-extension
          # tridactyl # TODO: Consider theme: https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/modules/home/firefox/tridactyl/tridactylrc
          no-pdf-download
        ];

        search = {
          force = true;
          default = "Google";
          order = [ "Google" ];
          engines = {
            "Nix Packages" = {
              urls = [{
                template = "https://search.nixos.org/packages";
                params = [
                  { name = "type"; value = "packages"; }
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@np" "@nix" ];
            };
            "NixOS Options" = {
              definedAliases = ["@nixopts" "@no"];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              urls = [
                {
                  template = "https://search.nixos.org/options";
                  params = [
                    { name = "channel"; value = "unstable"; }
                    { name = "query"; value = "{searchTerms}"; }
                  ];
                }
              ];
            };
            "Home Manager Options" = {
              definedAliases = ["@homemanager" "@hm"];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              urls = [{
                  template = "https://home-manager-options.extranix.com";
                  params = [ { name = "query"; value = "{searchTerms}"; } ];
              }];
            };
            "NixOS Wiki" = {
              urls = [{ template = "https://nixos.wiki/index.php?search={searchTerms}"; }];
              iconUpdateURL = "https://nixos.wiki/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000; # every day
              definedAliases = [ "@nw" "@nixwiki" ];
            };
            "YouTube" = {
              iconUpdateURL = "https://youtube.com/favicon.ico";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = ["@yt"];
              urls = [{
                template = "https://www.youtube.com/results";
                params = [
                  { name = "search_query"; value = "{searchTerms}"; }
                ];
              }];
            };
            "Wikipedia" = {
              definedAliases = [ "@wk" "@wikipedia" ];
              urls = [{
                template = "https://en.wikipedia.org/wiki/Special:Search";
                params = [
                  { name = "search"; value = "{searchTerms}"; }
                ];
              }];
            };
            "Google".metaData.alias = "@g"; # builtin engines only support specifying one additional alias
          };
        };
      };
    };
  };

  xdg.mimeApps.defaultApplications = {
    "application/x-extension-htm" = "firefox.desktop";
    "application/x-extension-html" = "firefox.desktop";
    "application/x-extension-shtml" = "firefox.desktop";
    "application/x-extension-xht" = "firefox.desktop";
    "application/x-extension-xhtml" = "firefox.desktop";
    "application/xhtml+xml" = "firefox.desktop";
    "text/html" = "firefox.desktop";
    "text/xml" = ["firefox.desktop"];
    "x-scheme-handler/chrome" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
  };
}