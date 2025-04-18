{ lib, pkgs, ... }:
let
  inherit (pkgs.nur.repos.rycee) firefox-addons;
in
lib.mkIf pkgs.stdenv.isLinux {
  programs.firefox = {
    enable = true;
    profiles = {
      default = {
        name = "Bruno";
        settings = lib.attrsets.mergeAttrsList [ (import ./basic.nix) (import ./telemetry.nix) ];

        # Require manual activation once installed: compute list with nix flake show "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons"
        extensions.packages = [
          firefox-addons.ublock-origin
          firefox-addons.bitwarden
          firefox-addons.keepa
          firefox-addons.linkding-extension
          firefox-addons.floccus
          firefox-addons.no-pdf-download
        ];

        search = {
          force = true;
          default = "google";
          order = [ "google" ];
          engines = {
            "Nix Packages" = {
              definedAliases = [ "@n" "@nix" ];
              urls = [{
                template = "https://search.nixos.org/packages";
                params = [
                  { name = "type"; value = "packages"; }
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
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
              definedAliases = [ "@nw" "@nixwiki" ];
              urls = [{ template = "https://nixos.wiki/index.php?search={searchTerms}"; }];
              icon = "https://nixos.wiki/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000; # every day
            };
            "youtube" = {
              definedAliases = ["@yt"];
              icon = "https://youtube.com/favicon.ico";
              updateInterval = 24 * 60 * 60 * 1000;
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
            "google".metaData.alias = "@g"; # builtin engines only support specifying one additional alias
          };
        };
      };
    };
  };

  custom.xdgDefaultApps.internetBrowser = lib.mkBefore [ "firefox.desktop" ];
  home.sessionVariables.BROWSER = "firefox";
}
