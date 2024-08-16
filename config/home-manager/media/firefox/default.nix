{ lib, pkgs, config, ... }:

# https://github.com/edmundmiller/dotfiles/blob/main/modules/desktop/browsers/firefox.nix
# https://github.com/bbigras/nix-config/blob/master/users/bbigras/graphical/firefox.nix
# https://github.com/oddlama/nix-config/blob/main/users/myuser/graphical/firefox.nix
let
  merge = lib.foldr (a: b: a // b) { };
in
{
  programs.firefox = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    #package = pkgs.floorp.override {
    #  nativeMessagingHosts = [
    #    pkgs.tridactyl-native
    #    pkgs.gnome-browser-connector
    #  ];
    #};
    profiles = {
      default = {
        id = 0;
        name = "Bruno Henriques";
        settings = merge [ (import ./basic.nix) (import ./telemetry.nix) ];

        # Require manual activation once installed
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          ublock-origin
          bitwarden
          keepa
          multi-account-containers
          linkding-extension
          # tridactyl # TODO: Consider theme: https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/modules/home/firefox/tridactyl/tridactylrc
          consent-o-matic
          no-pdf-download

          onetab
          # libredirect# One day, check auto-redirects

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
            "Google".metaData.alias = "@g"; # builtin engines only support specifying one additional alias
          };
        };

        # Default containers for all profiles
        containersForce = true;
        containers = {
          "Personal" = {
            id = 1;
            color = "turquoise";
            icon = "chill";
          };
          "Sensitive" = {
            id = 2;
            color = "red";
            icon = "fingerprint";
          };
          "Shopping" = {
            id = 3;
            color = "orange";
            icon = "cart";
          };
          "Social" = {
            id = 4;
            color = "yellow";
            icon = "tree";
          };
        };
      };
    };
  };

  xdg.mimeApps.defaultApplications = {
    "text/html" = ["firefox.desktop"];
    "text/xml" = ["firefox.desktop"];
    "x-scheme-handler/http" = ["firefox.desktop"];
    "x-scheme-handler/https" = ["firefox.desktop"];
  };
}
