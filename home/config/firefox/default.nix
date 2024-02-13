{ lib, pkgs, ... }:

let merge = lib.foldr (a: b: a // b) { };
in
{
  programs.firefox = {
    enable = true;
    package = if pkgs.stdenv.hostPlatform.isDarwin then null else pkgs.firefox;

    # FIXME: Need to handle this when I get the chance. Need a solution for bookmarking.
    profiles = lib.mkIf pkgs.stdenv.isDarwin {
      default = {
        id = 0;
        name = "Bruno Henriques";
        settings = merge [ (import ./basic.nix) (import ./telemetry.nix) ];

        # Require manual activation once installed
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          ublock-origin
          bitwarden
          multi-account-containers
          vim-vixen
          keepa
          linkding-extension
          # One day, check auto-redirects
        ];

        # Need to look for a better private alternative...
        # - https://searx.space/ but requires self-hosting...
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
            "Google".metaData.alias = "@g"; # builtin engines only support specifying one additional alias
          };
        };

        # Default containers for all profiles
        containers = {
          "Personal" = {
            id = 1;
            color = "turquoise";
            icon = "chill";
          };
          "Banking" = {
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
}
