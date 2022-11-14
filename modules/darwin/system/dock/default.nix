{ config, lib, pkgs, ... }:

with lib;
with types;

let
  inherit (builtins) isPath isAttrs match;

  cfg = config.system.dock;

  validDesktopIdRegex = "desktop [1-9][1-10]*";
  isValidDesktopId = str: (match validDesktopIdRegex str) != null;

  setPicture = desktop: picture: ''
    echo "setting ${desktop} picture to ${picture}"
    osascript -e "tell application \"System Events\" to tell ${desktop} to set picture to \"${picture}\" as POSIX file"
  '';
in
{
  options.system.dock = {
    picture = lib.mkOption {
      type = nullOr (either path (attrsOf path));
      default = {};
      description = "The path to the wallpaper on a every desktop or specific ones";
      example = literalExpression ''
        For every desktop set to ./wallpaper1
        For a particular desktop set to:
        {
          "desktop 1" = ./wallpaper1;
          "desktop 2" = ./wallpaper1;
        }

        The key must follow the pattern '${validDesktopIdRegex}'.
      '';
    };
  };

  config = {
    assertions = [ {
      assertion =
        (isPath cfg.picture) ||
        ((isAttrs cfg.picture) && (all isValidDesktopId (mapAttrsToList (id: path: id) cfg.picture)));
      message = ''All desktops must follow the pattern "${validDesktopIdRegex}"'';
    } ];

    system.activationScripts.postUserActivation.text = mkIf (cfg.picture != null) (
      if isPath cfg.picture then (setPicture "every desktop" cfg.picture)
      else if isAttrs cfg.picture then (concatStringsSep "\n" (mapAttrsToList setPicture cfg.picture))
      else throw "invalid picture type"
    );
  };
}
