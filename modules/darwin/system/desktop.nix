{ config, lib, pkgs, ... }:

let
  inherit (builtins) isPath isAttrs match;

  cfg = config.system.desktop;

  setPicture = desktop: picture: ''
    echo "setting ${desktop} picture to ${picture}"
    osascript -e "tell application \"System Events\" to tell ${desktop} to set picture to \"${picture}\" as POSIX file"
  '';
in
{
  options.system.desktop = with lib.types; {
    picture = lib.mkOption {
      type = nullOr path;
      default = null;
    };
  };

  config = lib.mkIf (cfg.picture != null) {
    system.activationScripts.postUserActivation.text = setPicture "every desktop" cfg.picture;
  };
}
