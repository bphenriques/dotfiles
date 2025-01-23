{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.system.screencapture;
  screenshotsLocation = config.system.defaults.screencapture.location;
in
{
  options.system.screencapture = {
    ensureLocation = mkEnableOption "darwin-screencapture-ensure-location-exists";
  };
  
  config = mkIf (cfg.ensureLocation && screenshotsLocation != null) {
    system.activationScripts.postUserActivation.text = ''
      if [ ! -d "${screenshotsLocation}" ]; then
        echo "Screencapture - Creating ${screenshotsLocation} .."
        mkdir -pv "${screenshotsLocation}"
      fi
    '';
  };
}
