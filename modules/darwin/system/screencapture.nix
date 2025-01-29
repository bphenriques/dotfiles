{ config, lib, pkgs, ... }:

let
  cfg = config.system.screencapture;
  screenshotsLocation = config.system.defaults.screencapture.location;
in
{
  options.system.screencapture = {
    ensureLocation = lib.mkEnableOption "darwin-screencapture-ensure-location-exists";
  };
  
  config = lib.mkIf (cfg.ensureLocation && screenshotsLocation != null) {
    system.activationScripts.postUserActivation.text = ''
      if [ ! -d "${screenshotsLocation}" ]; then
        echo "Screencapture - Creating ${screenshotsLocation} .."
        mkdir -pv "${screenshotsLocation}"
      fi
    '';
  };
}
