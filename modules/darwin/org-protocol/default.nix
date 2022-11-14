{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.programs.org-protocol;
in
{
  options.programs.org-protocol = {
    enable = mkEnableOption "darwin-org-protocol";
  };

  config = mkIf cfg.enable {
    system.activationScripts.postUserActivation.text = ''
      echo "Copying org-protocol.app to /Applications .."
      ln -sfn "${builtins.toString ./org-protocol.app}" /Applications/org-protocol.app
    '';
  };
}
