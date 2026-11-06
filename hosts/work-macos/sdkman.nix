{ config, ... }:
let
  sdkmanDir = "${config.home.homeDirectory}/.sdkman";
  javaHome = "${sdkmanDir}/candidates/java/current";
in
{
  # FIXME: automate the sdkman install. SDKMan is for work; personal projects use nix shells.
  home = {
    sessionPath = [ "${javaHome}/bin" ];
    sessionVariables = {
      SDKMAN_DIR = sdkmanDir;
      JAVA_HOME = javaHome;
    };
  };

  # Sourced on first use to keep startup fast.
  programs.zsh.initContent = ''
    sdk() {
      unfunction sdk
      source "$SDKMAN_DIR/bin/sdkman-init.sh"
      sdk "$@"
    }
  '';
}
