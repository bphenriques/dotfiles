{ config, ... }:
let
  sdkmanDir = "${config.home.homeDirectory}/.sdkman";
  javaHome = "${sdkmanDir}/candidates/java/current";
in
{
  # FIXME: Automate installation of sdkman: curl -s "https://get.sdkman.io" | bash
  # SDKMan is practical in a work setting. Personal projects can use plain nix shells that skip sdkman.
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
