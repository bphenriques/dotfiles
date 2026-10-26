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

  programs.fish.functions.sdk = {
    description = "SDKMAN CLI (bash)";
    body = ''bash -c 'source "$HOME/.sdkman/bin/sdkman-init.sh" && sdk "$@"' bash $argv'';
  };

  # Sourced on first use to keep startup fast; unlike fish, `sdk use` then works in this shell.
  programs.zsh.initContent = ''
    sdk() {
      unfunction sdk
      source "$SDKMAN_DIR/bin/sdkman-init.sh"
      sdk "$@"
    }
  '';
}
