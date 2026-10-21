_:
let
  username = "bruno.henriques";
in
{
  imports = [
    ../../profiles/darwin
  ];

  homebrew = {
    taps = [
      { name = "Scalr/scalr"; trusted = true; }
    ];

    casks = [
      "cursor"
      "visual-studio-code"
    ];

    brews = [
      # Coding

      # Infrastructure / DevOps
      "scalr"
      "helm"
      "colima"
      "docker"
      "docker-compose"

      # Scala (JDK via SDKMAN on work-macos; see hosts/work-macos/sdkman.nix)
      "bazelisk"
      "sbt"
      "scala-cli"
      "metals"

      # Typescript
      "nvm"     # Node version
      "yarn"    # Package manager:
      "pnpm@10" # Package Manager: Requires brew link --overwrite pnpm@10
    ];
  };

  nix.settings.trusted-users = [ username ];
  system.primaryUser = username;

  users.users.${username}.home = "/Users/${username}";
  home-manager.users.${username} = ./brunohenriques.nix;

  system.stateVersion = 6;
}

# https://github.com/EngFlow/auth/releases
# sudo curl -L -o /usr/local/bin/engflow_auth "https://github.com/EngFlow/auth/releases/download/v0.0.14/engflow_auth_macos_arm64"
# sudo chmod +x /usr/local/bin/engflow_auth