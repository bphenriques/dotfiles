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
