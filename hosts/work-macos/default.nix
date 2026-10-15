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
    ];

    brews = [
      # Coding

      # Infrastructure / DevOps
      "scalr"
      "helm"

      # Scala
      "bazelisk"

      # Typescript
      "nvm"
      "pnpm"
    ];
  };

  nix.settings.trusted-users = [ username ];
  system.primaryUser = username;

  users.users.${username}.home = "/Users/${username}";
  home-manager.users.${username} = ./brunohenriques.nix;

  system.stateVersion = 6;
}
