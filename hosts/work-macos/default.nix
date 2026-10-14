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

    brews = [
      "scalr"
      "helm"
      "bazelisk"
    ];
  };

  nix.settings.trusted-users = [ username ];
  system.primaryUser = username;

  users.users.${username}.home = "/Users/${username}";
  home-manager.users.${username} = ./brunohenriques.nix;

  system.stateVersion = 6;
}
