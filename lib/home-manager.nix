{ home-manager, nixpkgs, ... }:

{
  mkHMHost = { system ? "x86_64-linux", username, homeConfig }:
  let
    baseModule = {
      home = {
        inherit username;
        homeDirectory = "/home/${username}";
      };
    };
  in
    home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        inherit system;
      };
      modules = [ baseModule homeConfig ];
    };
}
