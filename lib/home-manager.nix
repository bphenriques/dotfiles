{ home-manager, homeManagerModules, nixpkgs, nixpkgsConfig, ... }:
{
  mkHomeManagerHost = { system, username, hostModule }:
    let
      common = {
        home = {
          inherit username;
          homeDirectory = "/home/${username}";
        };
      };
    in
      home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          inherit system;
          inherit (nixpkgsConfig) config;
        };
        modules = [common hostModule] ++ homeManagerModules;
      };
}
