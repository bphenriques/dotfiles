{ home-manager, homeManagerModules, nixpkgs, nixpkgsConfig, ... }:
{
  mkHomeManagerHost = { system, username, hostModule, extraSpecialArgs ? [] }:
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
        inherit extraSpecialArgs;
        modules = [common hostModule] ++ homeManagerModules;
      };
}
