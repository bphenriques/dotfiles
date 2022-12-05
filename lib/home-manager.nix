{ home-manager, homeManagerModules, nixpkgs, nixpkgsConfig, lib, ... }:
{
  mkHomeManagerHost = { system, username, hostModules ? [] }:
    let
      inherit (lib) attrValues;
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
        modules = [common] ++ attrValues homeManagerModules ++ hostModules;
      };
}
