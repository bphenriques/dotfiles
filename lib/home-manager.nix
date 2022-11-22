{ home-manager, homeManagerModules, nixpkgs, nixpkgsConfig, }:
{
  mkHomeManagerHost = { system, username, hostModules ? [] }:
    let
      common = {
        home = {
          inherit username;
          homeDirectory = "/home/${username}";
        };
        imports = [../home/common.nix];
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
