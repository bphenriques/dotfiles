{ home-manager, nixpkgs, ... }:

{
  mkHMHost = { system ? "x86_64-linux", username, homeConfig}:
    home-manager.lib.homeManagerConfiguration {
        inherit system username;
        homeDirectory = "/home/${username}";
        stateVersion = "22.11";
        configuration = { pkgs, ... }: {
            inherit nixpkgs;
            imports = [ homeConfig ];
        };
    };
}
