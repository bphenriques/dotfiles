{ config, pkgs, lib, ... }:

{
    system = "x86_64-linux";
    homeDirectory = "/home/bphenriques";
    username = "bphenriques";
    stateVersion = "21.05";
    configuration = { pkgs, ... }: {
        imports = [ ../home/shared-home.nix ];
        nixpkgs = pkgs;
    };
}
