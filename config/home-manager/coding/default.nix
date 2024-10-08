{ lib, pkgs, config, ... }:
{
  imports = [
    ./git.nix
    ./scala
    ./helix.nix
    ./ssh.nix
  ];

  home.packages = with pkgs; [
    jetbrains.idea-community
  ] ++ lib.optionals (pkgs.stdenv.isLinux) [
    filezilla   # Access remote files
  ];
}
