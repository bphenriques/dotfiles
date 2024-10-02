{ lib, pkgs, config, ... }:
{
  imports = [
    ./git
    ./scala
    ./helix
  ];

  home.packages = with pkgs; [
    jetbrains.idea-community
  ] ++ lib.optionals (pkgs.stdenv.isLinux) [
    filezilla   # Access remote files
  ];
}
