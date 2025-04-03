{ config, lib, pkgs, ... }:
{
  home.packages = [
    pkgs.metals      # Build Server
    pkgs.scala-cli   # Scala cli
    pkgs.jdk21
  ];

  programs.sbt.enable = true;
  programs.git.ignores = [
    ".metals"
    ".bsp"
    ".scala-build"
  ];
}

