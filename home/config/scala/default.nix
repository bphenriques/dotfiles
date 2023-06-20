{ config, lib, pkgs, ... }:

# TODO:
# Explore: https://github.com/sei40kr/dotfiles/blob/master/modules/dev/scala.nix
{
  home.packages = with pkgs; [
    sbt         # Build tool.
    scalafmt    # Linter formatter.
    scala-cli   # Scala cli
    ammonite    # REPL.
    jdk17
  ];
  
  # Ammonite is not XDG_CONFIG_HOME compliant: https://github.com/lihaoyi/Ammonite/issues/696
  home.file.".ammonite/predef.sc".source = ./ammonite-predef.sc;
}

