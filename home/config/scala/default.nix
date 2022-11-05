{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    sbt                     # Build tool.
    scalafmt                # Linter formatter.
    scala-cli      # Scala cli
    ammonite                # REPL.
  ];
  
  # Ammonite is not XDG_CONFIG_HOME compliant: https://github.com/lihaoyi/Ammonite/issues/696
  home.file.".ammonite/predef.sc".source = ./ammonite-predef.sc;
}

