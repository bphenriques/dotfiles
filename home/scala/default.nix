{ config, lib, pkgs, ... }:

# TODO: add this to default sbt: addSbtPlugin("com.github.cb372" % "sbt-explicit-dependencies" % "0.3.1")
{
  home.packages = with pkgs; [
    sbt         # Build tool.
    metals      # If Helix doesn't pick up, call: sbt bloopInstall
    scalafmt    # Linter formatter.
    scala-cli   # Scala cli
    ammonite    # REPL.
    jdk21
  ];

  systemd.user.services.bloop = lib.mkIf pkgs.stdenv.isLinux {
    Unit.Description = "Bloop Scala build server";
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.bloop}/bin/bloop server";
      Restart = "always";
      Environment = [ "PATH=${lib.makeBinPath [ pkgs.jdk17 ]}" ];
    };
  };
  
  # Ammonite is not XDG_CONFIG_HOME compliant: https://github.com/lihaoyi/Ammonite/issues/696
  home.file.".ammonite/predef.sc".source = ./ammonite-predef.sc;
}

