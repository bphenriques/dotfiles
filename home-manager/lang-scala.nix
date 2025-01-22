{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    sbt         # Build tool.
    metals      # If Helix doesn't pick up, call: sbt bloopInstall
    scalafmt    # Linter formatter.
    scala-cli   # Scala cli
    ammonite    # REPL.
    jdk21
  ];

  # TODO:
  #programs.sbt = {
  #  enable = true;
  #  plugins = [ { org, artifact, version} ]
  #  pluginsExtra = 'addDependencyTreePlugin' # might not neede it
  #  credentials = ???
  #};

  systemd.user.services.bloop = lib.optionalAttrs pkgs.stdenv.isLinux {
    Unit.Description = "Bloop Scala build server";
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.bloop}/bin/bloop server";
      Restart = "always";
      Environment = [ "PATH=${lib.makeBinPath [ pkgs.jdk17 ]}" ];
    };
  };

  # Ammonite is not XDG_CONFIG_HOME compliant: https://github.com/lihaoyi/Ammonite/issues/696
  home.file.".ammonite/predef.sc".text = ''
    object load {
      def fs2Version(version: String): Unit = {
        repl.load.apply(s"""
          import $$ivy.`co.fs2::fs2-core:$version`
          import $$ivy.`co.fs2::fs2-reactive-streams:$version`
          import $$ivy.`co.fs2::fs2-io:$version`

          import cats.syntax.all._
          import cats.effect.{IO, Resource}
          import fs2.io.file.{Files, Path}
          import fs2.{Stream, text}

          // For unsafeRunSync
          implicit val runtime = cats.effect.unsafe.IORuntime.global
        """)
      }

      def fs2: Unit = fs2Version("3.11.0")
    }
  '';

  programs.git.ignores = [
    ".metals"
    ".ammonite"
    ".bsp"
    ".scala-build"
  ];
}

