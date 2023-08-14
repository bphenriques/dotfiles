{ config, lib, pkgs, ... }:

# TODO:
# Explore: https://github.com/sei40kr/dotfiles/blob/master/modules/dev/scala.nix
{
  home.packages = with pkgs; [
    sbt         # Build tool.
    metals
    scalafmt    # Linter formatter.
    scala-cli   # Scala cli
    ammonite    # REPL.
    jdk17
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

