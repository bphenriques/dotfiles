{ lib, pkgs, config, ... }:
{
  imports = [
    ./git.nix             # Version control
    ./direnv.nix          # Automate dev environment when we enter directories
    ./lang-scala.nix      # Programming language
    ./jetbrains.nix       # IDE
    ./claude-code.nix     # AI Assistant
    ./omp.nix             # AI Assistant
    ./helix.nix           # Editor
    ./zellij.nix          # Terminal multiplexer
  ];

  programs.parallel = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    package = pkgs.parallel;  # `parallel-full` default drags in 28MiB of SQL/niceload extras I don't use
    will-cite = true;
  };

  home.packages = lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    pkgs.yq-go            # Query YAML
    pkgs.dateutils
    pkgs.openssl
    pkgs.amp-cli          # AI Assistant
  ];

  programs.gpg = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    homedir = "${config.xdg.dataHome}/gnupg";
  };

  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    "z ${config.programs.gpg.homedir}       0700 ${config.home.username} users"
  ];
}
