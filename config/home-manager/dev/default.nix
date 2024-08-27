{ lib, pkgs, config, ... }:
{
  imports = [ ./git ./helix ];

  # Some of these packages should likely move to shell environment
  home.packages = with pkgs; [
    # Need to get familiar with working with it....
    # jetbrains.idea-community # TODO Add plugins: GraphQL, "Makefile Language" , Scala, Terraform and HCL,

    # Linters
    shellcheck        # Linter for shell scripts.
    shfmt             # Format shell scripts
    nixfmt-rfc-style  # Format nix files.

    # Tools
    gnumake
    rpi-imager
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    lima    # Virtual Machine -  limactl start --set='.cpus = 4 | .memory = "10GiB"'
    (pkgs.writeShellScriptBin "docker" ''${lima}/bin/lima nerdctl $@'')
  ] ++ lib.optionals (pkgs.stdenv.isLinux && config.custom.dotfiles.graphicalEnvironment) [
    filezilla   # Access remote files
  ];

  custom.impermanence = {
    filezilla = true;
    jetbrains = false;
  };
}
