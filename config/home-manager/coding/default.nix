{ lib, pkgs, config, headless, ... }:
{
  imports = [ ./git ];

  # https://github.com/0xcharly/nix-config/blob/a8e1427a67494ad5de3d639d94ee619ca69f51c7/users/delay/home.nix#L99 ?

  # Some of these packages should likely move to shell environment
  home.packages = with pkgs; [
    # Need to get familiar with working with it....
    jetbrains.idea-community # TODO Add plugins: GraphQL, "Makefile Language" , Scala, Terraform and HCL,

    # Linters
    shellcheck        # Linter for shell scripts.
    shfmt             # Format shell scripts
    nixfmt-rfc-style  # Format nix files.
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    lima    # Virtual Machine -  limactl start --set='.cpus = 4 | .memory = "10GiB"'
    (pkgs.writeShellScriptBin "docker" ''${lima}/bin/lima nerdctl $@'')
  ] ++ lib.optionals (pkgs.stdenv.isLinux && !headless) [
    filezilla   # Access remote files
  ];
}
