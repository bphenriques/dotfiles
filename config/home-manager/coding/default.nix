{ lib, pkgs, config, headless, ... }:
{
  imports = [ ./git ];

  # TODO: Explore jujutsu: https://github.com/0xcharly/nix-config/blob/a8e1427a67494ad5de3d639d94ee619ca69f51c7/users/delay/home.nix#L99 ?

  # Some of these packages should likely move to shell environment
  home.packages = with pkgs; [
    jetbrains.idea-community
  ] ++ lib.optionals (pkgs.stdenv.isLinux && !headless) [
    filezilla   # Access remote files
  ];
}
