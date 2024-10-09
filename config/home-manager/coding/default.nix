{ lib, pkgs, config, network-devices, ... }:
let
  mkSSHMatchBlock = deviceCfg: {
    inherit (deviceCfg) hostname;
    inherit (deviceCfg.ssh) user port;
  };
in
{
  imports = [
    ./git.nix
    ./scala
    ./helix.nix
  ];

  programs.ssh = {
    enable = true;
    serverAliveInterval = 60;
    matchBlocks = {
      home-nas  = mkSSHMatchBlock network-devices.home-nas;
      pi-zero   = mkSSHMatchBlock network-devices.pi-zero;
      rg353m    = mkSSHMatchBlock network-devices.rg353m;
      deck      = mkSSHMatchBlock network-devices.deck;
    };

    extraConfig = ''
      Include ''${HOME}/.ssh/config.local
    '';
  };

  home.packages = with pkgs; [
    jetbrains.idea-community
  ] ++ lib.optionals (pkgs.stdenv.isLinux) [
    filezilla   # Access remote files
  ];
}
