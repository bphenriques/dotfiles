{ lib, pkgs, ... }:

with lib;
pkgs.writeShellApplication {
    name = "frg";
    runtimeInputs = with pkgs; [
      ripgrep
      fzf
    ];
    text = fileContents ./frg.sh;
  }
