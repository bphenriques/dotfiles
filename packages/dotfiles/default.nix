{ lib, pkgs, ... }:
pkgs.stdenv.mkDerivation rec {
  name = "dotfiles";
  src = ./.;
  buildCommand = let
    script = pkgs.writeShellApplication {
      name = name;
      runtimeInputs = [ ];
      text = lib.fileContents ./script.sh;
      meta.platforms = lib.platforms.all;
    };
    fishPlugin = ./fish-plugin;
  in ''
    mkdir -p $out/bin
    cp ${script}/bin/${name} $out/bin

    mkdir -p $out/share/fish/vendor_functions.d
    mkdir -p $out/share/fish/vendor_conf.d
    cp -r ${fishPlugin}/functions $out/share/fish/vendor_functions.d
    cp -r ${fishPlugin}/conf.d $out/share/fish/vendor_conf.d
  '';
  dontBuild = true;
}