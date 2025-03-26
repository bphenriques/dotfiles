{ lib, pkgs, preview, ... }:
pkgs.stdenv.mkDerivation rec {
  name = "project";
  src = ./.;
  buildCommand = let
    script = pkgs.writeShellApplication {
      name = name;
      runtimeInputs = [ pkgs.fzf pkgs.fd preview ];
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