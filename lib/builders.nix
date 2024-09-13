{ lib, inputs, ... }:
{
  forAllSystems = lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
  forDarwinSystems = lib.genAttrs [ "aarch64-darwin" ];
  forLinuxSystems = lib.genAttrs [ "x86_64-linux" "aarch64-linux" ];

  # Replace the interpreter's location to be one under the nix store.
  # - writeShellApplication does not support that.
  # - we now have to pass the dependencies by-hand
  # - we now lose shellchecks at build time. It is fine in this case as I usually run by hand.
  #
  # Why: I like the option of running the scripts locally.
  writeLocalCompatibleScriptBin = pkgs: { name, text, runtimeInputs }:
    let
      patchShebangs = pkg: pkg.overrideAttrs(old: {
        buildCommand = "${old.buildCommand}\n patchShebangs $out";
      });
    in pkgs.symlinkJoin {
      inherit name;
      paths = [ (patchShebangs (pkgs.writeScriptBin name text)) ] ++ runtimeInputs;
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
    };
}