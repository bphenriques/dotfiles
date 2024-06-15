# Workaround for https://github.com/NixOS/nixpkgs/issues/73323
# Based on https://github.com/nix-community/nur-combined/blob/master/repos/ataraxiasjel/pkgs/proton-ge/default.nix#
{ stdenv, lib, fetchurl, writeScript, ... }:
stdenv.mkDerivation (finalAttrs: {
  name = "proton-ge-custom";
  version = "GE-Proton9-7";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${finalAttrs.version}/${finalAttrs.version}.tar.gz";
    hash = "sha256-4jh3mDm1GpR9tQBkzx9BRsJ5awNrBgRzylyRB04LqIE=";
  };

  buildCommand = ''
    runHook preBuild
    mkdir -p $out/bin
    tar -C $out/bin --strip=1 -x -f $src
    runHook postBuild
  '';

  meta = with lib; {
    description = "Compatibility tool for Steam Play based on Wine and additional components";
    homepage = "https://github.com/GloriousEggroll/proton-ge-custom";
    license = licenses.bsd3;
    platforms = [ "x86_64-linux" ];
    maintainers = with maintainers; [ bphenriques ];
  };
})
