{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "generate-oidc-client";
  runtimeInputs = [ pkgs.openssl ];
  text = lib.fileContents ./script.sh;
}
