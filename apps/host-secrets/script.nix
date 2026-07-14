{ flake, host }:
let
  nixos = flake.nixosConfigurations.${host};
  inherit (nixos.pkgs) lib formats;
in
builtins.readFile ((formats.yaml { }).generate "host-secrets-example.yaml" (
  builtins.foldl'
    (acc: key: lib.recursiveUpdate acc (lib.setAttrByPath (lib.splitString "/" key) "secret"))
    { }
    (builtins.attrNames nixos.config.sops.secrets)))
