{ inputs, mylib, ... }:
let
  inherit (inputs.nixpkgs.lib.attrsets) attrValues;
  # Ideally modules are imported in the file that uses it. However, it leads to a infinite recursion. Aka, a rabbit-hole to debug.
  overlays = attrValues inputs.self.overlays ++ [ inputs.nur.overlay ];
  nixosModules = attrValues inputs.self.nixosModules ++ [
    inputs.sops-nix.nixosModules.sops
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
  ];
  hmModules = attrValues inputs.self.homeManagerModules ++ [
    inputs.ags.homeManagerModules.default
  ];
in mylib.hosts.mkNixOSHost {
  inherit nixosModules hmModules overlays;
  extraSpecialArgs = {
    host.webcam = "/dev/video0";
    network-devices = import ../network-devices.nix;
  };
  hostModule = ./config.nix;
}
