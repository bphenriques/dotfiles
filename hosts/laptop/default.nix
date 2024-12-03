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
    inherit inputs;
    host.webcam = "/dev/video0";  # FIXME: This only needed once, let's move mvp to a module and make this a parameter.
    network-devices = import ../network-devices.nix;
    monitors = {
      "eDP-1" = {
        description = "built-in";
        mode = "2880x1800@143.91Hz";
        scale = 1.5;
      };
      "Dell Inc. DELL S2721DGF 4P11R83" = {
        description = "Office Monitor";
        mode = "2560x1440@120.00Hz";
        scale = 1.0;
      };
    };
  };
  hostModule = ./config.nix;
}
