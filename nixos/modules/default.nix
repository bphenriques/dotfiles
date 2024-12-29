{
  proton-run                        = ./programs/proton-run.nix;
  services-input-remapper-profiles  = ./services/input-remapper.nix;
  services-solaar                   = ./services/solaar.nix;
  boot-theme                        = ./system/boot-theme.nix;
}
