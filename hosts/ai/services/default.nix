{
  imports = [
    ./llama-swap.nix
    ./monitoring.nix
    ./whisper.nix
  ];

  virtualisation.podman = {
    enable = true;
    autoPrune = {
      enable = true;
      dates = "weekly";
      flags = [ "--all" ]; # image tags pile up on every bump; only running containers keep theirs
    };
  };
}
