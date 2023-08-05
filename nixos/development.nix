{ pkgs, lib, config, ... }:
{
  environment.systemPackages = with pkgs; [
    jetbrains.idea-community
    filezilla

    # Raspberry Pi
    rpi-imager
  ];

  # TODO: Potentially move docker images outside, see https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/virtualisation/docker.nix#L56
  virtualisation.docker = {
    enable = true;
    enableNvidia = true; # test with: sudo docker run --gpus=all nvidia/cuda:12.2.0-base-ubuntu22.04 nvidia-smi. #FIXME: only work as sudo?
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };
}
