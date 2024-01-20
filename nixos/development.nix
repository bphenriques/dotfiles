{ pkgs, lib, config, ... }:
{
  programs.fish.enable = true;                          # System level: source vendor's completions or functions.
  users.users."${config.user.name}".shell = pkgs.fish;  # Fish is managed in Home-Manager. Root keeps the default shell.

  environment.systemPackages = with pkgs; [
    jetbrains.idea-community
    filezilla

    # Raspberry Pi
    rpi-imager
  ];

  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };
  user.extraGroups = ["docker"];
}

