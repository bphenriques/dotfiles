{ pkgs, lib, config, ... }:

{
  # Set zsh shell
  users.defaultUserShell = pkgs.zsh;
  environment.shells = with pkgs; [ zsh ];

  imports = [
    ./gaming.nix
    ./media.nix
    ./development.nix
  ];

  # Basic programs
  environment.systemPackages = with pkgs; [
    ark             # Zip/Unzip
  ];

  # Localization
  time.timeZone = "Europe/Lisbon";
  i18n = {
    defaultLocale = "en_GB.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "pt_PT.UTF-8";
      LC_IDENTIFICATION = "pt_PT.UTF-8";
      LC_MEASUREMENT = "pt_PT.UTF-8";
      LC_MONETARY = "pt_PT.UTF-8";
      LC_NAME = "pt_PT.UTF-8";
      LC_NUMERIC = "pt_PT.UTF-8";
      LC_PAPER = "pt_PT.UTF-8";
      LC_TELEPHONE = "pt_PT.UTF-8";
      LC_TIME = "pt_PT.UTF-8";
    };
  };
}
