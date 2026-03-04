{ pkgs, lib, self, ... }:
{
  imports = [ ../common.nix ];

  system.nixos.label = let
    rev = self.shortRev or self.dirtyShortRev or "dirty";
    # self.lastModifiedDate is YYYYMMDDHHMMSS format -> YYMMdd-HHmm.shortRev'
    date = self.lastModifiedDate or "00000000000000";
    fmt = "${builtins.substring 2 6 date}-${builtins.substring 8 4 date}";
  in "${fmt}.${rev}";

  # Pin nixpkgs registry to flake input (replaces nix-channel for flake workflows)
  # Allows: nix shell nixpkgs#hello, nix run nixpkgs#cowsay, etc.
  nix.registry.nixpkgs.flake = self.inputs.nixpkgs;

  boot = {
    tmp.cleanOnBoot = true; # Not enabling useTmpfs despite having enough RAM. Might consider it.
    kernelParams = [
      "boot.shell_on_fail" # allows for root shell if failure to boot
    ];
  };

  # Localization
  time.timeZone = "Europe/Lisbon";
  i18n = {
    defaultLocale = "en_GB.UTF-8";
    extraLocaleSettings = lib.genAttrs [
      "LC_ADDRESS" "LC_IDENTIFICATION" "LC_MEASUREMENT" "LC_MONETARY"
      "LC_NAME" "LC_NUMERIC" "LC_PAPER" "LC_TELEPHONE" "LC_TIME"
    ] (_: "pt_PT.UTF-8");
  };

  environment.systemPackages = let
    filesystems = [ pkgs.exfat pkgs.ntfs3g ]; # Support exFAT and NTFS
    hardware    = [
      pkgs.powertop   # Check what is consuming too much energy
      pkgs.usbutils   # USB utilities
    ];
  in filesystems ++ hardware;

  programs.fish.enable = true;  # System level.

  # Disabling some defaults
  programs.command-not-found.enable = false;
  programs.nano.enable = false;

  # Security
  services.journald.extraConfig = ''
    MaxRetentionSec=1month
    SystemMaxUse=1G
  '';
  security.sudo.extraConfig = "Defaults lecture=never";

  # Misc
  home-manager.useGlobalPkgs    = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages  = true;   # Install packages defined in home-manager.
  documentation.nixos.enable    = false;  # Disable generating NixOS configuration options
}
