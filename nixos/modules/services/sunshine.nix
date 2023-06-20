{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.sunshine;
in
{
  # TODO: Review systemd ? https://github.com/NixOS/nixpkgs/blob/e6272819a169325163735314cf796166943d5d75/nixos/modules/services/networking/syncthing.nix
  # TODO: Check https://github.com/francocalvo/nixos-eris/blob/main/modules/gaming/default.nix
  options.modules.services.sunshine = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = mdDoc ''Whether to enable sunshine service.'';
    };
  };

  # From https://docs.lizardbyte.dev/projects/sunshine/en/latest/about/usage.html#linux
  config = mkIf cfg.enable {
    warnings = [
      ''You have enabled sunshine which requires a wrapper to run as root which poses a security risk.''
    ];

    #  ++ lib.optionals (!config.services.avahi.enable && !config.services.avahi.userServices) [
          #      ''Avahi and userServices are disabled. Sunshine server will be not be discoverable but it is still acessible through its IP''
          #    ];


    # https://docs.lizardbyte.dev/projects/sunshine/en/latest/about/advanced_usage.html#port
    networking.firewall = {
      allowedTCPPorts = [ 47984 47989 47990 48010 ];
      allowedUDPPorts = [ 47998 47999 48000 48002 ];
    };

    # Make it work for KMS.
    # TODO: Should I migrate cap_sys_admin+p to CapabilityBoundingSet within the systemd?
    security.wrappers.sunshine = {
      owner = "root";
      group = "root";
      capabilities = "cap_sys_admin+p";
      source = "${pkgs.sunshine}/bin/sunshine";
    };

    # Requires to simulate input
    boot.kernelModules = [ "uinput" ];
    services.udev.extraRules = ''
      KERNEL=="uinput", SUBSYSTEM=="misc", OPTIONS+="static_node=uinput", TAG+="uaccess"
    '';

    environment.systemPackages = with pkgs; [ sunshine ];
    systemd.packages = with pkgs; [ sunshine ];

    systemd.user.services.sunshine-temp-v5 = {
      enable = true;
      description = "Starts Sunshine";
      wantedBy = ["graphical-session.target"];
      startLimitIntervalSec = 500;
      startLimitBurst = 5;
      serviceConfig = {
         Restart = "on-failure";
         RestartSec = 5;
         ExecStart = "${pkgs.sunshine}/bin/sunshine";
       };
    };

    #services.avahi = {
    #  enable = true;
    #  reflector = true;
    #  nssmdns = true;
    #  publish = {
    #    enable = true;
    #    addresses = true;
    #    userServices = true;
    #    workstation = true;
    #  };
    #};
  };
}
