{ pkgs, lib, config, ... }:
{
  services.sunshine = {
    enable = true;
    openFirewall = true;
    capSysAdmin = true;
    applications = {
      env = {
        PATH = "$(PATH):$(HOME)/.local/bin";
      };
      apps = [
        {
          name = "Desktop";
          image-path = "desktop.png";
        }
        {
           name = "Steam Big Picture";
           output = "/tmp/sunlight-steam.txt";
           detached = ["${pkgs.util-linux}/bin/setsid ${pkgs.steam}/bin/steam steam://open/bigpicture"];
           image-path = "steam.png";
        }
      ];
    };
  };
  # Required to simulate input in sunshine
  services.udev.extraRules = lib.optionalString config.services.sunshine.enable ''
    KERNEL=="uinput", SUBSYSTEM=="misc", OPTIONS+="static_node=uinput", TAG+="uaccess"
  '';
}

# https://github.com/azuwis/nix-config/blob/d29d918097e5da916be5762255fb418c657860bc/nixos/sunshine/home.nix#L17
# https://github.com/lucasew/nixcfg/blob/90505958b98379ac19d7c952a27bd7bce8714816/nix/nodes/gui-common/sunshine.nix#L9
# https://github.com/aostanin/nixos-config/blob/c73238deae8538bc6cd0b885f108255e60c60e94/nixos/modules/headless-gaming.nix#L12

# Enable using:
# services.sunshine.enable = true;
# Get Service Status
# systemctl --user status sunshine
# get logs
# journalctl --user -u sunshine --since "2 minutes ago"