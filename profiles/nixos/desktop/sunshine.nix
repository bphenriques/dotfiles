{ pkgs, lib, config, ... }:
let
  # Sunshine runs with cap_sys_admin (setcap wrapper). Steam's bwrap sandbox rejects inherited capabilities.
  # setpriv clears ambient+inheritable caps so Steam launches cleanly.
  steamBigPicture = pkgs.writeShellScript "sunshine-steam-big-picture" ''
    exec ${lib.getExe' pkgs.util-linux "setsid"} \
      ${lib.getExe' pkgs.util-linux "setpriv"} \
        --ambient-caps=-all \
        --inh-caps=-all \
        ${lib.getExe pkgs.steam} steam://open/bigpicture
  '';
in
{
  services.sunshine = {
    enable = true;
    autoStart = false; # Start manually: `systemctl --user start sunshine`. Avoids session-order issues on Wayland.
    package = pkgs.sunshine.override { cudaSupport = true; };
    openFirewall = true;
    capSysAdmin = true; # Required for KMS capture on Wayland

    settings = {
      capture = "kms";      # Force KMS capture — avoids portal/other fallbacks on hybrid GPU setups
      encoder = "nvenc";    # Force NVIDIA encoder — prevents wandering into VAAPI/software on hybrid systems
      min_threads = 4;
      # output_name: set after checking `journalctl --user -u sunshine` for the correct monitor index
      # adapter_name: set to "/dev/dri/renderD129" (NVIDIA) only if Sunshine picks the wrong GPU
    };

    applications = {
      apps = [
        {
           name = "Steam Big Picture";
           output = "/tmp/sunshine-steam.txt";
           detached = ["${steamBigPicture}"];
           image-path = "steam.png";
        }
      ];
    };
  };

  # Sunshine needs access to /dev/uinput (virtual mouse/keyboard) and /dev/uhid (DS5 gamepad emulation).
  # The NixOS module runs Sunshine as a user service, so these devices must be group-accessible.
  services.udev.extraRules = lib.optionalString config.services.sunshine.enable ''
    KERNEL=="uinput", SUBSYSTEM=="misc", MODE="0660", GROUP="input", OPTIONS+="static_node=uinput"
    KERNEL=="uhid", SUBSYSTEM=="misc", MODE="0660", GROUP="input", OPTIONS+="static_node=uhid"
  '';
}
