{ pkgs, lib, config, ... }:
let
  # Steam's bwrap rejects the inherited cap_sys_admin, so setpriv clears ambient and inheritable caps.
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
    autoStart = true;
    package = pkgs.sunshine.override { cudaSupport = true; }; # TODO Review why
    openFirewall = true;
    capSysAdmin = true; # Required for KMS capture on Wayland

    settings = {
      # Nvidia
      capture = "kms";      # Force KMS capture: avoids portal/other fallbacks on hybrid GPU setups
      encoder = "nvenc";    # Force NVIDIA encoder: prevents wandering into VAAPI/software on hybrid systems

      min_threads = 4;
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

  # TODO: still required? The user service needs group access to /dev/uinput and /dev/uhid.
  services.udev.extraRules = lib.optionalString config.services.sunshine.enable ''
    KERNEL=="uinput", SUBSYSTEM=="misc", MODE="0660", GROUP="input", OPTIONS+="static_node=uinput"
    KERNEL=="uhid", SUBSYSTEM=="misc", MODE="0660", GROUP="input", OPTIONS+="static_node=uhid"
  '';
}
