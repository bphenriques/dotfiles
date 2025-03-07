{ pkgs, lib, config, self, osConfig, ... }:
{
  programs.btop = {
    enable = true;
    package = lib.mkIf pkgs.stdenv.isLinux (pkgs.btop.override {
      # Enable Nvidia only if sync is enabled (GPU is always on). Otherwise, the external GPU might activate: https://github.com/aristocratos/btop/issues/808
      cudaSupport = (builtins.elem "nvidia" osConfig.services.xserver.videoDrivers) && osConfig.hardware.nvidia.prime.sync.enable;
      rocmSupport = osConfig.hardware.amdgpu.amdvlk.enable || osConfig.hardware.amdgpu.opencl.enable;
    });
    settings = {
      theme_background = true;
      proc_gradient = false;
      graph_symbol = "block";
      shown_boxes = "cpu mem proc";
    };
  };
  stylix.targets.btop.enable = true;

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    (pkgs.makeDesktopItem {
      name = "system-monitor";
      desktopName = "System Monitor";
      icon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } "system-monitor" "";
      exec = ''${lib.getExe' pkgs.foot "footclient"} --title=btop-tui ${lib.getExe config.programs.btop.package}'';
    })
  ];
}
