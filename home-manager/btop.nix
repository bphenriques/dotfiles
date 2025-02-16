{ pkgs, lib, config, ... }:
{
  programs.btop = {
    enable = true;
    settings = {
      theme_background = true;
      proc_gradient = false;
      graph_symbol = "block";
      shown_boxes = "cpu mem proc";
      clock_format = "";
    };
  };
  stylix.targets.btop.enable = true;
}
