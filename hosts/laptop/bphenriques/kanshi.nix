{ lib, pkgs, config, ... }:
let
  kanshictl = lib.getExe' pkgs.kanshi "kanshictl";

  # Moves all named niri workspaces to the given output.
  moveWorkspacesScript = pkgs.writeShellApplication {
    name = "niri-move-workspaces";
    runtimeInputs = [ pkgs.niri pkgs.jq ];
    text = ''
      target="$1"
      niri msg --json workspaces | jq -r '.[] | select(.name != null) | .name' | while read -r ws; do
        niri msg action move-workspace-to-monitor --reference "$ws" "$target" || true
      done
    '';
  };

  moveWorkspacesTo = output: "sleep 0.5 && ${lib.getExe moveWorkspacesScript} ${lib.escapeShellArg output}";

  mkScreen = { criteria, resolution, refreshRate, scale }: {
    inherit criteria resolution refreshRate scale;
    mode = "${resolution}@${refreshRate}Hz";
  };

  laptopScreen = mkScreen {
    criteria = "eDP-1";
    resolution = "2880x1800";
    refreshRate = "120.001";
    scale = 1.75;
  };
  dellScreen = mkScreen {
    criteria = "Dell Inc. DELL S2721DGF 4P11R83";
    resolution = "2560x1440";
    refreshRate = "143.92";
    scale = 1.0;
  };
  livingRoomScreen = mkScreen {
    criteria = "LG Electronics LG TV SSCR2 0x01010101";
    resolution = "2560x1440";
    refreshRate = "119.998";
    scale = 1.0;
  };

  toKanshi = screen: { inherit (screen) criteria mode scale; };
  enable = screen: toKanshi screen // { status = "enable"; };
  disable = screen: toKanshi screen // { status = "disable"; };
in
{
  services.kanshi = {
    enable = true;
    settings = [
      {
        profile = {
          name = "internal";
          outputs = [ (enable laptopScreen) ];
          exec = [ (moveWorkspacesTo laptopScreen.criteria) ];
        };
      }
      {
        profile = {
          name = "external-office";
          outputs = [ (disable laptopScreen) (enable dellScreen) ];
          exec = [ (moveWorkspacesTo dellScreen.criteria) ];
        };
      }
      {
        profile = {
          name = "external-living-room";
          outputs = [ (disable laptopScreen) (enable livingRoomScreen) ];
          exec = [ (moveWorkspacesTo livingRoomScreen.criteria) ];
        };
      }
    ];
  };

  custom.programs.wlr-which-key.menus = {
    display = [
      { key = "l"; desc = "Laptop";         cmd = "${kanshictl} switch internal"; }
      { key = "o"; desc = "Office monitor"; cmd = "${kanshictl} switch external-office"; }
      { key = "t"; desc = "Living room TV"; cmd = "${kanshictl} switch external-living-room"; }
      { key = "c"; desc = "Configure";      cmd = lib.getExe pkgs.wdisplays; }
    ];
    global = lib.mkAfter [
      { key = "d"; desc = "Display"; submenu = config.custom.programs.wlr-which-key.menus.display; }
    ];
  };

  custom.programs.niri.output.default = {
    identifier  = laptopScreen.criteria;
    inherit (laptopScreen) resolution;
    inherit (laptopScreen) refreshRate;
    scale       = toString laptopScreen.scale;
  };
}
