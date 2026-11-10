# Screens and what switches them: niri holds the settings for each screen and the runtime
# on/off state, a menu entry toggles one screen, and a small reactor keeps the panel dark while
# anything else is plugged in. There is no profile to pick: a screen keeps its settings whatever
# port it is plugged into and whatever else is plugged in beside it.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  brightnessctl = "${lib.getExe pkgs.brightnessctl} --class=backlight";
  brightness = [ "${brightnessctl} set 5%-" "${brightnessctl} set 5%+" ];

  # The panel. niri only knows it by its connector name; the externals below are named by make,
  # model and serial so they keep their settings when they move to another port.
  panel = "eDP-1";

  # The screens the Display menu lists, in that order. Modes are the exact strings
  # `niri msg --json outputs` reports, refresh in mHz: the highest the screen accepts at the
  # resolution wanted. A mode niri cannot set it ignores, and picks one itself.
  screens = [
    {
      key = "1";
      name = panel;
      label = "Laptop";
      desc = "Laptop panel";
      mode = "2880x1800@120.001";
      scale = 1.75;
    }
    {
      key = "2";
      name = "Dell Inc. DELL S2721DGF 4P11R83";
      label = "Monitor";
      desc = "Office monitor";
      mode = "2560x1440@143.912";
      scale = 1.0;
    }
    {
      key = "3";
      name = "LG Electronics LG TV SSCR2 0x01010101";
      label = "TV";
      desc = "Living room TV";
      mode = "2560x1440@119.998";
      scale = 1.0;
    }
  ];

  # `niri msg output` takes the connector name, which for a monitor depends on the port it is
  # plugged into, so both the menu and the reactor below resolve a screen through it.
  displayctl = pkgs.writeShellApplication {
    name = "displayctl";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.jq
      pkgs.niri
      pkgs.systemd
    ];
    text = ''
      # The laptop panel, as niri names it.
      panel=${lib.escapeShellArg panel}

      # Switch one screen, or follow the panel to what else is plugged in.
      # A screen is named as the Display menu names it: a connector, or make, model and serial.
      toggle() {
        outputs=$(niri msg --json outputs)
        conn=$(echo "$outputs" | jq -r --arg want "$1" '
          to_entries[]
          | select(.key == $want or (([.value.make, .value.model, .value.serial] | join(" ")) == $want))
          | .key')
        # Not plugged in, and the menu has already greyed the entry out.
        [ -n "$conn" ] || return 0
        # An output turned off is still listed, only without its `logical` geometry.
        if echo "$outputs" | jq -e --arg conn "$conn" '.[$conn].logical' >/dev/null; then
          niri msg output "$conn" off
          # Nothing lit left is a black screen, so the panel takes over.
          if ! niri msg --json outputs | jq -e "any(.[]; .logical)" >/dev/null; then
            niri msg output "$panel" on || true
          fi
        else
          niri msg output "$conn" on
        fi
      }

      # Keep the panel dark while any other screen is on: plug something in and the laptop goes
      # out, unplug everything and it comes back.
      #
      # Only the set of *connected* screens is looked at, since switching one off with the menu
      # leaves it connected, so this never undoes what the menu just did.
      connected=""
      panel_follows() {
        outputs=$(niri msg --json outputs)
        now=$(echo "$outputs" | jq -r 'keys | sort | join(" ")')
        [ "$now" != "$connected" ] || return 0
        connected=$now
        if echo "$outputs" | jq -e --arg panel "$panel" 'any(keys[]; . != $panel)' >/dev/null; then
          niri msg output "$panel" off || true
        else
          niri msg output "$panel" on || true
        fi
      }

      # The kernel says so over udev: niri's IPC has no event for a screen arriving or leaving,
      # and a screen switched off is still listed, so a drm change is the only real news here.
      follow() {
        panel_follows
        udevadm monitor --subsystem-match=drm | while read -r _; do
          # A screen plugged in this moment has not been picked up by niri yet.
          sleep 0.3
          panel_follows
        done
      }

      case "''${1:-}" in
      toggle) toggle "''${2:-}" ;;
      follow) follow ;;
      *)
        echo "usage: displayctl toggle <screen> | follow" >&2
        exit 2
        ;;
      esac
    '';
  };

  # A menu entry is one command string, so the toggle is spelled out here once.
  toggleCmd = "${lib.getExe displayctl} toggle";
in
{
  home.packages = [ displayctl ];
  # The settings DSL writes each attrset key as a KDL node name, and an output is a
  # node with a string argument, so the screens go in as raw config instead.
  wayland.windowManager.niri.extraConfig = lib.mkAfter (
    lib.concatMapStrings (screen: ''
      output "${screen.name}" {
          mode "${screen.mode}"
          scale ${toString screen.scale}
      }
    '') screens
  );

  my.programs.desktop-shell = {
    settings = {
      osd.adjust.brightness = brightness;
      # The Display menu row lists the screens that are on by these names.
      display.labels = builtins.listToAttrs (
        map (screen: {
          inherit (screen) name;
          value = screen.label;
        }) screens
      );
    };
    menu.entries = lib.mkOrder 500 [
      {
        key = "d";
        desc = "Display";
        status = "display";
        submenu =
          [
            {
              key = [
                "Down"
                "Up"
              ];
              desc = "Brightness";
              cmd = brightness;
              keepOpen = true;
              requires.enabled = [ panel ];
            }
          ]
          ++ map (screen: {
            inherit (screen) key desc;
            cmd = "${toggleCmd} ${lib.escapeShellArg screen.name}";
            keepOpen = true;
            requires.connected = [ screen.name ];
            onOutput = screen.name;
          }) screens
          ++ [
            {
              key = "c";
              desc = "Configure...";
              cmd = lib.getExe pkgs.wdisplays;
            }
          ];
      }
    ];
  };

  systemd.user.services.display-policy = {
    Unit = {
      description = "Keep the laptop panel off while an external screen is connected";
      PartOf = [ config.wayland.systemd.target ];
      After = [ config.wayland.systemd.target ];
      X-Restart-Triggers = [ "${displayctl}" ];
    };
    Service = {
      ExecStart = "${lib.getExe displayctl} follow";
      Restart = "always";
      RestartSec = 5;
    };
    Install.WantedBy = [ config.wayland.systemd.target ];
  };
}
