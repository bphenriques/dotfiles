{ config, lib, pkgs, ... }:
# TODO
# https://github.com/shazow/nixfiles/blob/main/home/bin/volumectl
# https://github.com/mitchellh/nixos-config/blob/main/users/mitchellh/rofi

let
  font = {
    variable = "Hack Nerd Font Mono";
  };
  colorScheme = {
    palette = {
      base00 = "#000000";
      base01 = "#100323";
      base02 = "#3C3C3C";
      base03 = "#595959";
      base04 = "#BEBCBF";
      base05 = "#FFFFFF";
      base06 = "#EDEAEF";
      base07 = "#FFFFFF";
      base08 = "#FF8059";
      base09 = "#EF8B50";
      base0A = "#D0BC00";
      base0B = "#44BC44";
      base0C = "#00D3D0";
      base0D = "#2FAFFF";
      base0E = "#FEACD0";
      base0F = "#B6A0FF";
    };
  };
in
{
  programs.wofi = {
    enable = pkgs.stdenv.isLinux;
    settings = {
      width = 800;
      height = 400;
      insensitive = true;
      mode = "drun,run";
    };

    style = ''
      window {
        border: 2px solid #${colorScheme.palette.base03};
        background-color: #${colorScheme.palette.base00};
      }

      #input {
        color: #${colorScheme.palette.base09};
        border: 2px solid #${colorScheme.palette.base03};
        background-color: #${colorScheme.palette.base00};
        font-size: 13px;
        font-family: ${font.variable};
      }

      #outer-box {
        margin: 10px;
      }

      #scroll {
        margin: 5px 0px;
        font-size: 13px;
        font-family: ${font.variable};
        color: #${colorScheme.palette.base06};
      }

      #scroll label {
        margin: 2px 0px;
      }

      #entry:selected {
        color: #${colorScheme.palette.base06};
        background-color: #${colorScheme.palette.base00};
      }
    '';
  };
}
