{ config, lib, pkgs, ... }:

{
  xdg.configFile."amethyst/amethyst.yml".source = ./amethyst.yml;
}
