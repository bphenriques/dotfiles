{ config, pkgs, lib, self, ... }:

import ./lib/mkArrService.nix { inherit config pkgs lib self; } {
  name = "radarr";
  port = 9098;
  description = "Movie Tracker";
  rootPath = config.custom.paths.media.movies;
  categoryField = "movieCategory";
  forwardAuthGroup = config.custom.home-server.groups.admin;
  downloadClient = "transmission";
}
