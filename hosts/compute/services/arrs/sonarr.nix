{ config, pkgs, lib, self, ... }:

import ./lib/mkArrService.nix { inherit config pkgs lib self; } {
  name = "sonarr";
  port = 9097;
  description = "TV Tracker";
  rootPath = config.custom.paths.media.tv;
  categoryField = "tvCategory";
  forwardAuthGroup = config.custom.home-server.groups.admin;
  downloadClient = "transmission";
}
