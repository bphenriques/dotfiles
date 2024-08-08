{ config, lib, pkgs, ... }:
let
  logseqDotfiles = "${config.custom.dotfiles.directory}/home/logseq";
  pluginsPath = "${logseqDotfiles}/plugins.edn";
  configPath = "${logseqDotfiles}/config.edn";
in
{
  # TODO: Create logseq configurations for .logseq/settings/
  home.packages = lib.optionals pkgs.stdenv.isLinux [ pkgs.logseq ];
  home.file = {
    ".logseq/config/plugins.edn".source = config.lib.file.mkOutOfStoreSymlink pluginsPath;
    ".logseq/config/config.edn".source = config.lib.file.mkOutOfStoreSymlink configPath;
  };
}
