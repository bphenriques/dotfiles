{ config, lib, pkgs, ... }:
let
  logseqDotfiles = "${config.custom.dotfiles.directory}/home/logseq";
  pluginsPath = "${logseqDotfiles}/plugins.edn";
  configPath = "${logseqDotfiles}/config.edn";
in
{
  assertions = [
    {
      assertion = config.custom.dotfiles.enable;
      message = "dotfiles module is not enabled. It is required to access logseq mutable configuration files";
    }
  ];

  # TODO: Create logseq configurations for .logseq/settings/
  home.packages = lib.optionals pkgs.stdenv.isLinux [ pkgs.logseq ];
  home.file = {
    ".logseq/config/plugins.edn".source = config.lib.file.mkOutOfStoreSymlink pluginsPath;
    ".logseq/config/config.edn".source = config.lib.file.mkOutOfStoreSymlink configPath;
  };
}
