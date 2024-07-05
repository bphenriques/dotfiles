{ config, lib, pkgs, ... }:
let
  logseqDotfiles = "/home/${config.home.username}/.dotfiles/home/config/logseq";
in
{
  # TODO: Create logseq configurations for .logseq/settings/
  home.packages = lib.optionals pkgs.stdenv.isLinux [ pkgs.logseq ];
  home.file = {
    ".logseq/config/plugins.edn".source = config.lib.file.mkOutOfStoreSymlink "${logseqDotfiles}/plugins.edn";
    ".logseq/config/config.edn".source = config.lib.file.mkOutOfStoreSymlink "${logseqDotfiles}/configs.edn";
  };
}
