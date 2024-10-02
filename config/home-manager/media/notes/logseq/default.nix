{ config, lib, pkgs, ... }:
let
  basePath = "${config.custom.dotfiles.directory}/home-manager/media/notes/logseq";
in
{
  # TODO: Create logseq configurations for .logseq/settings/
  home.packages = lib.optionals (pkgs.stdenv.isLinux) [ pkgs.logseq ];

  # TODO
  # It might be possible to work on this: https://github.com/hall/config/blob/cc519cc7bad40fb3a386dabfe24a1c909fd5d902/modules/stylix-logseq.nix#L3
  # https://github.com/calops/nix/blob/16cf492554f0a82b1b1ade49d6a29621846bc266/modules/home/programs/logseq.nix#L11
  home.file = lib.mkIf (pkgs.stdenv.isLinux) {
    ".logseq/config/plugins.edn".source = config.lib.file.mkOutOfStoreSymlink "${basePath}/plugins.edn";
    ".logseq/config/config.edn".source = config.lib.file.mkOutOfStoreSymlink "${basePath}/config.edn";
  };
}
