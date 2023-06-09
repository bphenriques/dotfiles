{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    emacs

    # Doom emacs dependencies
    # https://github.com/hlissner/doom-emacs
    coreutils
    git
    ripgrep
    fd

    # Modules dependencies
    gcc         # Required to build sqlite if not compiled.
    sqlite      # Org Roam's database.
    plantuml    # To export Plant UML diagrams.
    xapian      # Notdeft's backend to index files.
    pandoc      # Preview markdown.
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    pngpaste
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    (makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "Org Protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeTypes = ["x-scheme-handler/org-protocol"];
    })
  ];

  xdg.configFile."doom".source = ./doom;
  home.shellAliases = {
    emacsclient = ''emacsclient --tty --alternate-editor ""''; # Start emacs daemon if not running already.
    killemacs   = ''emacsclient --eval "(kill-emacs)"'';
  };
}
