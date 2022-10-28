{ config, lib, pkgs, ... }:

let
  # MacOS: Pasting images to emacs
  pngpaste = pkgs.stdenv.mkDerivation rec {
    name = "pngpaste";
    buildInputs = [pkgs.darwin.apple_sdk.frameworks.Cocoa];
    src = pkgs.fetchFromGitHub {
      owner = "jcsalterego";
      repo="pngpaste";
      rev="67c39829fedb97397b691617f10a68af75cf0867";
      sha256="089rqjk7khphs011hz3f355c7z6rjd4ydb4qfygmb4x54z2s7xms";
    };
    installPhase = ''
      mkdir -p $out/bin
      cp pngpaste $out/bin/
    '';
  };
in
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
      mimeType = "x-scheme-handler/org-protocol";
    })
  ];

  xdg.configFile."doom".source = ./doom;
  home.shellAliases = {
    emacsclient = ''emacsclient --tty --alternate-editor ""''; # Start emacs daemon if not running already.
    killemacs   = ''emacsclient --eval "(kill-emacs)"'';
  };
}
