{ config, lib, pkgs, ... }:

let
  # Pasting images to emacs
  pngpaste = pkgs.stdenv.mkDerivation rec {
    src = pkgs.fetchFromGitHub {
      owner = "jcsalterego";
      repo="pngpaste";
      rev="67c39829fedb97397b691617f10a68af75cf0867";
      sha256="089rqjk7khphs011hz3f355c7z6rjd4ydb4qfygmb4x54z2s7xms";
    };
    name = "pngpaste";
    buildInputs = [pkgs.darwin.apple_sdk.frameworks.Cocoa];
    installPhase = ''
      mkdir -p $out/bin
      cp pngpaste $out/bin/
    '';
  };
in
{
  home.packages = with pkgs; [
    emacsNativeComp

    # Doom emacs dependencies
    # https://github.com/hlissner/doom-emacs
    coreutils   # Consistency across different Operating Systems.
    git         # Doom Emacs's dependency. Should be already installed.
    ripgrep     # Doom Emacs's dependency. Alternative to grep.
    fd          # Doom Emacs's dependency. Faster and more user-friendly find.

    # Modules dependencies
    sqlite      # Org Roam's database.
    plantuml    # To export Plant UML diagrams.
    xapian      # Notdeft's backend to index files.
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

  xdg.configFile = {
    "doom".source = ./doom;
  };
}
