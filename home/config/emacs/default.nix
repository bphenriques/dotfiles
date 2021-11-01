{ config, lib, pkgs, ... }:

let
  # See options here: https://github.com/nix-community/emacs-overlay
  # https://emacs.stackexchange.com/questions/33065/on-linux-why-should-one-choose-lucid-over-gtk-gui-for-emacs
  # Still not sure if I am going to be full-blown terminal for Emacs.
  # TODO: Review why this happens:
  # EmacsGCC: 29.50
  # EmacsPgtkGcc: 28.50
  emacs-package = if pkgs.stdenv.isDarwin then pkgs.emacsGcc else pkgs.emacsPgtkGcc;

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
    emacs-package

    # Doom emacs dependencies
    # https://github.com/hlissner/doom-emacs
    coreutils   # Consistency across different Operating Systems.
    git         # git should be already installed.
    ripgrep     # Alternative to grep.
    fd          # Faster and more user-friendly find.

    # Modules dependencies
    sqlite      # Org Roam.
    plantuml    # Plant UML.
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
