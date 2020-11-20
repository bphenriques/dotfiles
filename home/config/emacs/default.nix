{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Doom emacs dependencies
    # https://github.com/hlissner/doom-emacs
    coreutils   # Consistency across different Operating Systems.
    git         # git should be already installed.
    ripgrep     # Alternative to grep.
    fd          # Faster and more user-friendly find.

    # Modules dependencies
    sqlite      # Org Roam.
    plantuml    # Plant UML.
    
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
