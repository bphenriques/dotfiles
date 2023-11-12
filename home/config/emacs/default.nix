{ config, lib, pkgs, ... }:

let
  # The path needs to be absolute: https://github.com/nix-community/home-manager/pull/1455#issuecomment-681041818
  doomEmacsCfg = "${config.home.homeDirectory}/.dotfiles/home/config/emacs/doom";
in
{
  home.packages = with pkgs; [
    emacs29

    # Doom emacs dependencies (https://github.com/hlissner/doom-emacs)
    coreutils
    git
    ripgrep
    fd

    # Modules dependencies
    gcc                               # Required to build sqlite if not compiled.
    sqlite                            # Org Roam's database.
    xapian                            # Notdeft's backend to index files.
    pandoc                            # Universal file converter (e.g., `org-mode` -> `markdown` -> `html`)

    # Helpers when coding
    python311Packages.grip            # Preview Markdown
    nodePackages.bash-language-server # LSP for Bash
    nodePackages.yaml-language-server # LSP for YAML

    # Plain writting. Language Tools is not great for me, therefore opting for just spelling.
    wordnet                           # Offline dictionary
    hunspell                          # Spell Check
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    pngpaste
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    pkgs.emacs-all-the-icons-fonts    # Emacs required font
    (makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "Org Protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeTypes = ["x-scheme-handler/org-protocol"];
    })
  ];

  home.sessionVariables.DICPATH = "${config.xdg.configHome}/hunspell";
  xdg.configFile = {
    "hunspell/en_US.aff".source = "${pkgs.hunspellDicts.en_US}/share/hunspell/en_US.aff";
    "hunspell/en_US.dic".source = "${pkgs.hunspellDicts.en_US}/share/hunspell/en_US.dic";
    "hunspell/pt_PT.aff".source = "${pkgs.hunspellDicts.pt_PT}/share/hunspell/pt_PT.aff";
    "hunspell/pt_PT.dic".source = "${pkgs.hunspellDicts.pt_PT}/share/hunspell/pt_PT.dic";
  };


  # Workaround to avoid having to recompile the whole nix project to iterate on my config.
  # mkOutOfStoreSymlink allows me to create a file outside of the store. I.e., to the actual file in the repo.
  xdg.configFile."doom".source = config.lib.file.mkOutOfStoreSymlink "${doomEmacsCfg}";

  home.shellAliases = {
    emacscli  = ''emacsclient --tty --alternate-editor ""''; # Start emacs daemon if not running already. Similar to 'emacseditor': https://github.com/NixOS/nixpkgs/blob/e909d72f7f71ae13c7ad70133541e07d28f7ae22/nixos/modules/services/editors/emacs.nix#L7
    emacskill = ''emacsclient --eval "(kill-emacs)"'';
  };
}
