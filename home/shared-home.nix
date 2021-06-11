{ config, pkgs, lib, ... }:
{
  # Basic packages
  home.packages = with pkgs; [
    # Common Tools:
    coreutils   # Consistency across different Operating Systems.
    watch       # Commands on loop.
    tree        # File navigation.
    ripgrep     # Alternative to grep. Use with `rg`.
    fzf         # Fuzzy search.
    jq          # Json Processor.
    bat         # Preview with code highlight.
    fd          # A better `find`.
    gnupg       # To manage GNUPG keys.
    pinentry    # To input keys.
    
    # Other tools that I use from time to time:
    httpie      # Alternative to curl. Use with `http`.
    ngrok       # Tunneling.
    hugo        # Blogging.
    parallel    # Useful to parallelize tasks.

    # Tools I am experimenting:
    htop        # Fancy `top`.
    # Unstable: bottom      # Fancy `top` with ASCII graphics. Use with `btm`.
    exa         # Fancy `ls`.
  ];

  imports = [
    ./config/git
    ./config/scala
    ./config/emacs
    ./config/alacritty
    ./config/zsh
    ./config/fzf
    ./config/tmux
    
    # Packages I am experimenting with.
    ./config/neovim
    ./config/vscodium
  ];

  # This value determines the Home Manager release that your configuration is compatible with. This
  # helps avoid breakage when a new Home Manager release introduces backwards incompatible changes.
  #
  # You can update Home Manager without changing this value. See the Home Manager release notes for
  # a list of state version changes in each release.
  home.stateVersion = "21.03";
}
