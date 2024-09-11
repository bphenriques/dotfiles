{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    lazygit                         # Cross-platform GUI to interact with Git
    git-absorb                      # Trying https://github.com/tummychow/git-absorb
  ];

  programs.git = {
    enable = true;
    userName = "Bruno Henriques";
    userEmail = "4727729+bphenriques" + "@" + "users.noreply.github.com"; # Minor obsfuscation to prevent webscrappers

    signing = {
      key = "792C2768AD3A4930BCCFA467075389B5C3ADA858";
      signByDefault = true;
    };

    aliases = {
      co        = "checkout";
      a         = "add";
      aa        = "add --all";
      s         = "status -s";
      p         = "pull --rebase";
      cp        = "cherry-pick";
      rb        = "rebase";
      pushf     = "push --force-with-lease";
      amend     = "commit --amend";
      wip       = ''!git add --all && git commit -m "WIP"'';
      wipp      = "!git wip && git push";

      # Tools
      patch   = "!git --no-pager diff --no-color";
      fix-upstream = "!git branch --set-upstream-to=origin/$(git rev-parse --abbrev-ref HEAD) $(git rev-parse --abbrev-ref HEAD)";
      root = "!git rev-parse --show-toplevel";

      # History
      ls      = "log --pretty=format:'%C(yellow)%h%d %Creset%s%Cblue [%cn] [%ar] %C(black)%C(bold)%cr%Creset' --decorate";
      graph   = "log --graph --date=relative --pretty=tformat:'%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%an %ad)%Creset'";
    };

    ignores = [
      # Visual Code
      ".vscode"

      # IntelliJ files
      ".idea"
      ".iml"

      # Vim
      "*.swp"
      "*.swo"

      # NPM
      "node_modules/"

      # Python
      "*.pyc"
      "*.pyo"

      # MacOS
      ".DS_Store"
      ".DS_Store?"
      ".Spotlight-V100"
      ".Trashes"

      # direnv temporary directory
      ".direnv/"

      # Personal tools
      "bphenriques-tools/"
    ];

    diff-so-fancy.enable = true;

    extraConfig = {
      commit.template =  builtins.toPath ./git-message;
      tag = {
        gpgSign = true;
        sort = "version:refname";     # Natural order of commits
      };

      pull.rebase = false; # Not enabled by default as I usually collaborate with others on the same branch.
      push.autoSetupRemote = true;
      rebase.autosquash = true; # Automatic fixup! and/or squash!

      fetch = {
        prune = true;
        pruneTags = true;
      };
      branch.sort = "-committerdate";

      # Automatically translate HTTPS to SSH when cloning repos
      url."ssh://git@github.com/".insteadOf = "https://github.com/";
    };
  };
}
