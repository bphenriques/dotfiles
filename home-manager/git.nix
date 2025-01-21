{ config, lib, pkgs, ... }:
let
  commitTemplate = pkgs.writeText "git-message" ''
   # <type>(<scope>): Max. 50 characters    ------->
   # type: feat fix doc perf refactor style test infra ci chore
   #
   # Problem:
   #
   # Solution:
   #
   # Test:
   #
   # Body: Max. 72 characters per line                               ---->
   '';
in
{
  home.packages = with pkgs; [
    git-absorb
  ];

  programs.lazygit.enable = true;
  programs.git = {
    enable = true;
    userName = "Bruno Henriques";
    userEmail = "4727729+bphenriques" + "@" + "users.noreply.github.com"; # Minor obsfuscation to prevent webscrappers

    diff-so-fancy.enable = true;
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

      # Personal tools
      "bphenriques-tools/"
    ];


    extraConfig = {
      commit.template = "${commitTemplate}";

      tag = {
        gpgSign = true;
        sort = "version:refname";     # Natural order of commits
      };

      pull.rebase = false; # Not enabled by default as I usually collaborate with others on the same branch.
      push.autoSetupRemote = true;
      rebase = {
        autosquash = true;  # Automatic fixup! and/or squash!
        autoStash = true;   # Automatically stash my changes and apply after rebase finishes.
      };

      # Improve readability
      blame = {
        coloring = "repeatedLines";
        markIgnoredLines = true;
        markUnblamables = true;
      };

      branch.sort = "-committerdate"; # Sane sort

      # Cleanup
      fetch = {
        prune = true;
        pruneTags = true;
      };

      merge.conflictStyle = "zdiff3"; # Improve diff output
      rerere.enabled = true;          # Record how I solved some conflicts. Cache is under .git/rr-cache

      # Automatically translate HTTPS to SSH when cloning repos
      url."ssh://git@github.com/".insteadOf = "https://github.com/";
    };
  };
}
