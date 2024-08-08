{ lib, pkgs, config, ... }:

# TODO: Explore keybindings: https://github.com/fish-shell/fish-shell/blob/master/share/functions/fish_default_key_bindings.fish
let
  inherit (builtins) readFile readDir attrNames;
  inherit (lib) filterAttrs foldl' optionalString concatStringsSep removeSuffix;
  listFiles = from: attrNames (filterAttrs (_ : type: type == "regular") (readDir from));
  listFishFiles = from: (builtins.filter (fileName: lib.hasSuffix ".fish" fileName) (listFiles from));
in
{
  home.packages = with pkgs; [ fish dotfiles ];

  programs.zoxide = {
    enable = true;
    options = [ "--cmd j" ];
    enableFishIntegration = true;
  };

  programs.direnv = {
    enable                  = true; # Automatically load .envrc or .env.
    nix-direnv.enable       = true; # Faster direnv for nix environments.
    silent                  = true; # Disable verbose messages when entering a directory.
  };

  programs.fish = {
    enable = true;

    plugins = [
      { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
      {
        name = "pure";
        src = pkgs.fetchFromGitHub {
          owner = "pure-fish";
          repo = "pure";
          rev = "v4.11.0";
          sha256 = "sha256-8zxqPU9N5XGbKc0b3bZYkQ3yH64qcbakMsHIpHZSne4=";
        };
      }
      {
        name = "fish-async-prompt";
        src = pkgs.fetchFromGitHub {
          owner = "acomagu";
          repo = "fish-async-prompt";
          rev = "v1.2.0";
          sha256 = "sha256-B7Ze0a5Zp+5JVsQUOv97mKHh5wiv3ejsDhJMrK7YOx4=";
        };
      }
      # Defined under overlays
      { name = "frg"; src = pkgs.fishPlugins.frg.src; }
      { name = "proj"; src = pkgs.fishPlugins.proj.src; }
      { name = "ffd"; src = pkgs.fishPlugins.ffd.src; }
      { name = "dotfiles"; src = pkgs.fishPlugins.dotfiles.src; }
    ];

    functions = (foldl' (acc: f: acc // {
      "${removeSuffix ".fish" f}" = (readFile (./functions + "/${f}"));
    }) { } (listFishFiles ./functions));

    interactiveShellInit = let
      nixIntegration = ''
        test -f "$HOME"/.nix-profile/etc/profile.d/nix.fish && source "$HOME"/.nix-profile/etc/profile.d/nix.fish
        test -f "$HOME"/.nix-profile/etc/profile.d/nix-daemon.fish && source "$HOME"/.nix-profile/etc/profile.d/nix-daemon.fish
        fish_add_path "/etc/profiles/per-user/$USER/bin"
      '';
      purePrompt = ''
        set -U pure_enable_single_line_prompt true
        set -U pure_enable_virtualenv false
        set -U pure_color_success green
        set -U pure_shorten_window_title_current_directory_length 1
        set -U pure_enable_nixdevshell true
        set -g async_prompt_functions _pure_prompt_git
      '';
      zellij = ''
        if test "$ZELLIJ_AUTO_START" = "true"
          eval (zellij setup --generate-auto-start fish | string collect)
        end
      '';
      extra = [
        ''set fish_greeting''
        (optionalString (pkgs.stdenv.system == "aarch64-darwin") ''eval "$(/opt/homebrew/bin/brew shellenv)"'')
        ''test -f "$XDG_CONFIG_HOME"/fish/local.fish && source "$XDG_CONFIG_HOME"/fish/local.fish''
      ];

    in concatStringsSep "\n" ([ nixIntegration purePrompt zellij ] ++ extra);
  };

  home.persistence."${config.custom.impermanence.cacheLocation}" = {
    directories = [ ".local/share/fish" ".local/share/zoxide" ];
    files = [ ".bash_history" ];
  };
}
