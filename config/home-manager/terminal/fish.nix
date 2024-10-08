{ lib, pkgs, config, self, ... }:
let
  inherit (lib) optionalString concatStringsSep;
in
{
  home.packages = with pkgs; [ fish ];

  home.shellAliases = {
    nixsh = "nix-shell --run ${lib.getExe pkgs.fish}";
    devsh = "nix develop --command ${lib.getExe pkgs.fish}";
  };

  programs.fish = {
    enable = true;

    plugins = [
      { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
      { name = "pure"; src = pkgs.fishPlugins.pure.src; }
      { name = "fish-async-prompt"; src = pkgs.fishPlugins.async-prompt.src; }
      { name = "frg"; src = self.pkgs.fishPlugins.frg.src; }
      { name = "ffd"; src = self.pkgs.fishPlugins.ffd.src; }
    ];

    functions = {
      nix-search = ''nix-env -qaP | grep -i $argv[1]'';
      nix-run = ''nix run nixpkgs#$argv[1] --impure -- $argv[2..-1]'';
    };

    interactiveShellInit = let
      # Am I missing? https://codeberg.org/adamcstephens/dotfiles/src/branch/main/apps/fish/init.fish#L3
      nixDarwinIntegration = ''
        test -f "$HOME"/.nix-profile/etc/profile.d/nix.fish && source "$HOME"/.nix-profile/etc/profile.d/nix.fish
        test -f "$HOME"/.nix-profile/etc/profile.d/nix-daemon.fish && source "$HOME"/.nix-profile/etc/profile.d/nix-daemon.fish
        fish_add_path "/etc/profiles/per-user/$USER/bin"
      '';
      darwinHomebrew = ''eval "$(/opt/homebrew/bin/brew shellenv)"'';
      purePrompt = ''
        set -U pure_enable_single_line_prompt true
        set -U pure_enable_virtualenv false
        set -U pure_color_success green
        set -U pure_shorten_window_title_current_directory_length 1
        set -U pure_enable_nixdevshell true
        set -g async_prompt_functions _pure_prompt_git
      '';
      extra = ''
        set fish_greeting
        test -f "$XDG_CONFIG_HOME"/fish/local.fish && source "$XDG_CONFIG_HOME"/fish/local.fish
      '';
    in concatStringsSep "\n" [
      (optionalString (pkgs.stdenv.system == "aarch64-darwin") nixDarwinIntegration)
      purePrompt
      (optionalString (pkgs.stdenv.system == "aarch64-darwin") darwinHomebrew)
      extra
    ];
  };
}
