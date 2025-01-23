{ lib, pkgs, config, self, ... }:
let
  inherit (lib) optionalString concatStringsSep;
in
{
  programs.fish = {
    enable = true;
    plugins = [
      { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
      { name = "pure"; src = pkgs.fishPlugins.pure.src; }
      { name = "fish-async-prompt"; src = pkgs.fishPlugins.async-prompt.src; }
    ];

    functions = {
      # For me to remember: https://fishshell.com/docs/current/interactive.html#shared-bindings
      # Print the binding combo using `fish_key_reader`
      fish_user_key_bindings = concatStringsSep "\n" [
        ''bind \e/ frg-widget''                                     # Alt + /     : Find text
        ''bind \e. ffd-widget''                                     # Alt + .     : Find file (cd or $EDITOR)
        ''bind \e\x20 ${config.programs.yazi.shellWrapperName}''    # Alt + SPC   : Browse files
        ''bind \ep p-widget''                                       # Alt + p     : Open project
      ];
    };

    interactiveShellInit = let
      nixDarwinIntegration = ''
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
      extra = ''
        set fish_greeting
        test -f "$XDG_CONFIG_HOME"/fish/local.fish && source "$XDG_CONFIG_HOME"/fish/local.fish
      '';
    in concatStringsSep "\n" [
      (optionalString pkgs.stdenv.isDarwin nixDarwinIntegration)
      purePrompt
      extra
    ];
  };
}
