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
        ''bind alt-/ frg-widget''                                     # Find text
        ''bind alt-. ffd-widget''                                     # Find file (cd or $EDITOR)
        ''bind alt-space ${config.programs.yazi.shellWrapperName}''   # Browse files
      ];
    };

    interactiveShellInit = let
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
      purePrompt
      extra
    ];
  };
}
