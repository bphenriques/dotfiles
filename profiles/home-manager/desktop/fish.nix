{ lib, pkgs, config, ... }:
let
  inherit (lib) concatStringsSep;
in
{
  programs.fish = {
    enable = true;
    plugins = [
      { name = "autopair"; inherit (pkgs.fishPlugins.autopair) src; }
      { name = "pure"; inherit (pkgs.fishPlugins.pure) src; }
      { name = "fish-async-prompt"; inherit (pkgs.fishPlugins.async-prompt) src; }
    ];

    functions = {
      # For me to remember: https://fishshell.com/docs/current/interactive.html#shared-bindings
      # Print the binding combo using `fish_key_reader`
      fish_user_key_bindings = concatStringsSep "\n" [
        ''bind alt-/ __frg-widget''                                   # Find text (fzf-rg)
        ''bind alt-. __ffd-widget''                                   # Find file (fzf-fd)
        ''bind alt-p __project-widget''                               # Switch project
        ''bind alt-space ${config.programs.yazi.shellWrapperName}''   # Browse files
      ];
    };

    interactiveShellInit = let
      purePrompt = ''
        set -g pure_enable_single_line_prompt true
        set -g pure_enable_virtualenv false
        set -g pure_color_success green
        set -g pure_shorten_window_title_current_directory_length 1
        set -g pure_enable_nixdevshell true
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
