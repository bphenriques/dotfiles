{ config, lib, pkgs, ... }:

with lib;
with types;

# Alternative (and inspired by) Home-Manager's zsh module: https://github.com/nix-community/home-manager/blob/master/modules/programs/zsh.nix
# Reasons: learn and to have more control over what is happening.
let
  cfg = config.modules.zsh;

  functionsDir  = "functions";
  widgetsDir    = "widgets";
  pluginsDir    = "plugins";

  pluginsFullDir = config.xdg.configHome + "/zsh/${pluginsDir}";
  pluginModule = submodule {
    options = {
      name = mkOption { type = str; };
      src = mkOption { type = path; };
      file = mkOption { type = str; };

      sourceTiming = mkOption {
        type = enum [ "before-compinit" "after-compinit" "last" ];
        default = "before-compinit";
      };

      sourceExtra = mkOption {
        type = lines;
        default = "";
      };
    };
  };

  functionsModule = submodule {
    options = {
       name = mkOption { type = str; };
       text = mkOption { type = lines; };
     };
  };

  widgetsModule = submodule {
    options = {
       name = mkOption { type = str; };
       text = mkOption { type = lines; };
       keybinding = mkOption { type = str; };
     };
  };

  sourcePlugins = plugins:
    (concatMapStringsSep "\n" (plugin:
      let
        path = "${pluginsFullDir}/${plugin.name}/${plugin.file}";
      in
        ''
        . "${path}" || echo "Failed to load ${plugin.name}!"
        '' + (optionalString (plugin.sourceExtra != "") plugin.sourceExtra)
    ) plugins);
in
{
  options.modules.zsh = {
    enable = mkEnableOption "Z shell (Zsh)";

    envExtra = mkOption {
      type = lines;
      default = "";
    };

    aliases = mkOption {
      type = attrsOf str;
      default = {};
    };

    completions = mkOption {
      type = lines;
      default = "";
    };

    functions = mkOption {
      type = listOf (either path functionsModule);
      default = [];
    };

    widgets = mkOption {
      type = listOf widgetsModule;
      default = [];
    };

    plugins = mkOption {
      type = listOf pluginModule;
      default = [];
    };

    options = mkOption {
      type = lines;
      default = "";
    };

    initExtraBeforeCompinit = mkOption {
      type =  lines;
      default = "";
    };

    initExtraAfterCompInit = mkOption {
      type = lines;
      default = "";
    };

    debugStartup = mkEnableOption "Z Shell Debug Startup";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [
        {
          assertion = !config.programs.zsh.enable;
          message = "Home-Manager's zsh module is enabled. This custom module is not compatible with both setups at the same time.";
        }
      ];
    }

    # Setup ZSHENV
    {
      home.file.".zshenv".text = ''
        # This file is absent in NixOS but present on other regular setups.
        test -f "$HOME"/.nix-profile/etc/profile.d/nix.sh && . "$HOME"/.nix-profile/etc/profile.d/nix.sh

        # Add home-manager binaries to PATH
        export PATH="/etc/profiles/per-user/$USER/bin:$PATH"

        # Source home-manager session variables
        . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"

        # Override ZSH location to unclutter $HOME folder
        export ZDOTDIR="$XDG_CONFIG_HOME"/zsh

        ${cfg.envExtra}
      '';
    }

    # Aliases
    {
      xdg.configFile."zsh/aliases.zsh".text =
        concatStringsSep "\n" (mapAttrsToList (k: v: "alias ${k}=${escapeShellArg v}") config.home.shellAliases);
    }

    # Completions
    {
      xdg.configFile."zsh/completion.zsh".text = concatStringsSep "\n" [
        cfg.completions
        ''
        autoload -Uz compinit
        for dump in ~/.zcompdump(N.mh+24); do
          compinit
        done
        compinit -C
        ''
      ];
    }

    # Widgets and keybindings
    {
      xdg.configFile =
        foldl' (a: b: a // b) {}
        (map (widget: { "zsh/${widgetsDir}/${widget.name}".text = widget.text; } ) cfg.widgets);
    }
    {
      xdg.configFile."zsh/keybindings.zsh".text = concatStringsSep "\n" [
        ''
          fpath=("$ZDOTDIR/${widgetsDir}" $fpath);
          autoload -Uz $fpath[1]/*(:t)
        ''
        (concatMapStrings (widget:
          ''
          zle -N ${widget.name}
          bindkey '${widget.keybinding}' ${widget.name}
          ''
        ) cfg.widgets)
      ];
    }

    # Setup ZSHRC
    {
      home.packages = with pkgs; [zsh];
      xdg.configFile."zsh/.zshrc".text = concatStringsSep "\n" [
        # "zmodload zsh/zprof"
        cfg.options

        # Prune duplicate entries in $PATH
        "typeset -aU path"

        # Autoload functions and widgets
        ''
          fpath=("$ZDOTDIR/${functionsDir}" "$ZDOTDIR/${widgetsDir}" $fpath);
          autoload -Uz $fpath[1]/*(:t) $fpath[2]/*(:t)
        ''

        (sourcePlugins (filter (plugin: plugin.sourceTiming == "before-compinit") cfg.plugins))
        cfg.initExtraBeforeCompinit
        ''. "$ZDOTDIR"/completion.zsh''
        cfg.initExtraAfterCompInit
        (sourcePlugins (filter (plugin: plugin.sourceTiming == "after-compinit") cfg.plugins))

        ''. "$ZDOTDIR"/keybindings.zsh''
        ''. "$ZDOTDIR"/aliases.zsh''

        (sourcePlugins (filter (plugin: plugin.sourceTiming == "last") cfg.plugins))
        # "zprof"
      ];
    }

    # Mount Plugins: Ensure plugins are symlinked outside /nix/store folder
    # See:
    # - https://github.com/nix-community/home-manager/pull/56#issuecomment-328057513
    # - https://github.com/nix-community/home-manager/commit/cff9ee7cce1bd40f63beef4b4f3044c29a5a41cb
    {
      xdg.configFile =
        foldl' (a: b: a // b) {}
        (map (plugin: { "zsh/${pluginsDir}/${plugin.name}".source = plugin.src; }) cfg.plugins);
    }

    # Mount auto-load functions
    {
      xdg.configFile =
        foldl' (a: b: a // b) {}
        (map (function:
          if builtins.isPath function then
            { "zsh/${functionsDir}/${removeSuffix ".zsh" (baseNameOf function)}".source = function; }
          else if builtins.isString function then
            { "zsh/${functionsDir}/${function.name}".text = function.text; }
          else
            throw "invalid type of function"
        ) cfg.functions);
    }
  ]);
}
