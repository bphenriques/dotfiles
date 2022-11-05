{ config, lib, pkgs, ... }:

with lib;
with types;

# Alternative (and inspired by) Home-Manager's zsh module: https://github.com/nix-community/home-manager/blob/master/modules/programs/zsh.nix
#
# Reasons: learn and to have more control over what is happening.
let
  cfg = config.modules.zsh;

  functionsDir  = "functions";
  widgetsDir    = "widgets";
  pluginsDir    = "plugins";

  aliasesStr = concatStringsSep "\n" (
    mapAttrsToList (k: v: "alias ${k}=${lib.escapeShellArg v}") config.home.shellAliases # TODO: Why cfg.shellAliases?
  );

  pluginsFullDir = config.xdg.configHome + "/zsh/${pluginsDir}";
  pluginModule = submodule {
    options = {
      name = mkOption {
        type = str;
        description = ''
          The name of the plugin.
          Don't forget to add <option>file</option>
          if the script name does not follow convention.
        '';
      };

      src = mkOption {
        type = path;
        description = ''
          Path to the plugin folder.
          Will be added to <envar>fpath</envar> and <envar>PATH</envar>.
        '';
      };

      file = mkOption {
        type = str;
        description = "The plugin script to source.";
      };

      sourceTiming = mkOption {
        type = enum [ "before-compinit" "after-compinit" ];
        default = "before-compinit";
        description = "When to source the plugin";
      };

      sourceExtra = mkOption {
        type = lines;
        default = "";
        description = "Additional configuration desired after sourcing";
      };
    };
  };

  sourcePlugins = plugins:
    (concatMapStrings (plugin:
       ''
       if [[ -f "${pluginsFullDir}/${plugin.name}/${plugin.file}" ]]; then
         . "${pluginsFullDir}/${plugin.name}/${plugin.file}"
         ${plugin.sourceExtra}
       fi
       ''
      ) plugins);

  functionsModule = submodule {
    options = {
       name = mkOption {
         type = str;
         description = "The name of the function";
       };

       text = mkOption {
         type = lines;
         description = "The function's source code";
       };
     };
  };

  widgetsModule = submodule {
    options = {
       name = mkOption {
         type = str;
         description = "The name of the widget function";
       };

       text = mkOption {
         type = lines;
         description = "The widgets's source code";
       };

       keybinding = mkOption {
         type = str;
         description = "The keybinding";
       };
     };
  };

  zshFastSyntaxHighlightingOptPlugin = {
    name = "zsh-fast-syntax-highlighting";
    src = pkgs.zsh-fast-syntax-highlighting;
    file = "share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh";
  };
in
{

  options.modules.zsh = {
    enable = mkEnableOption "Z shell (Zsh)";

    aliases = mkOption {
      type = attrsOf str;
      default = {};
    };

    envExtra = mkOption {
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

    plugins = {
      enableFastSyntaxHighlighting = mkEnableOption "fast-syntax-highlighting";
      list = mkOption {
        type = listOf pluginModule;
        default = [];
      };
    };

    initExtraFirst = mkOption {
      type = lines;
      default = "";
    };

    initExtraBeforePlugins = mkOption {
      type =  lines;
      default = "";
    };

    initExtraAfterPlugins = mkOption {
      type =  lines;
      default = "";
    };

    initExtraBeforeCompInit = mkOption {
      type = lines;
      default = "";
    };

    initExtraAfterCompInit = mkOption {
      type = lines;
      default = "";
    };
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
        # Source nix & Home-Manager packages
        test -f "$HOME"/.nix-profile/etc/profile.d/nix.sh && . "$HOME"/.nix-profile/etc/profile.d/nix.sh
        export PATH="/etc/profiles/per-user/$USER/bin:$PATH"

        # Source session variables
        test -f "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh" && . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"

        # Override ZSH location to unclutter $HOME folder
        export ZDOTDIR="$XDG_CONFIG_HOME"/zsh

        ${cfg.envExtra}
      '';
    }

    # Setup ZSHRC
    {
      home.packages = with pkgs; [zsh]
        ++ optional cfg.plugins.enableFastSyntaxHighlighting zsh-fast-syntax-highlighting;

      xdg.configFile = {
        "zsh/.zshrc".text = lib.concatStringsSep "\n" [
          cfg.initExtraFirst

          cfg.initExtraBeforePlugins

          # Load plugins Before compinit
          (sourcePlugins (filter (plugin: plugin.sourceTiming == "before-compinit") cfg.plugins.list))

          cfg.initExtraAfterPlugins

          # Prune duplicate entries in $PATH
          "typeset -aU path"

          # Autoload functions and widgets
          ''
            fpath=("$ZDOTDIR/${functionsDir}" "$ZDOTDIR/${widgetsDir}" $fpath);
            autoload -Uz $fpath[1]/*(:t) $fpath[2]/*(:t)
          ''

          aliasesStr      # Register Aliases


          # Register widgets and keybindings
          (concatMapStrings (widget:
              ''
              zle -N ${widget.name}
              bindkey '${widget.keybinding}' ${widget.name}
              ''
            ) cfg.widgets)


          cfg.initExtraBeforeCompInit
          "autoload -Uz compinit && compinit"
          cfg.initExtraAfterCompInit

          (sourcePlugins (filter (plugin: plugin.sourceTiming == "after-compinit") cfg.plugins.list))

          (optionalString cfg.plugins.enableFastSyntaxHighlighting ''
            . "${pluginsFullDir}/${zshFastSyntaxHighlightingOptPlugin.name}/${zshFastSyntaxHighlightingOptPlugin.file}"
          '')
        ];
      };
    }

    # Setup Plugins: Ensure plugins are symlinked outside /nix/store folder
    # See:
    # - https://github.com/nix-community/home-manager/pull/56#issuecomment-328057513
    # - https://github.com/nix-community/home-manager/commit/cff9ee7cce1bd40f63beef4b4f3044c29a5a41cb
    {
      xdg.configFile = let
        pluginsToMount = cfg.plugins.list ++ optional cfg.plugins.enableFastSyntaxHighlighting zshFastSyntaxHighlightingOptPlugin;
      in
        foldl' (a: b: a // b) {}
        (map (plugin: { "zsh/${pluginsDir}/${plugin.name}".source = plugin.src; }) pluginsToMount);
    }

    # Mount auto-load functions
    {
      xdg.configFile =
        foldl' (a: b: a // b) {}
        (map (function:
          if builtins.isPath function then
            { "zsh/${functionsDir}/${removeSuffix ".zsh" (baseNameOf function)}".source = function; }
          else
            { "zsh/${functionsDir}/${function.name}".text = function.text; }
        ) (cfg.functions));
    }

    # Mount auto-load widgets
    {
      xdg.configFile =
        foldl' (a: b: a // b) {}
        (map (widget:
          { "zsh/${widgetsDir}/${widget.name}".text = widget.text; }
        ) (cfg.widgets));
    }
  ]);
}
