{ config, lib, pkgs, ... }:

with lib;
with types;

#
# Inspired by Home-Manager's zsh module: https://github.com/nix-community/home-manager/blob/master/modules/programs/zsh.nix
#
# To learn and to have more control over what is happening.
# TODO: Normalize Paths
let
  cfg = config.modules.zsh;

  aliasesStr = concatStringsSep "\n" (
    mapAttrsToList (k: v: "alias ${k}=${lib.escapeShellArg v}") config.home.shellAliases # TODO: Why cfg.shellAliases?
  );

  pluginsDir = config.xdg.configHome + "/zsh/plugins";
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

      afterSource = mkOption {
        type = lines;
        default = "";
        description = "Additional configuration desired after sourcing";
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
      type = listOf path;
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

          # Load plugins
          (concatMapStrings (plugin:
              ''
              if [[ -f "${pluginsDir}/${plugin.name}/${plugin.file}" ]]; then
                . "${pluginsDir}/${plugin.name}/${plugin.file}"
                ${plugin.afterSource}
              fi
              ''
            ) cfg.plugins.list)

          cfg.initExtraAfterPlugins

          "typeset -aU path"    # Prune duplicate entries in $PATH

          ''
            # Load functions
            fpath=("$ZDOTDIR/functions" $fpath);
            autoload -Uz $fpath[1]/*(:t)
          ''

          # Load Aliases
          aliasesStr

          cfg.initExtraBeforeCompInit
          "autoload -Uz compinit && compinit"
          cfg.initExtraAfterCompInit

          (optionalString cfg.plugins.enableFastSyntaxHighlighting ''
            . "${pluginsDir}/${zshFastSyntaxHighlightingOptPlugin.name}/${zshFastSyntaxHighlightingOptPlugin.file}"
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
        (map (plugin: { "zsh/plugins/${plugin.name}".source = plugin.src; }) pluginsToMount);
    }

    # Mount auto-load functions
    {
      xdg.configFile =
        foldl' (a: b: a // b) {}
        (map (function: { "zsh/functions/${removeSuffix ".zsh" (baseNameOf function)}".source = function; }) cfg.functions);
    }
  ]);
}
