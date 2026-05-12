{ config, lib, pkgs, ... }:

let
  cfg = config.custom.programs.goose;
  yaml = pkgs.formats.yaml { };

  enabledServers = lib.filterAttrs (_: s: s.enable) cfg.servers;

  gooseConfig = {
    extensions = lib.mapAttrs (_: s: {
      name = s.name;
      type = "stdio";
      enabled = true;
      cmd = s.cmd;
      args = s.args;
      timeout = s.timeout;
    } // lib.optionalAttrs (s.envs != { }) {
      envs = s.envs;
    }) enabledServers;
  };

  serverOpts = { name, ... }: {
    options = {
      enable = lib.mkEnableOption "this MCP server";

      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Human-readable name shown in Goose.";
      };

      cmd = lib.mkOption {
        type = lib.types.str;
        description = "Command to start the MCP server.";
      };

      args = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Arguments passed to the command.";
      };

      timeout = lib.mkOption {
        type = lib.types.int;
        default = 300;
        description = "Timeout in seconds for tool calls.";
      };

      envs = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
        description = "Environment variables passed to the server.";
      };
    };
  };
in {
  options.custom.programs.goose = {
    enable = lib.mkEnableOption "Goose CLI with declarative MCP servers";

    servers = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule serverOpts);
      default = { };
      description = "MCP servers to register as Goose extensions.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.goose-cli ];

    xdg.configFile."goose/config.yaml".source =
      yaml.generate "goose-config.yaml" gooseConfig;
  };
}
