{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  baseTaskModule = { name, ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Task identifier (defaults to attribute name)";
      };

      systemdServices = lib.mkOption {
        type = lib.types.coercedTo lib.types.str (s: [ s ]) (lib.types.listOf lib.types.str);
        default = [ ];
        description = "Systemd service name(s) managed by this task. Used by integrations to inject behavior.";
      };
    };
  };
in
{
  options.custom.homelab = {
    _taskOptionExtensions = lib.mkOption {
      type = lib.types.listOf lib.types.deferredModule;
      default = [ ];
      internal = true;
    };

    tasks = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submoduleWith {
        modules = [ baseTaskModule ] ++ cfg._taskOptionExtensions;
      });
      default = { };
      description = ''
        Registry of non-HTTP tasks: backup, timers, maintenance jobs.
        Integrations extend this via internal extension hooks.
      '';
    };
  };
}
