{ lib, ... }:
{
  options.custom.programs.terminal = {
    package = lib.mkOption {
      type = lib.types.package;
      description = "Terminal emulator package";
    };

    exec = lib.mkOption {
      type = lib.types.str;
      description = "Command to open a new terminal window";
    };

    execApp = lib.mkOption {
      type = lib.types.raw;
      description = "Function: { cmd, title? } -> command string to launch a command in a new terminal window";
    };
  };
}
