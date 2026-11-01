{ lib, ... }:
{
  options.my.programs.terminal = {
    package = lib.mkOption {
      type = lib.types.package;
      description = "Terminal emulator package";
    };

    exec = lib.mkOption {
      type = lib.types.str;
      description = "Command to open a new terminal window";
    };

    execApp = lib.mkOption {
      type = lib.types.functionTo lib.types.str;
      description = "Function { cmd, title? } -> command that runs cmd in a new terminal";
    };
  };
}
