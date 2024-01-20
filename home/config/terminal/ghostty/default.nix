{ pkgs, ... }:
{
    home.packages = with pkgs; [ ghostty ];
    # theme = Doom One
    # TOOD: Mount the config file.
}

