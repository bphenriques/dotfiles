_:
{
  programs.gamemode = {
    enable = true;
    enableRenice = true;  # Ensure niceness is lower to increased priority
    settings = {
      general.inhibit_screensaver = 1; # Inhibit idle/screensaver while a game is running
    };
  };
}


