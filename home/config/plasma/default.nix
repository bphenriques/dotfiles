{ pkgs, ... }:

# https://github.com/pjones/plasma-manager/blob/trunk/examples/home.nix
{
  programs.plasma = {
    enable = true;
    workspace = {
      theme = "breeze-dark";
      colorScheme = "BreezeDark";
      cursorTheme = "Breeze";
      clickItemTo = "select";
      lookAndFeel = "org.kde.breezedark.desktop";
      #wallpaper = ;
      #wallpaperSlideShow = {
        #path =
        #interval =
      #};
    };

    windows.allowWindowsToRememberPositions = false;

    panels = [
      {
        location = "bottom";
        hiding = "autohide";
        widgets = [
          "org.kde.plasma.kickoff"
          {
            name = "org.kde.plasma.icontasks";
            config = {
              General.launchers = [
                "applications:org.kde.dolphin.desktop"
              ];
            };
          }
          "org.kde.plasma.marginsseparator"
          {
            systemTray.items = {
              # We explicitly show bluetooth and battery
              shown = [
                "org.kde.plasma.battery"
                "org.kde.plasma.bluetooth"
              ];
              # And explicitly hide networkmanagement and volume
              hidden = [
                "org.kde.plasma.networkmanagement"
                "org.kde.plasma.volume"
              ];
            };
          }
          {
            digitalClock = {
              calendar.firstDayOfWeek = "monday";
              time.format = "24h";
            };
          }
        ];
      }
    ];
  };
}
