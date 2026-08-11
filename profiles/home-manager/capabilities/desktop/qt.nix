_: {
  qt.enable = true;
  stylix.targets.qt = {
    enable = true;
    standardDialogs = "xdgdesktopportal"; # Same file chooser as the GTK apps
  };
  wayland.windowManager.niri.settings.environment = {
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    QT_QPA_PLATFORM = "wayland";
  };
}