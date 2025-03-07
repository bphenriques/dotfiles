_: {
  qt.enable = true;
  stylix.targets.qt.enable = true;
  custom.programs.niri.environment = {
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    QT_QPA_PLATFORM = "wayland";
  };
}