import Quickshell
import Quickshell.Wayland

// Floats above everything, fullscreen windows included, without reserving screen space.
PanelWindow {
  exclusionMode: ExclusionMode.Ignore
  WlrLayershell.layer: WlrLayer.Overlay
  color: "transparent"
}
