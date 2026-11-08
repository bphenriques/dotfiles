pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Wayland
import qs
import qs.services
import qs.ui

// Brief feedback when volume, the audio device, brightness, the keyboard layout or keep awake changes.
// A slim bar at the right edge, so it never resembles a notification.
Scope {
  id: root

  property bool shown: false
  // Held open while the pointer is over it.
  property bool hovered: false
  // What is or was last shown: "sink", "source", "brightness", "layout" or "idle"; kept while the window closes.
  property string kind: ""

  function show(newKind) {
    if (!state.armed) return;
    kind = newKind;
    shown = true;
    hideTimer.restart();
  }

  QtObject {
    id: state
    // Bound here, not read in show(): singletons start on first use, and this one must start with the shell.
    readonly property bool armed: Startup.settled
  }

  Timer {
    id: hideTimer
    interval: Config.osd.timeout
    onTriggered: if (root.hovered) restart(); else root.shown = false
  }

  Connections {
    target: Audio
    function onSinkVolumeChanged() { root.show("sink"); }
    function onSinkMutedChanged() { root.show("sink"); }
    function onSinkNameChanged() { root.show("sink"); }
    function onSourceVolumeChanged() { root.show("source"); }
    function onSourceMutedChanged() { root.show("source"); }
  }

  Connections {
    target: Brightness
    function onPercentageChanged() { root.show("brightness"); }
  }

  Connections {
    target: KeyboardLayout
    function onIndexChanged() { root.show("layout"); }
  }

  Connections {
    target: IdleInhibit
    function onEnabledChanged() { root.show("idle"); }
  }

  LazyLoader {
    active: root.shown

    OverlayWindow {
      id: window

      readonly property bool metered: root.kind === "sink" || root.kind === "source" || root.kind === "brightness"
      readonly property bool muted: root.kind === "sink" ? Audio.sinkMuted
        : root.kind === "source" ? Audio.sourceMuted
        : root.kind === "idle" ? !IdleInhibit.enabled
        : false
      readonly property string glyph: root.kind === "sink" ? (muted ? Glyphs.volumeOff : Glyphs.audioDevice(Audio.sinkType))
        : root.kind === "source" ? (muted ? Glyphs.microphoneOff : Glyphs.microphone)
        : root.kind === "brightness" ? Glyphs.brightness(Brightness.percentage)
        : root.kind === "idle" ? Glyphs.keepAwake
        : Glyphs.keyboard

      anchors.right: true
      margins.right: Config.theme.edgeMargin
      WlrLayershell.namespace: "osd"

      implicitWidth: metered ? bar.implicitWidth : pill.implicitWidth
      implicitHeight: metered ? bar.implicitHeight : pill.implicitHeight

      // A level: a vertical bar, the level above the icon.
      Surface {
        id: bar
        anchors.fill: parent
        visible: window.metered
        implicitWidth: Config.theme.osdIconSize + 2 * Config.theme.padding
        implicitHeight: levelColumn.implicitHeight + 2 * Config.theme.padding

        // The wheel adjusts the level shown, with the commands the tool's module set.
        WheelSteps {
          anchors.fill: parent
          hoverEnabled: true
          onContainsMouseChanged: root.hovered = containsMouse
          onStepped: up => {
            const commands = Config.osd.adjust[root.kind];
            if (commands) Quickshell.execDetached(["sh", "-c", commands[up ? 1 : 0]]);
          }
        }

        Column {
          id: levelColumn
          anchors.centerIn: parent
          spacing: Config.theme.padding

          Meter {
            anchors.horizontalCenter: parent.horizontalCenter
            orientation: Qt.Vertical
            height: Config.theme.osdLength
            tone: window.muted ? "dim" : "primary"
            value: root.kind === "sink" ? Audio.sinkVolume
              : root.kind === "source" ? Audio.sourceVolume
              : Brightness.percentage / 100
          }

          Icon {
            anchors.horizontalCenter: parent.horizontalCenter
            size: Config.theme.osdIconSize
            tone: window.muted ? "dim" : "normal"
            glyph: window.glyph
          }
        }
      }

      // A state without a level (layout, keep awake): a pill with the icon and what it is now.
      Pill {
        id: pill
        anchors.fill: parent
        visible: !window.metered
        padding: Config.theme.padding

        Icon {
          anchors.verticalCenter: parent.verticalCenter
          tone: window.muted ? "dim" : "normal"
          glyph: window.glyph
        }

        Label {
          anchors.verticalCenter: parent.verticalCenter
          tone: window.muted ? "dim" : "normal"
          text: root.kind === "layout" ? (KeyboardLayout.label || KeyboardLayout.name.slice(0, 2))
            : IdleInhibit.enabled ? "on" : "off"
        }
      }
    }
  }
}
