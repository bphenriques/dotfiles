pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.Mpris

// Local mpd as published over MPRIS by mpdris2. Playback position is left out: MPRIS only updates it when polled.
Singleton {
  readonly property var player: Mpris.players.values.find(player => player.identity === "Music Player Daemon") ?? null
  readonly property bool present: player !== null
  readonly property bool playing: player?.isPlaying ?? false
  readonly property string title: player?.trackTitle ?? ""
  readonly property string artist: player?.trackArtist ?? ""
  readonly property bool repeat: (player?.loopState ?? MprisLoopState.None) !== MprisLoopState.None
  readonly property bool shuffle: player?.shuffle ?? false
}
