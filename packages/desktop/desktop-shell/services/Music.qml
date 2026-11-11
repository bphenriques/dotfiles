pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.Mpris
import qs

// The player Config.music.player names over MPRIS, else the one playing, else any. Playback position is left out: MPRIS only updates it when polled.
Singleton {
  readonly property bool present: internal.player !== null
  readonly property bool playing: internal.player?.isPlaying ?? false
  readonly property string title: internal.xml ? internal.field("DB_SONG_NAME") : internal.title
  readonly property string artist: internal.xml ? internal.field("DB_LEAD_ARTIST_NAME") : internal.track?.trackArtist ?? ""
  readonly property bool repeat: (internal.player?.loopState ?? MprisLoopState.None) !== MprisLoopState.None
  readonly property bool shuffle: internal.player?.shuffle ?? false

  QtObject {
    id: internal

    readonly property var players: Mpris.players.values
    readonly property var player: Config.music.player
      ? players.find(player => player.identity === Config.music.player) ?? null
      : players.find(player => player.isPlaying) ?? players[0] ?? null
    // A stopped player keeps its last track.
    readonly property var track: player?.playbackState === MprisPlaybackState.Stopped ? null : player
    readonly property string title: track?.trackTitle ?? ""
    // Bauer Media radio sends an XML document as the stream title; its song and artist are inside.
    readonly property bool xml: title.startsWith("<?xml")

    function field(name) {
      return title.match(new RegExp("<" + name + ">([^<]*)</" + name + ">"))?.[1]?.trim() ?? "";
    }
  }
}
