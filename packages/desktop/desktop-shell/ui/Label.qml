import QtQuick
import qs

// Text in the theme font, coloured by `tone` (see Tone).
Text {
  property string tone: "normal"

  color: Tone.color(tone)
  font.family: Config.theme.monospaceFont
  font.pointSize: Config.theme.fontSize
  // Never rich text: stream titles can carry whole XML documents (Bauer Media radio does).
  textFormat: Text.PlainText
}
