# `displayctl status` (the current layout, then those the connected screens allow) as quickkey's state line.
(split("\n") | map(select(. != ""))) as $lines
| ($lines[1:]) as $allowed
| ($layouts | map(select(.id == $lines[0])) | first // {}) as $current
| {
    glyph: ($current.icon // $fallback),
    text: ($current.label // ""),
    items: [$layouts[] | . + { active: (.id == $lines[0]), enabled: (.id as $id | $allowed | index($id) != null) }]
  }
