# $1 is the mounted card. Onion restores a bogus clock on boot unless this marker exists.
def main [card: string] {
  mkdir ([$card ".tmp_update"] | path join)
  touch ([$card ".tmp_update" ".noTimeRestore"] | path join)
  print "  rtc restore disabled"
}
