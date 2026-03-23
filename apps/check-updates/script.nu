let metadata = open $env.METADATA_FILE

let headers = if "GITHUB_TOKEN" in $env {
  { Authorization: $"Bearer ($env.GITHUB_TOKEN)" }
} else {
  {}
}

print "Checking pinned external packages for updates...\n"

let results = $metadata | each { |pkg|
  let latest_tag = try {
    http get --headers $headers $"https://api.github.com/repos/($pkg.repo)/releases/latest" | get tag_name
  } catch {
    print $"  ⚠ ($pkg.name): failed to query ($pkg.repo)"
    return null
  }

  let latest = if ($pkg.stripPrefix | is-not-empty) and ($latest_tag | str starts-with $pkg.stripPrefix) {
    $latest_tag | str substring ($pkg.stripPrefix | str length)..
  } else {
    $latest_tag
  }

  if $pkg.version == $latest {
    print $"  ✓ ($pkg.name) ($pkg.version) \(up to date\)"
    { outdated: false }
  } else {
    print $"  ✗ ($pkg.name) ($pkg.version) → ($latest) \(($pkg.repo)\)"
    { outdated: true }
  }
} | compact

print ""
if ($results | any { $in.outdated }) {
  print "Update versions and hashes in overlays/default.nix."
  exit 1
} else {
  print "All pinned packages are up to date."
}
