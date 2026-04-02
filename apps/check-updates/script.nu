let headers = if "GITHUB_TOKEN" in $env {
  { Authorization: $"Bearer ($env.GITHUB_TOKEN)" }
} else {
  {}
}

def query-latest [pkg: record]: nothing -> record {
  let latest_tag = try {
    http get --headers $headers $"https://api.github.com/repos/($pkg.repo)/releases/latest" | get tag_name
  } catch {
    return { name: $pkg.name, version: $pkg.version, latest: null, outdated: false, error: true }
  }

  let latest = if ($pkg.stripPrefix | is-not-empty) and ($latest_tag | str starts-with $pkg.stripPrefix) {
    $latest_tag | str substring ($pkg.stripPrefix | str length)..
  } else {
    $latest_tag
  }

  {
    name: $pkg.name
    version: $pkg.version
    latest: $latest
    outdated: ($pkg.version != $latest)
    error: false
  }
}

def check-group [entries: list, label: string]: nothing -> list {
  let results = $entries | each { |e| query-latest $e }

  let max_name = $results | get name | str length | math max
  let max_ver = $results | get version | str length | math max

  print $"($label):"
  for r in $results {
    let padded_name = $r.name | fill -c ' ' -w $max_name
    let padded_ver = $r.version | fill -c ' ' -w $max_ver
    if $r.error {
      print $"  ($padded_name)  ($padded_ver)  ⚠ failed to query"
    } else if $r.outdated {
      print $"  ($padded_name)  ($padded_ver)  ✗ → ($r.latest)"
    } else {
      print $"  ($padded_name)  ($padded_ver)  ✓ up to date"
    }
  }

  $results
}

print "Checking for updates...\n"

let pkg_results = check-group (open $env.PACKAGES_FILE) "Pinned packages (overlays/default.nix)"

print ""
let container_results = check-group (open $env.CONTAINERS_FILE) "Container images (overlays/default.nix)"

let all_results = ($pkg_results | append $container_results)
print ""
if ($all_results | any { $in.outdated }) {
  print "Some pinned versions are outdated."
  exit 1
} else {
  print "All pinned versions are up to date."
}
