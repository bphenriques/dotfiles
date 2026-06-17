let headers = if "GITHUB_TOKEN" in $env {
  {Authorization: $"Bearer ($env.GITHUB_TOKEN)"}
} else {
  {}
}
def query-latest [pkg: record]: nothing -> record {
  let latest_tag = try {
    http get --headers $headers $"https://api.github.com/repos/($pkg.repo)/releases/latest" | get tag_name
  } catch {
    return {
      name: $pkg.name
      version: $pkg.version
      latest: null
      outdated: false
      error: true
    }
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
def check-group [entries: list<any>, label: string]: nothing -> list<any> {
  let results = $entries | each {|e| query-latest $e }
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
def update-containers [results: list<any>]: nothing -> nothing {
  let outdated = $results | where {|r| $r.outdated and (not $r.error)}
  if ($outdated | is-empty) {
    print "No container images to update."
    return
  }
  let file = $"($env.PWD)/overlays/containers.nix"
  if not ($file | path exists) {
    print $"Cannot update: ($file) not found. Run from the dotfiles repo root."
    return
  }
  mut text = open --raw $file
  for r in $outdated {
    let pattern = '(?s)(' + $r.name + ' = \{.*?version = ")[^"]*(")'
    $text = ($text | str replace --regex $pattern ('${1}' + $r.latest + '${2}'))
    print $"  ($r.name): ($r.version) → ($r.latest)"
  }
  $text | save --force --raw $file
  print $"Updated ($outdated | length) container image\(s\) in overlays/containers.nix."
}
def main [--update] {
  print "Checking for updates...\n"
  let pkg_results = check-group (open $env.PACKAGES_FILE) "Pinned packages (overlays/default.nix)"
  print ""
  let container_results = check-group (open $env.CONTAINERS_FILE) "Container images (overlays/default.nix)"
  print ""
  if $update {
    update-containers $container_results
    return
  }
  let all_results = $pkg_results | append $container_results
  if ($all_results | any { $in.outdated }) {
    print "Some pinned versions are outdated. Re-run with --update to bump container images."
    exit 1
  } else {
    print "All pinned versions are up to date."
  }
}
