let gh_headers = if "GITHUB_TOKEN" in $env {
  {Authorization: $"Bearer ($env.GITHUB_TOKEN)"}
} else {
  {}
}

const MANIFEST_TYPES = "application/vnd.oci.image.index.v1+json,application/vnd.docker.distribution.manifest.list.v2+json,application/vnd.docker.distribution.manifest.v2+json"

# `into semver` rejects ElegantFin's `26.09.05`, so versions are compared naturally.
def version-newer [a: string, b: string]: nothing -> bool {
  ($a != $b) and (([$a $b] | sort --natural | last) == $a)
}

# Newest first. `/releases/latest` is repo-wide, so on a monorepo it answers with whichever
# package published last; match on the tag prefix instead, then keep only plain versions so
# rc and diagnostic tags that share the prefix drop out.
def upstream-versions [entry: record]: nothing -> list<string> {
  let versions = http get --headers $gh_headers $"https://api.github.com/repos/($entry.repo)/releases?per_page=100"
  | where draft == false and prerelease == false
  | get tag_name
  | where { |tag| $tag | str starts-with $entry.stripPrefix }
  | each { |tag| $tag | str substring ($entry.stripPrefix | str length).. }
  | where { |version| $version =~ '^[0-9][0-9.]*$' }
  | sort --natural --reverse

  if ($versions | is-empty) {
    # Upstream renamed its tags: reporting "up to date" off an empty list would be a lie.
    error make {msg: $"no ($entry.repo) release tag matches '($entry.stripPrefix)'"}
  }
  $versions
}

def registry [image: string]: nothing -> record {
  let parts = $image | split row "/"
  let host = $parts | first
  let path = $parts | skip 1 | str join "/"
  if $host == "docker.io" {
    {api: "https://registry-1.docker.io", auth: "https://auth.docker.io/token", service: "registry.docker.io", path: $path}
  } else {
    {api: $"https://($host)", auth: $"https://($host)/token", service: $host, path: $path}
  }
}

# Both registries 401 an anonymous manifest read but hand out a pull token for the asking.
def pull-token [reg: record]: nothing -> string {
  http get $"($reg.auth)?service=($reg.service)&scope=repository:($reg.path):pull" | get token
}

def tag-exists [reg: record, token: string, tag: string]: nothing -> bool {
  let headers = {Authorization: $"Bearer ($token)", Accept: $MANIFEST_TYPES}
  let response = http get --full --allow-errors --headers $headers $"($reg.api)/v2/($reg.path)/manifests/($tag)"
  match $response.status {
    200 => true
    404 => false
    # Throttling or an outage reads like a missing tag, so refuse to guess.
    _ => { error make {msg: $"($reg.path):($tag) answered HTTP ($response.status)"} }
  }
}

def check-package [entry: record]: nothing -> record {
  let latest = upstream-versions $entry | first
  {name: $entry.name, version: $entry.version, latest: $latest, outdated: ($entry.version != $latest), note: null, error: false}
}

def unpublished-note [versions: list<string>]: nothing -> any {
  if ($versions | is-empty) { null } else { $"($versions | str join ', ') released, no image tag yet" }
}

# Upstream cuts releases the registry has not caught up with, and a tag that cannot be pulled is
# not an update: walk the releases above the pin newest-first and stop at the first tag that exists.
def check-container [entry: record]: nothing -> record {
  let current = {name: $entry.name, version: $entry.version, latest: $entry.version, outdated: false, note: null, error: false}
  let newer = upstream-versions $entry | where { |version| version-newer $version $entry.version }
  if ($newer | is-empty) { return $current }

  let reg = registry $entry.image
  let token = pull-token $reg
  mut unpublished = []
  for version in $newer {
    if (tag-exists $reg $token $"($entry.tagPrefix)($version)($entry.tagSuffix)") {
      return ($current | merge {latest: $version, outdated: true, note: (unpublished-note $unpublished)})
    }
    $unpublished = ($unpublished | append $version)
  }
  $current | merge {note: (unpublished-note $unpublished)}
}

def check-entry [entry: record, check: closure]: nothing -> record {
  try {
    do $check $entry
  } catch { |err|
    {name: $entry.name, version: $entry.version, latest: null, outdated: false, note: ($err.msg | lines | first), error: true}
  }
}

def check-group [entries: list<any>, label: string, check: closure]: nothing -> list<any> {
  let results = $entries | each {|e| check-entry $e $check }
  let max_name = $results | get name | str length | math max
  let max_ver = $results | get version | str length | math max
  print $"($label):"
  for r in $results {
    let padded_name = $r.name | fill -c ' ' -w $max_name
    let padded_ver = $r.version | fill -c ' ' -w $max_ver
    let status = if $r.error {
      "⚠ failed to query"
    } else if $r.outdated {
      $"✗ → ($r.latest)"
    } else {
      "✓ up to date"
    }
    let note = if ($r.note | is-empty) { "" } else { $"  \(($r.note))" }
    print $"  ($padded_name)  ($padded_ver)  ($status)($note)"
  }
  $results
}

def main [] {
  print "Checking for updates...\n"
  let pkg_results = check-group (open $env.PACKAGES_FILE) "Pinned packages (overlays/jellyfin.nix)" {|e| check-package $e }
  print ""
  let container_results = check-group (open $env.CONTAINERS_FILE) "Container images (overlays/containers.nix)" {|e| check-container $e }
  print ""
  let results = $pkg_results | append $container_results
  if ($results | any {|r| $r.outdated }) {
    print "Some pinned versions are outdated. Update the versions in overlays/."
    exit 1
  } else if ($results | any {|r| $r.error }) {
    print "Could not reach upstream for every pin, so this run is incomplete."
    exit 1
  } else {
    print "All pinned versions are up to date."
  }
}
