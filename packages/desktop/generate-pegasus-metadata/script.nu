def main [config_file: path, output_dir: path] {
  mkdir $output_dir

  let config = open $config_file
  let roms_dir = $config.romsDir
  let artwork_mapping = $config.artworkMapping

  for system in $config.systems {
    let roms_path = [$roms_dir $system.dir] | path join
    let out = [$output_dir $"($system.dir).metadata.pegasus.txt"] | path join

    if not ($roms_path | path exists) { continue }

    let exts = $system.extensions | each { str downcase }
    let roms = (glob --no-dir $"($roms_path)/*"
      | where { ($in | path parse | get extension | str downcase) in $exts }
      | sort
    )

    let header = [
      $"collection: ($system.name)"
      $"shortname: ($system.shortname)"
      $"launch: ($system.launch)"
      ""
    ]

    let game_lines = ($roms | each { |rom|
      let basename = $rom | path parse | get stem

      let assets = ($artwork_mapping | each { |mapping|
        let asset_file = [$roms_path "media" $mapping.folder $"($basename).png"] | path join
        if ($asset_file | path exists) {
          $"assets.($mapping.asset): ($asset_file)"
        }
      } | compact)

      [$"game: ($basename)" $"file: ($rom)"] | append $assets | append ""
    } | flatten)

    $header | append $game_lines | str join "\n" | save --force $out
  }

  print $"Pegasus metadata generated in ($output_dir)"
}
