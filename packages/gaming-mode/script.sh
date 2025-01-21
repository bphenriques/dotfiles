
get_resolution() {
  echo ""
}

get_framerate_limit() {

}

prefer_output() {

}

supports_adaptive_sync() {}

supports_hdr() {}


# TODO https://github.com/Misterio77/nix-config/blob/main/home/gabriel/features/games/steam.nix
case ${1:-} in
  start)
     gamescope \
        "--output-width ${toString monitor.width}"
        "--output-height ${toString monitor.height}"
        "--framerate-limit ${toString monitor.refreshRate}"
        "--prefer-output ${monitor.name}"
        "--adaptive-sync"
        "--expose-wayland"
        "--hdr-enabled"
        "--steam"
      ];

      notify-send
    ;;
  end)

    ;;
esac