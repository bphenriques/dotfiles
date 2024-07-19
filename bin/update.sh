#!/usr/bin/env sh
set -ef

# Relevant docs: https://nix.dev/manual/nix/2.18/installation/upgrading#upgrading-nix

nix flake update
case "$(uname -s)" in
  Darwin)
    brew upgrade && brew update
    sudo -i sh -c 'nix-channel --update && nix-env --install --attr nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'
    ;;
  *)
    if [ ! -d /etc/nixos ]; then
      nix-channel --update; nix-env --install --attr nixpkgs.nix nixpkgs.cacert; systemctl daemon-reload; systemctl restart nix-daemon
    fi
    ;;
esac
