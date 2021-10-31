{
  homebrew.brews = [
    "pngpaste"      # org-download dependency in MacOS.
  ];

  # Setups
  # - Emacs GUI app has to be copied manually (https://github.com/d12frosted/homebrew-emacs-plus/issues/41).
  # - Org-Protocol
  system.activationScripts.postUserActivation.text = ''
    echo "Copying org-protocol.app to /Applications"
    ln -sfn "${builtins.toString ./org-protocol.app}" /Applications/org-protocol.app
  '';
}
