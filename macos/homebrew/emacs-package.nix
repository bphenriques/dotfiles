{
  homebrew.enable = true;
  
  homebrew.taps = [ "d12frosted/emacs-plus" ];
  homebrew.brews = [
    "emacs-plus"    # Emacs itself.
    "pngpaste"      # org-download dependency in MacOS.
  ];

  # Setups
  # - Emacs GUI app has it has to be copied manually (https://github.com/d12frosted/homebrew-emacs-plus/issues/41).
  # - Org-Protocol
  system.activationScripts.postUserActivation.text = ''
    echo "Copying emacs-plus/Emacs.app to /Applications"
    ln -sfn /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app

    echo "Copying org-protocol.app to /Applications"
    ln -sfn "${builtins.toString ./org-protocol.app}" /Applications/org-protocol.app
  '';
}
