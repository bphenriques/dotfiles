{
  system.activationScripts.postUserActivation.text = ''
    echo "Copying org-protocol.app to /Applications"
    ln -sfn "${builtins.toString ./org-protocol.app}" /Applications/org-protocol.app
  '';
}