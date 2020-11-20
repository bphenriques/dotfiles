{
  homebrew.enable = true;
  
  homebrew.taps = [ "homebrew/cask" ];
  homebrew.casks = [
    "transmission"        # Torrent Client.
    "dropbox"             # Cloud Storage.
  ];
}
