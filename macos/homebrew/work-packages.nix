{
  homebrew.enable = true;
  
  homebrew.taps = [ "homebrew/cask" ];
  homebrew.brews = [
    "helm" 
    "python@3.9"
    "terraform"
  ];
  homebrew.casks = [
    "google-cloud-sdk"
    "slack"             # Communication. Oh god.. you can be annoying.
  ];
}
