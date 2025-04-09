{
  nix = {
    optimise.automatic = true;
    settings = {
      experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
      use-xdg-base-directories = true;                    # Hide ~/.nix-profile and ~/.nix-defexpr
      warn-dirty = false;                                 # I know...
    };
  };
  nixpkgs.config = {
    allowUnfree = true; # I was maintaining a list.. because it was _nicer_ and _explicit_ but.. I am lazy.
    permittedInsecurePackages = [ ];
  };
}