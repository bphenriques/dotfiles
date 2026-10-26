{ lib, pkgs, fleetFacts, alfredVm, private, inputs, ... }:
{
  imports = [
    ./cookidoo-sync.nix
    ./hermes-agent.nix
    ./hermes-webui.nix
  ];

  _module.args.alfred = import ../../../../agents/alfred {
    inherit pkgs lib inputs;
    facts = {
      inherit (alfredVm) vaultRoot recipesRoot stateRoot;
      recipesUrl = fleetFacts.publicUrls.cook-recipes;
      cookidooCollection = private.cookidoo.collection;
    };
  };
}
