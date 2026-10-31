{ lib, pkgs, fleet, fleetFacts, alfredVm, private, inputs, ... }:
{
  imports = [
    ./cookidoo-sync.nix
    ./hermes-agent.nix
  ];

  _module.args.alfred = import ../../../../agents/alfred {
    inherit pkgs lib inputs;
    facts = {
      inherit (alfredVm) vaultRoot recipesRoot stateRoot;
      recipesUrl = fleetFacts.publicUrls.cook-recipes;
      speechUrl = with fleet.ai.speechEndpoint; "http://${host}:${toString port}/inference";
      modelUrl = with fleet.ai.endpoint; "http://${host}:${toString port}";
      inherit (fleet.ai) model;
      cookidooCollection = private.cookidoo.collection;
    };
  };
}
