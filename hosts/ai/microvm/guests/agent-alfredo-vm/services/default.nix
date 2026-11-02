{ config, pkgs, fleetFacts, alfredoVm, private, inputs, ... }:
let
  inherit (config.my.microvm.guest) gateway;
in
{
  imports = [
    ./cookidoo-sync.nix
    ./hermes-agent.nix
  ];

  _module.args.alfredo = inputs.agents-alfredo.lib.alfredo {
    inherit pkgs;
    facts = {
      inherit (alfredoVm) vaultRoot recipesRoot stateRoot;
      recipesUrl = fleetFacts.publicUrls.cook-recipes;
      speechUrl = "http://${gateway}:${toString config.fleet.ai.speechEndpoint.port}/inference";
      modelUrl = "http://${gateway}:${toString config.fleet.ai.endpoint.port}";
      inherit (config.fleet.ai) model;
      cookidooCollection = private.cookidoo.collection;
    };
  };
}
