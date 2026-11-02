{ config, lib, pkgs, fleetFacts, alfredVm, private, inputs, ... }:
let
  inherit (config.my.microvm.guest) gateway;
in
{
  imports = [
    ./cookidoo-sync.nix
    ./hermes-agent.nix
  ];

  _module.args.alfred = import ../../../../../../agents/alfred {
    inherit pkgs lib inputs;
    facts = {
      inherit (alfredVm) vaultRoot recipesRoot stateRoot;
      recipesUrl = fleetFacts.publicUrls.cook-recipes;
      speechUrl = "http://${gateway}:${toString config.fleet.ai.speechEndpoint.port}/inference";
      modelUrl = "http://${gateway}:${toString config.fleet.ai.endpoint.port}";
      inherit (config.fleet.ai) model;
      cookidooCollection = private.cookidoo.collection;
    };
  };
}
