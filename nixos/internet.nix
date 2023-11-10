{ pkgs, lib, config, ... }:
{
  services = {
    qbittorrent.enable = true;

    syncthing =
      let
        inherit (builtins) elem foldl';
        inherit (lib) optional concatMapAttrs;

        # Each device is invited to sync any of the folders. They can choose to ignore.
        romFolders = ["dos" "dreamcast" "fbneo" "gb" "gbc" "gba" "megadrive" "n64" "nds" "nes" "pico8" "ps2" "ps3" "psp" "psx" "snes" "switch"];
        devices = {
          phone = {
            name = "Bruno's Phone";
            id = "TMPMB7L-ZXHQMWN-I2O3GQI-2E24O5M-ANMBRJV-OREQNRQ-KPKUS2Y-HHHAKAP";
          };
          steamDeck = {
            name = "Bruno's Steam Deck";
            id = "EUEKEKQ-ZC4ZO3V-QRIX5QA-YVUD7OZ-WJY5XKB-TW65VJO-SZN7WSM-I5XOMAG";
          };
        };
      in
      {
        enable = true;
        overrideDevices = true;     # overrides any devices added or deleted through the WebUI
        overrideFolders = true;     # overrides any folders added or deleted through the WebUI
        settings = {
          devices = concatMapAttrs (name: device: { "${device.name}" = { id = device.id; }; } ) devices;
          folders = {
            "Music" = {
              path = config.user.musicDir;
              devices = [ devices.phone.name ];
              type = "sendonly";
            };
            "Shared" = {
              path = config.user.shareDir;
              devices = [ devices.phone.name devices.steamDeck.name ];
              type = "sendonly";
            };
          } // foldl' (acc: romFolder: acc // {
            "${romFolder}" = {
              path = config.user.romsDir + "/" + romFolder;
              devices = [ devices.phone.name devices.steamDeck.name ];
              type = "sendonly";
            };
          }) { } romFolders;
        };
      };
  };
}
