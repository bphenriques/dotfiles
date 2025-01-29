{ config, lib, pkgs, ... }:

# Start solaar as service: https://github.com/pwr-Solaar/Solaar/issues/2024
{
  config = lib.mkIf config.hardware.logitech.wireless.enableGraphical {
    systemd.user.services.solaar-graphical-user-interface = {
      enable = true;
      description = "Starts Solaar GUI";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      script = "${pkgs.solaar}/bin/solaar --restart-on-wake-up --window=hide";
    };
    environment.systemPackages = [ pkgs.at-spi2-core ]; # Fixes solaar start-up: https://gist.github.com/jeffcogswell/62395900725acef1c0a5a608f7eb7a05
  };
}
