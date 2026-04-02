{ pkgs, ... }:
{
  # InkyPi (https://github.com/fatihak/InkyPi) is installed imperatively due to mutable state (plugins, config)

  environment.systemPackages = with pkgs; [
    # Python environment
    python3
    python3Packages.pip
    python3Packages.virtualenv

    # Image processing dependencies for Inky Impression display
    libtiff
    openjpeg
    freetype
    libjpeg

    # Build tools for Python packages (only needed during setup)
    gcc
    gnumake

    # Git for cloning InkyPi
    git
  ];

  # Dedicated user for InkyPi with SPI/GPIO access
  users.users.inkypi = {
    isSystemUser = true;
    group = "inkypi";
    extraGroups = [ "spi" "gpio" ];
    home = "/opt/inkypi";
  };
  users.groups.inkypi = {};

  # Systemd service wrapper for InkyPi
  # Activates only after manual installation at /opt/inkypi
  systemd.services.inkypi = {
    description = "InkyPi Display Service";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];

    # Only start if InkyPi is installed
    unitConfig = {
      ConditionPathExists = "/opt/inkypi/server/server.py";
    };

    serviceConfig = {
      Type = "simple";
      WorkingDirectory = "/opt/inkypi";
      ExecStart = "/opt/inkypi/.venv/bin/python server/server.py";
      Restart = "on-failure";
      RestartSec = "10s";
      User = "inkypi";
      Group = "inkypi";
      SupplementaryGroups = [ "spi" "gpio" ];
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      ReadWritePaths = [ "/opt/inkypi" ];
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictRealtime = true;
      DeviceAllow = [ "/dev/spidev* rw" "/dev/gpiochip* rw" ];
    };
  };

  # Firewall: InkyPi web interface
  networking.firewall.allowedTCPPorts = [ 5000 ];
}
