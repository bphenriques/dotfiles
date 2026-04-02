{ config, pkgs, lib, self, ... }:
{
  imports = [
    ./hardware
    ./services
    ./bphenriques
  ];

  networking.hostName = "inky";

  # Inlined from profiles/nixos and profiles/nixos/headless.
  # Inky uses nixos-raspberrypi's pinned nixpkgs (for binary cache hits on the RPi kernel),
  # which may lag behind nixpkgs-unstable. Avoid importing shared profiles that assume
  # nixpkgs-unstable options.
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      use-xdg-base-directories = true;
      warn-dirty = false;
      auto-optimise-store = true;
    };
  };
  nixpkgs.config.allowUnfree = true;

  # Localization
  time.timeZone = "Europe/Lisbon";
  i18n = {
    defaultLocale = "en_GB.UTF-8";
    extraLocaleSettings = lib.genAttrs [
      "LC_ADDRESS" "LC_IDENTIFICATION" "LC_MEASUREMENT" "LC_MONETARY"
      "LC_NAME" "LC_NUMERIC" "LC_PAPER" "LC_TELEPHONE" "LC_TIME"
    ] (_: "pt_PT.UTF-8");
  };

  # Shell & programs
  programs.fish.enable = true;
  programs.command-not-found.enable = false;
  programs.nano.enable = false;

  # SSH
  networking.firewall.enable = true;
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      X11Forwarding = false;
      AllowAgentForwarding = false;
      AllowTcpForwarding = false;
      MaxAuthTries = 3;
      LoginGraceTime = "30s";
    };
  };

  # Headless: auto-reboot on failure
  boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];

  # Headless: allow remote deployment
  users.users.root.openssh.authorizedKeys.keys = self.shared.authorizedSSHKeys;

  # Headless: prevent suspend/hibernate
  systemd.sleep.extraConfig = ''
    AllowSuspend=no
    AllowHibernation=no
    AllowHybridSleep=no
    AllowSuspendThenHibernate=no
  '';

  # Headless: no DHCP (WiFi only on this host)
  networking.useDHCP = false;

  # Home Manager
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  documentation.nixos.enable = false;
  documentation.man.enable = false;
  documentation.doc.enable = false;
  security.sudo.extraConfig = "Defaults lecture=never";

  environment.systemPackages = [ pkgs.nvd ];

  # Homelab integration
  networking.hosts = lib.foldlAttrs (acc: name: ip: acc // { ${ip} = (acc.${ip} or []) ++ [ name ]; }) {} self.shared.networks.main.hosts;
  custom.homelab.smb = {
    enable = true;
    hostname = self.shared.networks.main.hosts.bruno-home-nas;
    credentialsPath = config.sops.templates."homelab-samba-credentials".path;
    mounts = {
      bphenriques = { gid = 5000; };
      media = { gid = 5001; };
    };
  };
  sops.secrets."homelab/samba/username" = { };
  sops.secrets."homelab/samba/password" = { };
  sops.templates."homelab-samba-credentials" = {
    owner = "root";
    group = "root";
    mode = "0400";
    content = ''
      username=${config.sops.placeholder."homelab/samba/username"}
      password=${config.sops.placeholder."homelab/samba/password"}
    '';
  };

  # Media paths (derived from SMB mount)
  custom.homelab.paths = {
    media.root = config.custom.homelab.smb.mounts.media.localMount;
  };

  # WiFi configuration - Pi Zero 2W uses wireless only
  sops.secrets."wifi/env" = { };
  networking.nameservers = [ self.shared.dns.cloudflare ];
  networking.wireless = {
    enable = true;
    interfaces = [ "wlan0" ];
    secretsFile = config.sops.secrets."wifi/env".path;
    networks = {
      "@WIFI_SSID@" = {
        pskRaw = "ext:WIFI_PASSWORD";
      };
    };
  };

  # Don't block boot waiting for ethernet (WiFi only)
  systemd.network.wait-online.anyInterface = true;

  # Memory optimization for Pi Zero 2W (512MB RAM)
  zramSwap = {
    enable = true;
    memoryPercent = 50;
    algorithm = "zstd";
  };

  # SD card wear reduction - volatile logs
  services.journald.extraConfig = ''
    Storage=volatile
    RuntimeMaxUse=16M
  '';

  # Disable rtkit (not needed without PipeWire/PulseAudio)
  security.rtkit.enable = false;

  # Power management - scale CPU frequency based on load
  powerManagement.cpuFreqGovernor = "ondemand";

  # Watchdog for automatic recovery from hangs
  systemd.settings.Manager = {
    RuntimeWatchdogSec = "30s";
    RebootWatchdogSec = "10min";
  };

  # Secrets
  sops.defaultSopsFile = self.private.hosts.inky.sopsSecretsFile;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11";
}
