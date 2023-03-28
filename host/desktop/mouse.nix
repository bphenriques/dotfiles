{ config, pkgs, ... }:

# Who would have thought I would invest so many hours fixing a mouse setup?
let
  hostDir = "/home/${config.user.name}/.dotfiles/host/desktop";
in
{

  ## Installs solaar which gives control over the mouse's profile that I need to disable to ensure side-buttons work.
  hardware.logitech.wireless = {          # Compatibility with logitech devices.
    enable = true;
    enableGraphical = true;               # Installs Solaar. Ensure profile is off to allow key remapping.
  };

  # Terrible Hack as workaround to https://github.com/sezanzeb/input-remapper/issues/663
  # The files must be writable. Therefore, mkOutOfStoreSymlink allows me to create a file outside of the store. I.e., to the actual file in the repo.
  services.input-remapper.enable = true;
  home-manager.users.${config.user.name} = { config, ... }: { # ensure config is within home-manager's context
    xdg.configFile = {
      "input-remapper/config.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/config.json";
      "input-remapper/presets/Logitech G305/Media.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/Media.json";
    };
  };

  # One-time service addressing:
  # 1. Need to run solaar manually on each login to ensure that onboard profiles are properly disabled. See https://github.com/pwr-Solaar/Solaar/issues/2024
  # 2. Input-remapper does not apply the profile automatically. See https://github.com/sezanzeb/input-remapper/issues/653
  #
  # This one-time service, calls ensures that everything is setup on each login (debug using `systemctl --user status mouse-g305-v4.service`):
  environment.systemPackages = with pkgs; [ at-spi2-core ]; # Required for solaar: https://gist.github.com/jeffcogswell/62395900725acef1c0a5a608f7eb7a05
  systemd.user.services.mouse-g305-v5 = {
    enable = true;
    description = "Configures G305 mouse";
    wantedBy = ["graphical-session.target"];
    after = ["input-remapper.service"]; # Do I need "dbus.service"  ?

    # https://git.spritsail.io/frebib/dotfiles/src/branch/master/systemd/user/solaar.service
    #  ${pkgs.solaar}/bin/solaar config DC210D31 onboard_profiles Disable
    # solaar --restart-on-wake-up --window=hide
    script = ''
      ${pkgs.input-remapper}/bin/input-remapper-control --command stop-all && ${pkgs.input-remapper}/bin/input-remapper-control --command autoload
      ${pkgs.solaar}/bin/solaar --restart-on-wake-up --window=hide
    '';
  };
}
