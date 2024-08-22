{ config, ... }: {

  custom.impermanence = {
    enable = true;
    dataLocation = "/persist/data/bphenriques";
    cacheLocation = "/persist/cache/bphenriques";

    # Enable for those who don't have a programs.<prog>.enable
    heroic = true;
    steam = true;
    gog = true;
    nvidia = true;
    protontricks = true;
    vlc = true;
    qbittorrent = true;
    discord = true;
    bitwarden = true;
    jetbrains = true;
    g4music = true;
    mesa = true;
    wine = true;
    winetricks = true;
    solaar = true;
    filezilla = true;

    # Scala
    scalacli = true;
    metals = true;
    coursier = true;
  };

  home.persistence."${config.custom.impermanence.dataLocation}".directories = [
    ".dotfiles"
    "Downloads"
    "Pictures"
    "Videos"
  ];
}
