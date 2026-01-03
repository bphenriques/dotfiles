_: {
  # user/group: pocket-id
  services.pocket-id = {
    enable = true;
    # environmentFile = ""; to store secrets
    settings = {
      ANALYTICS_DISABLED = true;
      APP_URL = "https://pocket-id.${HOME_SERVER_BASE_URL}";
      TRUST_PROXY = true;
      # ENCRYPTION_KEY_FILE =
    };

    # port 1411
  };
}