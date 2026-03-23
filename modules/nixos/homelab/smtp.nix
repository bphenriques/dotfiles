{ lib, ... }:
{
  options.custom.homelab.smtp = {
    host = lib.mkOption {
      type = lib.types.str;
      description = "SMTP server hostname";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 587;
      description = "SMTP server port";
    };

    from = lib.mkOption {
      type = lib.types.str;
      description = "Sender email address";
    };

    user = lib.mkOption {
      type = lib.types.str;
      description = "SMTP authentication username";
    };

    tls = lib.mkOption {
      type = lib.types.enum [ "none" "starttls" "tls" ];
      default = "starttls";
      description = "TLS mode for SMTP connection";
    };

    passwordFile = lib.mkOption {
      type = lib.types.str;
      description = "Path to file containing the SMTP password (typically a sops secret path)";
    };
  };
}
