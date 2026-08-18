# fastapi-pagination 0.15.16 always calls fastapi's private `_get_body_field`, which only exists from
# 0.140.5, so RomM dies at import against nixpkgs' fastapi 0.139.0. Scoped to RomM's interpreter to keep
# immich-machine-learning on the nixpkgs version. Drop once nixpkgs ships fastapi >= 0.140.5.
_final: prev: {
  romm = prev.romm.override {
    python3 = prev.python3.override {
      packageOverrides = _: pyPrev: {
        fastapi = pyPrev.fastapi.overrideAttrs (finalAttrs: _: {
          version = "0.141.1";
          src = prev.fetchFromGitHub {
            owner = "tiangolo";
            repo = "fastapi";
            tag = finalAttrs.version;
            hash = "sha256-5P9aDMS7gLti2CBlrucvjgl4Od1mti9ityPdqxI1RIM=";
          };
        });
      };
    };
  };
}
