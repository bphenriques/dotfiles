_final: prev: {
  cook-cli = prev.cook-cli.overrideAttrs (old: rec {
    version = "0.29.1";

    src = prev.fetchFromGitHub {
      owner = "cooklang";
      repo = "cookcli";
      rev = "v${version}";
      hash = "sha256-fg8qq4j9NbQvnduPRBwqp+GyQaHx2axqH39KeMZqy2k=";
    };

    nativeBuildInputs = old.nativeBuildInputs ++ [ prev.esbuild ];

    preBuild = ''
      npm run build-css
      npm run build-js
    '';

    cargoDeps = prev.rustPlatform.fetchCargoVendor {
      inherit src;
      hash = "sha256-eU/iOb5gHEjWdALeVQr2K3JkD0qOwco3Vkm05HWKdIs=";
    };

    npmDeps = prev.fetchNpmDeps {
      inherit src;
      hash = "sha256-tBOBa2plgJ0dG5eDD9Yc9YS+Dh6rhBdqU6JiZUjTUY4=";
    };
  });
}
