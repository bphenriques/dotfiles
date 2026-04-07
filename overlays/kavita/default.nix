_final: prev: let
  version = "0.8.9.1";
  src = prev.fetchFromGitHub {
    owner = "kareadita";
    repo = "kavita";
    rev = "v${version}";
    hash = "sha256-pQuHnhHlctWhh3ZV5Qvi8vBVegwO57GYpwLI3ZReWws=";
  };
  frontend = prev.buildNpmPackage {
    pname = "kavita-frontend";
    inherit version src;
    nodejs = prev.nodejs_22;
    sourceRoot = "${src.name}/UI/Web";
    npmBuildScript = "prod";
    npmFlags = [ "--legacy-peer-deps" ];
    npmRebuildFlags = [ "--ignore-scripts" ];
    npmDepsHash = "sha256-YCCls05i16EmNEEWVs58BIwjbmUnahlhuR23hkfyWks=";
  };
  backend = prev.buildDotnetModule {
    pname = "kavita-backend";
    inherit version src;
    patches = [ ./change-webroot.diff ];
    postPatch = ''
      substituteInPlace API/Services/DirectoryService.cs --subst-var out
      substituteInPlace API/Startup.cs API/Services/LocalizationService.cs API/Controllers/FallbackController.cs \
        --subst-var-by webroot "${frontend}/lib/node_modules/kavita-webui/dist/browser"
    '';
    executables = [ "API" ];
    projectFile = "API/API.csproj";
    nugetDeps = ./nuget-deps.json;
    dotnet-sdk = prev.dotnetCorePackages.sdk_10_0;
    dotnet-runtime = prev.dotnetCorePackages.aspnetcore_10_0;
  };
in {
  kavita = prev.kavita.overrideAttrs (_prevAttrs: {
    inherit version src backend frontend;
  });
}
