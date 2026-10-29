# nixpkgs marks posthog broken with mcp >= 2, but mcp is only a test input. Drop once upstream fixes it.
_final: prev: {
  pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
    (_pyFinal: pyPrev: {
      posthog = pyPrev.posthog.overridePythonAttrs (old: {
        doCheck = false;
        meta = old.meta // {
          broken = false;
        };
      });
    })
  ];
}
