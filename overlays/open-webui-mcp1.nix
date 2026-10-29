# open-webui 0.12 pins mcp 1.x and imports `streamablehttp_client`, which mcp 2 removed. Drop once
# nixpkgs' open-webui moves to mcp 2.
_final: prev: {
  open-webui = prev.open-webui.overridePythonAttrs (old: {
    dependencies = map (p: if (p.pname or "") == "mcp" then prev.python3Packages.mcp_1 else p) old.dependencies;
  });
}
