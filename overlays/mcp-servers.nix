# nixpkgs moved `mcp` to 2.x, which the reference fetch and time servers do not support yet. Drop once they are ported.
_final: prev: let
  withMcp1 = pkg: pkg.overridePythonAttrs (old: {
    dependencies = map (d: if (d.pname or "") == "mcp" then prev.python3Packages.mcp_1 else d) old.dependencies;
  });
in {
  mcp-server-fetch = withMcp1 prev.mcp-server-fetch;
  mcp-server-time = withMcp1 prev.mcp-server-time;
}
