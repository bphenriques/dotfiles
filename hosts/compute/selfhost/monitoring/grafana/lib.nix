# Grafana dashboard-as-code helpers, shared across dashboards.
rec {
  datasource = {
    type = "prometheus";
    uid = "prometheus";
  };

  h = 8;
  w = 12;
  fullW = 24;

  mkPanel =
    {
      id,
      title,
      expr,
      unit ? "short",
      type ? "timeseries",
      gridPos,
      legendMode ? "list",
      thresholds ? null,
      ...
    }@args:
    {
      inherit id title type gridPos datasource;
      fieldConfig.defaults = {
        inherit unit;
        custom = {
          lineWidth = 1;
          fillOpacity = 10;
          spanNulls = true;
        };
      } // (if thresholds != null then { inherit thresholds; } else { });
      options = {
        tooltip.mode = "multi";
        legend = {
          displayMode = legendMode;
          placement = "bottom";
        };
      };
      targets =
        if builtins.isList expr then
          builtins.genList (i: {
            inherit datasource;
            refId = builtins.elemAt [ "A" "B" "C" "D" "E" "F" "G" "H" ] i;
            inherit ((builtins.elemAt expr i)) expr;
            legendFormat = (builtins.elemAt expr i).legend;
          }) (builtins.length expr)
        else
          [{ inherit datasource; refId = "A"; inherit expr; legendFormat = args.legend or ""; }];
    };

  mkRow =
    { id, title, gridPos }:
    {
      inherit id title gridPos;
      type = "row";
      collapsed = false;
    };
}
