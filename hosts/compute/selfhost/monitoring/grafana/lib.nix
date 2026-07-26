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

  # Single-value tile; renders one cell per returned series (used for the fleet strip).
  mkStat =
    {
      id,
      title,
      expr,
      gridPos,
      legend ? "{{instance}}",
      unit ? "short",
      thresholds ? null,
      mappings ? null,
      colorMode ? "value",
    }:
    {
      inherit id title gridPos datasource;
      type = "stat";
      fieldConfig.defaults =
        { inherit unit; color.mode = "thresholds"; }
        // (if thresholds != null then { inherit thresholds; } else { })
        // (if mappings != null then { inherit mappings; } else { });
      options = {
        reduceOptions = { calcs = [ "lastNotNull" ]; fields = ""; values = false; };
        inherit colorMode;
        graphMode = "none";
        textMode = "value_and_name";
        justifyMode = "auto";
      };
      targets = [{ inherit datasource; refId = "A"; inherit expr; legendFormat = legend; }];
    };

  mkRow =
    { id, title, gridPos, collapsed ? false, panels ? [ ] }:
    {
      inherit id title gridPos collapsed panels;
      type = "row";
    };

  # Assign gridPos to a list of mkPanel arg sets, flowing two-per-row from y0.
  layout2 = y0: specs:
    builtins.genList
      (i:
        let s = builtins.elemAt specs i;
        in s // { gridPos = { x = (i - 2 * (builtins.div i 2)) * w; y = y0 + (builtins.div i 2) * h; inherit w h; }; })
      (builtins.length specs);
}
