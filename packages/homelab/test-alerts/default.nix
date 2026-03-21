{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "test-alerts";
  runtimeInputs = with pkgs; [ curl ];
  text = ''
    ALERTMANAGER_URL="''${1:-http://localhost:9093}"

    echo "Sending test alert to $ALERTMANAGER_URL..."
    curl -s -X POST "$ALERTMANAGER_URL/api/v2/alerts" \
      -H 'Content-Type: application/json' \
      -d "[{
        \"labels\": {\"alertname\": \"TestAlert\", \"severity\": \"warning\", \"instance\": \"test\"},
        \"annotations\": {\"summary\": \"Test notification — please ignore\"},
        \"startsAt\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\",
        \"endsAt\": \"$(date -u -d '+5 minutes' +%Y-%m-%dT%H:%M:%SZ)\"
      }]"

    echo ""
    echo "Verifying alert was received..."
    curl -s "$ALERTMANAGER_URL/api/v2/alerts" | head -c 500
    echo ""
  '';
  meta.platforms = lib.platforms.linux;
}
