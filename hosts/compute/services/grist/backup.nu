# Exports all Grist documents as .grist (native SQLite) and .xlsx (portable) for backup.
# Recovery: Import .grist files via the Grist UI or API (POST /api/docs).
let headers = [Remote-Email $env.GRIST_ADMIN_EMAIL]
let output_dir = $env.OUTPUT_DIR

let workspaces = (http get --headers $headers $"($env.GRIST_URL)/api/orgs/current/workspaces")
let doc_ids = ($workspaces | get docs | flatten | get id)

for doc_id in $doc_ids {
  http get --headers $headers $"($env.GRIST_URL)/api/docs/($doc_id)/download"
    | save --force $"($output_dir)/($doc_id).grist"

  # Portable escape hatch
  http get --headers $headers $"($env.GRIST_URL)/api/docs/($doc_id)/download/xlsx"
    | save --force $"($output_dir)/($doc_id).xlsx"
}
