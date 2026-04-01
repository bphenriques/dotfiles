# Exports all Grist documents as .grist (native) and .xlsx (portable) for backup.
# Recovery: Import .grist files via the Grist UI or API (POST /api/docs).
let base_url = $env.GRIST_URL
let headers = [Remote-Email $env.GRIST_ADMIN_EMAIL]
let output_dir = $env.OUTPUT_DIR

let workspaces = (http get --headers $headers $"($base_url)/api/orgs/current/workspaces")
let doc_ids = ($workspaces | get docs | flatten | get id)

for doc_id in $doc_ids {
  http get --headers $headers $"($base_url)/api/docs/($doc_id)/download"
    | save --force $"($output_dir)/($doc_id).grist"

  http get --headers $headers $"($base_url)/api/docs/($doc_id)/download/xlsx"
    | save --force $"($output_dir)/($doc_id).xlsx"
}
