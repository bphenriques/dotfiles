# Backs up Actual Budget data: SQLite database (consistent snapshot) + user budget files.
# Recovery: restore both server-files/account.sqlite and user-files/ to the data directory.
mkdir -p "$OUTPUT_DIR/server-files" "$OUTPUT_DIR/user-files"

# Consistent SQLite snapshot
DB="$SERVER_FILES_DIR/account.sqlite"
if [ -f "$DB" ]; then
  sqlite3 "$DB" <<EOF
.timeout 5000
.backup '$OUTPUT_DIR/server-files/account.sqlite'
EOF
else
  echo "Actual Budget DB not found at $DB; skipping (fresh install?)" >&2
fi

# User budget files (binary blobs)
if [ -d "$USER_FILES_DIR" ] && [ -n "$(ls -A "$USER_FILES_DIR")" ]; then
  cp -a "$USER_FILES_DIR/." "$OUTPUT_DIR/user-files/"
fi
