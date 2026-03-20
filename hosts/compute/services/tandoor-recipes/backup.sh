# Exports Tandoor recipes database and media files for backup.
#
# Recovery:
#   1. systemctl stop tandoor-recipes
#   2. pg_restore --clean --if-exists --no-owner -d tandoor_recipes backup/tandoor/database.dump
#   3. cp -a backup/tandoor/media/. /var/lib/tandoor-recipes/media/
#   4. systemctl start tandoor-recipes
runuser -u "$DB_USER" -- pg_dump \
  --format=custom \
  --no-owner \
  --host=/run/postgresql \
  "$DB_NAME" > "$OUTPUT_DIR/database.dump"

test -s "$OUTPUT_DIR/database.dump" || { echo "ERROR: database dump is empty"; exit 1; }

if [ -d "$MEDIA_DIR" ]; then
  cp -a "$MEDIA_DIR/." "$OUTPUT_DIR/media/"
fi
