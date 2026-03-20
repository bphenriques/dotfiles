# Copies Radicale CalDAV/CardDAV collections (.vcf/.ics files) for backup.
# Recovery: cp -a backup/radicale/. /var/lib/radicale/collections/
cp -a "$RADICALE_DATA/." "$OUTPUT_DIR/"
