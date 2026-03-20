# Exports OpenCloud data directory while the service is stopped for consistency.
# decomposedfs is database-like (metadata in xattrs/sidecars + blobs); live copies risk corruption.
#
# Recovery:
#   1. systemctl stop opencloud
#   2. rsync -aHAX --numeric-ids backup/opencloud/ /var/lib/opencloud/
#   3. systemctl start opencloud
#   4. Optionally verify: opencloud backup consistency -p /var/lib/opencloud/storage/users
systemctl stop opencloud.service
trap 'systemctl start opencloud.service' EXIT

rsync -aHAX --numeric-ids "$DATA_DIR/" "$OUTPUT_DIR/"

test -d "$OUTPUT_DIR/storage" || { echo "ERROR: backup appears incomplete (no storage dir)"; exit 1; }
