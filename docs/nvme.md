Found out that new NVME might need fine-tuning:
1. List the nvmes available: `sudo nvme list`
2. Then, list the blocks of the device: ``lsblk -t /dev/nvme0n1`
3. You check if it supports 
# in the official docs.512 (physical/logical): `lsblk -t /dev/nvme0n1`
# But.. it can support 4096/4096: `sudo nvme id-ns -H /dev/nvme031` (difference between "in-use" an in the official docs.512/512 is for compatibility and we can increase it: `sudo nvme format --lbaf=1 /dev/nvme0n1`.
# - Where lbaf corresponds to the number next to "LBA Format"
#
# Now let's check again which LBA Format is in-use: `sudo nvme id-ns -H /dev/nvme0n1`
#
# Source
- https://wiki.archlinux.o in the official docs.ormat
- https://www.high-availability.com/docs/ZFS-Tuning-Guide/#alignment-shift-ashiftn
