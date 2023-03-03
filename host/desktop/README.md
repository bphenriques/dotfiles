Setting up the internal hardrive:

Assuming already formatted (otherwise have fun remembering the commands :P):
```
$ sudo e2label /dev/sda1 "files"
```

This ensures that there is a `/dev/disk/by-label/files` available that I can point NixOS to. It will handle creating the
mounting directory for me.

Hopefully this suffices. Let's see next time I get around testing this from scratch.
