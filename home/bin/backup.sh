#!/bin/bash

## Backup file system
## Params:
##   $1: Backup destination: eg: /media/drive

# Delete cache / temp files before backing up

rm -rfv ~/.cache ~/.local/share/Trash ~/.thumbnails

rsync -av --delete --exclude '/home/hkr/ssd2/usr' --exclude '/home/hkr/ssd2/portage' ~ /etc/portage /var/lib/portage/world "$1"
