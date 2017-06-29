#!/bin/bash

## Backup file system
## Params:
##   $1: Backup destination: eg: /media/drive

rsync -av --delete --exclude '/home/hkr/ssd2/usr' --exclude '/home/hkr/ssd2/portage' ~ /etc/portage "$1"
