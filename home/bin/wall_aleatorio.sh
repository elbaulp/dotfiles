#!/bin/bash

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
export DISPLAY=:0.0

picsfolder="/home/hkr/HD/"
cd $picsfolder

files=(*.{jpg,png})
N=${#files[@]}


((N=$(od -vAn -N4 -tu4 < /dev/urandom)%N))

randomfile1=${files[$N]}

logger "$0 - Setting $randomfile1 as wallpaper, random number » $N"

feh --bg-max "$picsfolder$randomfile1"
