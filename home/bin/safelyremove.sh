#!/bin/bash

# Safely unmount a usb drive

udisksctl unmount -b /dev/$1
sleep 5
udisksctl power-off -b /dev/$2
