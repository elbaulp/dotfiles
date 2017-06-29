#!/bin/bash

## Extract files with extension $1 in $2

find -iname "*.$1" -print0 | xargs -0 mv -t "$2"
