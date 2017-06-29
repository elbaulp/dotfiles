#!/bin/bash

cd $1

for image in *.jpg *.png;
do
    res=$(identify -format %wx%h\\n "$image");
    rename "s/^/[$res] - /" "$image"
done


