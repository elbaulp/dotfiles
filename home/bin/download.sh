#!/bin/bash

## Download an image for the blog
## Params:
##   $1: Name of the image to save in blog
##   $2: Url of image to download
##   $3: Resize image to given size, keeping aspect ratio

wget -O /home/hkr/Desarrollo/algui91-hugo/src/img/$1 $2

if [ "$#" -eq 3 ]
then
    convert -resize $3x /home/hkr/Desarrollo/algui91-hugo/src/img/$1 /home/hkr/Desarrollo/algui91-hugo/src/img/$1
    identify -format "width=\"%[fx:w]\" height=\"%[fx:h]\"\n" /home/hkr/Desarrollo/algui91-hugo/src/img/$1
fi
