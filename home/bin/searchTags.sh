#!/bin/bash

grep -ril " $1 " ./_posts | while read line 
do 
	grep -il "permalink" "$line" 
done
