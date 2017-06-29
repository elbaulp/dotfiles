#!/bin/bash

latexmk -shell-escape -f -pdf -pvc $1 #; latexmk -c $1
