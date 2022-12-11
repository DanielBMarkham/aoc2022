#!/bin/bash

mkdir day$1
touch day$1/example.txt
touch day$1/problem1.txt
touch day$1/problem2.txt 
cp ./scratch-template.fsx day$1/scratch.fsx

sed -i 's/dayX/day'$1'/g' day$1/scratch.fsx

