#!/bin/sh

echo '[objects]' > temp
echo $1.obj >> temp

wla-65816 -o $1.obj $1.asm
wlalink -v -r temp $1.smc 

rm $1.obj
rm temp

