#!/bin/bash

pasmo -d gif-reader.asm 0x6a00.bin > test.asm
size_prog=`stat --format=%s ./0x6a00.bin`
printf "program: ${size_prog} bytes\n"

prog="27136"    # $6A00
max="64800"     # under stack
adr=$((${prog}+${size_prog}))
data=""
i="0"

for size in `stat --format=%s ./Gifs/*.gif`
do
    if test "${data}" = "" 
    then
        data="${adr}"
    else
        data="${data},${adr}"
    fi
    i=$((${i}+1))
    adr=$((${adr}+${size}))
done

printf "10 DATA ${data}\n" > gif-reader.bas
printf "20 FOR i=1 TO ${i}:READ a:randomize usr ${prog},a:PAUSE0:NEXT i:STOP\n" >> gif-reader.bas
zmakebas gif-reader.bas -o gif-reader.tap

cat ./Gifs/*.gif >> 0x6a00.bin

test "${adr}" -gt "${max}" && printf "Prilis mnoho obrazku! Usetri $((${start}-${max})) bajtu...\n"
