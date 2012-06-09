#!/bin/bash

original="/usr/share/festival/dicts/upc/upcdict_catalan-1.0.out"
new="final/upcdict_catalan.out"

cat $original | cut -d " " -f 1 > comp_orig.txt
cat $new | cut -d " " -f 1 > comp_new.txt

diff -U 0 "comp_orig.txt" "comp_new.txt" > test.diff

rm comp_orig.txt comp_new.txt
