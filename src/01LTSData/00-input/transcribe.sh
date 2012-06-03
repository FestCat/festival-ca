#!/bin/sh

if [ $# -ne 2 ]; then
   exit 1
fi

infile="$1" # cat.pos
outfile="$2" # cat.phonetic

cat "$infile" | cut -d " " -f 1 | phonetic_trn -l ca-ca  | \
  perl -pe "s/\'([^ ]+)/\1-1/g;
              s/\`//g;
              s/-1/1/g; 
              s/@/ax/g;" > "$outfile"
