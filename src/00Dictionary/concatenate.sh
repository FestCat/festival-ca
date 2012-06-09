#!/bin/bash

outdir="$1"
indir="$2"
dicorto="$outdir/all.orto"
dicphon="$outdir/all.phon"
rm -rf "$dicorto" "$dicphon"

for infile in $(ls "$indir");do
    if [ -d "$indir/$infile" ]; then
       continue
    fi
    if [ ! -f "$indir/$infile" ]; then
       echo "$indir/$infile not found. Skipping"
       continue
    fi
    filename=$(basename $infile)
    phofile="$indir"/phon/${filename%.*}.phonetic
    if [ ! -f  "$phofile" ];then
       echo "Phonetic translation $phofile not found for $infile. Skipping."
       continue
    fi
    cat "${indir}/${infile}" >> "$dicorto"
    cat "${phofile}" >> "$dicphon"
done


