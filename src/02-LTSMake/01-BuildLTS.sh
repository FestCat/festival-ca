#!/bin/sh

infile="../00Dictionary/final/upcdict_catalan.out"
indir="00"
outdir="02"

mkdir -p "$indir"
cp "$infile" "$indir"

dict=$indir/$(basename "$infile")

./upc_catalan_lts_rules "allowables-1.1.scm" "$dict" "$outdir"
