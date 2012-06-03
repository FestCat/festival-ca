#!/bin/sh

infile="../01LTSData/08/dicrevisat"
indir="00"
outdir="02"

mkdir -p "$indir"
cp "$infile" "$indir"


./upc_catalan_lts_rules "allowables-1.1.scm" "$indir/dicrevisat" "$outdir"
