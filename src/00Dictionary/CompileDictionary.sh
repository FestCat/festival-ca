#!/bin/bash

outdir="$1"
shift
infile=$@

mkdir -p "$outdir"

FESTIVAL=`which festival`

cat $infile | ./allo2pho.pl | \
    perl -pe 's/^ +//; s/ +$//; s/  +/ /g; s/ *\t */\t/g' |\
    LC_ALL=POSIX sort -u | ./list2festival.pl > "$outdir/upcdict_catalan.scm"


#$FESTIVAL -b '(lex.compile "$outdir/upcdict_catalan.scm" "06/upcdict_catalan.out")'
$FESTIVAL -b "(lex.compile \"$outdir/upcdict_catalan.scm\" \"$outdir/upcdict_catalan.out\")"
