#!/bin/bash
# Copyright (C) 2009-2011  Antonio Bonafonte
#            Universitat Politècnica de Catalunya, Barcelona, Spain
#
#  This script is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation,
#  version 2.1 of the License.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA


# ./concatenate.sh outputdir inputdir
# 
# inputdir contains:
#    1. One or more files with ISO-8859-15 encoding and the following
#       format (without the leading '#') [ let's call this file "adjs" ]:
#abacial abacial AQ0CS0
#abacials abacial AQ0CP0
#abadal abadal AQ0CS0
#abadals abadal AQ0CP0
#abaldufada abaldufat AQ0FS0
#abaldufades abaldufat AQ0FP0
#abaldufat abaldufat AQ0MS0
#abaldufats abaldufat AQ0MP0
#
#    2. A directory named "phon" which contains:
#      2.1 For each file in inputdir, a file with the same name and
#          the suffix ".phonetic" appended to it and the following 
#          format [ this file would be "adjs.phonetic" ]:
#ax - B ax - s i - a1 l 
#ax - B ax - s i - a1 l s 
#ax - B ax - D a1 l 
#ax - B ax - D a1 l s 
#ax - B ax l - d u - f a1 - D ax 
#ax - B ax l - d u - f a1 - D ax s 
#ax - B ax l - d u - f a1 t 
#ax - B ax l - d u - f a1 t s 
#
# This script would take this two files


outdir="$1"
indir="$2"
dicorto="$outdir/all.orto"
dicphon="$outdir/all.phon"
rm -rf "$dicorto" "$dicphon"
mkdir -p "$outdir"

for infile in $(ls "$indir");do # for all files in indir
    # If the file is a directory, we ignore it.
    if [ -d "$indir/$infile" ]; then
       continue
    fi
    # if it is not a file we ignore it
    if [ ! -f "$indir/$infile" ]; then
       echo "$indir/$infile not found. Skipping"
       continue
    fi
    # for this dictionary file, we determine it's phonetic transcriptions.
    filename=$(basename $infile)
    phofile="$indir"/phon/${filename%.*}.phonetic
    # If we don't find the corresponding phonetic transcriptions, we skip this file.
    if [ ! -f  "$phofile" ];then
       echo "Phonetic translation $phofile not found for $infile. Skipping."
       continue
    fi
    # We concatenate all dictionary files and phonetic transcriptions in two files
    cat "${indir}/${infile}" >> "$dicorto"
    cat "${phofile}" >> "$dicphon"
done


