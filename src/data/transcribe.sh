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


# This script transcribes freeling dictionary files into phonemes using phonetic_trn.
which phonetic_trn > /dev/null || ( echo "phonetic_trn is not available (internal tool). Please provide your own phonetic transcriptions." ; exit 0 )

outdirpartial="$1"
outdirpartialphon="$outdirpartial/phon"
shift
mkdir -p "$outdirpartial"
mkdir -p "$outdirpartialphon"

for infile in $@; do
    filename=$(basename $infile)
    outfile="$outdirpartialphon"/${filename%.*}.phonetic
    
    if [ ! -f "$infile" ]; then
       echo "$infile not found. Skipping"
       continue
    fi
    
    # Convert UTF-8 to ISO-8859-15:
    infile2=$outdirpartial/${filename}
    file -ib "$infile" | grep "utf-8"  > /dev/null
    if [ $? -eq 0 ]; then 
        echo "Converting $infile to ISO-8850-15"
        iconv -f "utf8" -t "iso-8859-15" "$infile" > "${infile2}"
    else
        cp "$infile" "${infile2}"
    fi
    cat "${infile2}" | cut -d " " -f 1 | phonetic_trn -l ca-ca  | \
    perl -pe "s/\'([^ ]+)/\1-1/g;
              s/\`//g;
              s/-1/1/g; 
              s/@/ax/g;" > "$outfile"
done


