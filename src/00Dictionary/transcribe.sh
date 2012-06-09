#!/bin/bash

which phonetic_trn >/dev/null || ( echo "phonetic_trn is not available (internal tool). Please provide your own phonetic transcriptions." ; exit )

outdirpartial="$1"
outdirpartialphon="$outdirpartial/phon"
shift
mkdir -p "$outdirpartial"
mkdir -p "$outdirpartialphon"

for infile in $@;do
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


