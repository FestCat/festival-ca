#! /bin/bash
# This file converts Unicode phonetic transcriptions to something closer to SAMPA (Festival phonemes).
# usage: ./unicode2sampa.sh < unicode_file.txt > sampa_output.txt

rm -f tempA
while read line; do
	echo $(echo "$line" | awk 'BEGIN{FS=""}; {for (i=1; i<=NF; i++) print $i}') >> tempA
done 

sed -e 's/á/a1/g' -e 's/é/e1/g' -e 's/ɛ́/E1/g' 's/ɔ́/O1/g' -e 's/ó/o1/g' \
    -e 's/í/i1/g' -e 's/ú/u1/g' -e 's/r/rr/g' -e 's/ɾ/r/g' \
    -e 's/ʤ/Z/g' -e 's/ʎ/L/g' -e 's/ð/D/g' -e 's/ɣ/G/g' -e 's/ʃ/S/g' \
    -e 's/ŋ/N/g' -e 's/ɲ/J/g' -e 's/ʧ/tS/g'  tempA 

rm tempA
