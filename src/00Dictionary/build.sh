#!/bin/sh

#echo "Transcribing"
#./transcribe.sh "withphonetics/partial" from_freeling/*  || exit 1
echo "Concatenating"
./concatenate.sh "withphonetics" "withphonetics/partial" || exit 1
echo "Creating dictionary in internal format"
./CreateInternalFormat.pl "withphonetics/all.orto" "withphonetics/all.phon" "merged/freeling.dic" || exit 1
echo "Compiling dictionary"
./CompileDictionary.sh "final" merged/*.dic || exit 1
echo "Adding final entries"
./addfinalentries.sh "final/upcdict_catalan.out" || exit 1
