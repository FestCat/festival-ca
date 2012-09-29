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

if [ ! -f "${NGRAM_BUILD}" ]; then
   if [ ! -d "$ESTDIR" ];then
       NGRAM_BUILD=$(which ngram_build) || ( echo "Could not find ngram_build"; exit 1 )
   else
       NGRAM_BUILD="$ESTDIR/bin/ngram_build"
   fi
fi

infile1="$1"
infile2="$2"
outfile="$3"
${NGRAM_BUILD} "${infile1}" -o "${outfile}" -otype cstr_bin -w "${infile2}" -order 3 -smooth 15 -input_format sentence_per_line -dense -backoff 3 -floor 100 -freqsmooth 15 -oov_mode use_oov_marker -default_tags || exit 1
