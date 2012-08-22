#!/usr/bin/perl
# Copyright (C) 2009-2011  Antonio Bonafonte,
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


#if (/^#/) {
#   s/^#/;/;
#   print;
#   next;
#}


while (<STDIN>) {
chomp;
next unless (3 == (($wrd, $pos, $trn) = split /\t/));
next unless $wrd =~ /[a-zñç]/i;

#print "WRD: $wrd\n"; print "POS: $pos\n"; print "TRN: $trn\n";

$trn =~ s/\s*$//;
$trn =~ s/@/ax/g;
$trn =~ s/\'([^ ]+)/${1}1/g;
$trn =~ s/\`([^ ]+)/${1}2/g;
@syl = split / *\- */, $trn;

$fest_trn = '(';
$first_syl = 1;
for $syl (@syl) {
    if ($first_syl == 1) {
	$first_syl = 0;
    } else {
	$fest_trn .= ' ';
    }
    $fest_trn .= '((';
    $fest_trn .= $syl;
    $fest_trn .= ') ';
    if ($syl =~ /1/) {
        $fest_trn .= '1)';
    } else {
        $fest_trn .= '0)';
    }
}
$fest_trn .= ')';
#print "(\"$wrd\" $pos $fest_trn)\n";
print "(\"$wrd\" nil $fest_trn)\n";

#'u - n i - k @ s =>
#("uniques" n (((u1) 1) ((n i) 0) ((k ax s) 0)))
#(((u1) 1) ((n i) 0) ((k ax s) 0))
#(((u1) 1) ((n i) 0) ((k ax s) 0))

#("uniques" n (        ((u1) 1) ((n i) 0) ((k ax s) 0)             )     )

}
