#!/usr/bin/perl -nw
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

chomp;

if (3 != (($wrd, $pos, $trn) = split /\t/)) {
    print STDERR "Format error line: $_\n";
}

$trn =~ tr/BDG/bdg/;
$trn =~ s/ts/t s/g;
$trn =~ s/dz/d z/g;
$trn =~ s/uw/w/g;
$trn =~ s/y/j/g;

print "$wrd\t$pos\t$trn\n";