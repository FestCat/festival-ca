#!/usr/bin/perl  
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

#my $infile="../data/ngram/corpus-ca";
#my $outfile="work/corpus-ca-reduitsTAGS.out";

BEGIN {
use Cwd 'abs_path';
use File::Basename;
push @INC, dirname(abs_path($0)) . "/../functions";
}

require "functions_perl.pl" or die $!;



 while(my $linea=<STDIN>)
  { 
	($word, $lexema, $pos) = split(/\s+/,$linea);
    $pos=&simplificapos($pos);
    print join(' ',$word,$lexema,$pos) . "\n";
  }

