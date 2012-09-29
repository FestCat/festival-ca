#!/usr/bin/perl -w
# Copyright (C) 2012  Sergio Oller
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

BEGIN {
use Cwd 'abs_path';
use File::Basename;
push @INC, dirname(abs_path($0)) . "/../../functions/";
}

require "functions_perl.pl";

my %outhash;
my $num=0;
while ( my $line = <STDIN> )
{
   $num+=1;
   chomp $line;
   if (length($line) eq 0) { next;}

   my @fields = split(/[\t\s]+/,$line);
   if (scalar(@fields) != 3 ) {
       print STDERR "Error a la a línia $num: \"$line\". Té ". scalar(@fields) . " camps. \n";
       exit 1;
  }
   my $word = lc($fields[0]); # real word
   # my $lexema = $fields[1];
   my $tag = $fields[2];
   $tag = &simplificapos($tag);
   {
      if (not(defined($outhash{$word}{$tag}))) {
              $outhash{$word}{$tag} = 0;
      }
      $outhash{$word}{$tag}+=1;
   }
}

foreach $word (sort keys(%outhash)) {
   $possiblepos = join("-", sort keys($outhash{$word})); 
   print STDOUT $word . " " . $possiblepos . " ";
   foreach $pos (sort keys($outhash{$word})) {
     print STDOUT "$pos $outhash{$word}{$pos} ";
   }
   print STDOUT "\n";
}

