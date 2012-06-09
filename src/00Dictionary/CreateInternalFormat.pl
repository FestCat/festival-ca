#!/usr/bin/perl  

use strict;
use warnings;
use File::Basename;

if ( $#ARGV != 2 ) { print "$#ARGV arguments given instead of 3. Not enough input arguments\n";exit 1;}

my $infileorto=$ARGV[0];
my $infilephon=$ARGV[1];
my $outfile=$ARGV[2];


my $output_dir=dirname($outfile);
mkdir $output_dir;

open (ORTO, "<$infileorto") || die "Could not read dictionary";
open (PHONE, "<$infilephon") || die "Could not read phonetic transcriptions of the dictionary";
open (DIC, ">$outfile") || die "Could not create file. $!";

my ($dictionarypos, $linea);
while(!(eof(PHONE))){
   #Lectura de ortografía 
   $linea=<ORTO>;
   #LLEGIM TRANSCRIPCIÓ FONÉTICA
   my $phonetictranslation =<PHONE>;
   
   # process:
   my @palabras=split(" ", $linea);
   my $dictionaryword = shift(@palabras);

   if ($dictionaryword =~ /\./ ) { #ommit abrev.
     next;
   }
   
   #Busca la categoria de la paraula
   shift(@palabras); # This is the root of the $dictionaryword. Currently we ignore it.

   $linea=join("",@palabras); # Now linea has the POS information
   my $tipo = substr($linea,0,1);

   # We only consider a small part of the POS information (TODO: Is it worth to improve this?)
   if ($tipo eq "D") { 
       $dictionarypos = "dt"; # determinant
   } elsif ($tipo eq "N") {
       $dictionarypos = "n"; # noun
   } elsif ($tipo eq "V") {
       $dictionarypos = "v"; # verb
   } elsif ($tipo eq "A") {
       $dictionarypos = "j"; # adjective
   } else {
       $dictionarypos = "nil"; # other
   }

  print DIC $dictionaryword . "\t" . $dictionarypos . "\t" . $phonetictranslation;
} #fin bucle principal
close(PHONE);
close(ORTO);
close(DIC);
