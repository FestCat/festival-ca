#!/usr/bin/perl  

my $input_dir="04";
my $output_dir="06"; 
mkdir $output_dir;

open(AR,"<$input_dir/dicnoabreviatures")|| die "$!\n";
open(DIC,">$output_dir/diccionaricomplet") || die "No pudo crearse: $!"; 

my $linea;
 while($linea=<AR>)
  { 
     if(!($linea =~ /\'/)) {
     print DIC $linea;
    } else {print $linea;};
  }

close(AR);
close(DIC);
