#!/usr/bin/perl  

my $input_dir="06";
my $output_dir="08"; 
mkdir $output_dir;

open(AR,"<$input_dir/diccionaricomplet")|| die "$!\n";
open(DIC,">$output_dir/dicrevisat") || die "No pudo crearse: $!"; 

@lista=("Vè", "ag", "am", "cc", "dawai", "dg", "dj", "ds", "h", "nov", "op", "pm", "prov", "ps", "ptge", "tm", "vg" );

 while($linea=<AR>)
  { 
    # Me quedo con la palabra clave:
    @palclave2=split('"',$linea);
    $palclave=$palclave2[1];
    $encontrada=0;
    for $pala (@lista) {
       if ($pala eq $palclave) {
           $encontrada=1;
           print "Linea:" . $linea;
           print "Palabra: " . $pala ."\n";
           last;
        }
    }

    if($encontrada eq 0){
       $linea =~ s/u0w/w/g;
       print DIC $linea;
     }
  }

close(AR);
close(DIC);
