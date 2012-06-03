#!/usr/bin/perl  

my $input_dir="00-input";
my $output_dir="02";

mkdir $output_dir;
open (PHONE, "<$input_dir/cat.phonetic") || die "No pudo leerse phonetic_prova";
open (ORTO, "<$input_dir/cat.pos") || die "No pudo leerse escrit_prova";
open (DIC, ">$output_dir/dicformat") || die "No pudo crearse: $!";

my $dicline;
while(!(eof(PHONE))){
   #Lectura de ortografía 1 fila 1 paraula
   my $linea=<ORTO>;
   my @palabras=split(" ", $linea);
   $dicline = '("' . shift(@palabras) . '" ';

   #Busca la categoria de la paraula
   $linea=join("",@palabras);
   $i=0;
   while (!( ($tipo= substr($linea,$i,1))  =~ /[A-Z]/ )) {
      $i=$i+1;
   }
   if ($tipo eq "D") {
       $dicline .= 'dt (';
   } elsif ($tipo eq "N") {
       $dicline .= 'n (';
   } elsif ($tipo eq "V") {
       $dicline .= 'v (';
   } elsif ($tipo eq "A") {
       $dicline .= 'j (';
   } else {
       $dicline .= 'nil (';
   }

   #LLEGIM TRANSCRIPCIÓ FONÉTICA
   $linea =<PHONE>;
   chomp $linea;
   for ($i = 0; $i<length($linea);$i=$i+1) {
      $car_leido=substr($linea,$i,1);
      if ($car_leido eq "'") { #VOCAL TÓNICA
         $i+=1;$car_leido=substr($linea,$i,1);
         $dicline .= $car_leido . "r";
      } elsif ($car_leido eq "`") { #ACCENT SECUNDARI
         $i+=1;$car_leido=substr($linea,$i,1);
         $dicline .= $car_leido . "r";
      } elsif ($car_leido eq '-') {
         $i+=1;$car_leido=substr($linea,$i,1);
      } else {
         $dicline .= $car_leido;
      }  
  }
     chop $dicline; # remove an space
     $dicline .= "))\n";

  print DIC $dicline;
} #fin bucle principal
close(PHONE);
close(ORTO);
close(DIC);
