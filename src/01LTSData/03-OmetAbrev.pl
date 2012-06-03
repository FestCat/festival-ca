#!/usr/bin/perl 

my $input_dir="02";
my $output_dir="04"; 
mkdir $output_dir;
open(AR,"<$input_dir/dicformat")|| die "$!\n";
open(DIC,">$output_dir/dicnoabreviatures") || die "No pudo crearse: $!"; 

my $linea;
while($linea=<AR>)
  { 
     if(!($linea =~ /\./)  ) {
		print DIC $linea;
    } else {
           chomp $linea;
	   print "Ommiting: \"$linea\"\n";
    }
  }

close(AR);
close(DIC);
