#!/usr/bin/perl -nw

chomp;

if (3 != (($wrd, $pos, $trn) = split /\t/)) {
    print STDERR "Format error line: $_\n";
}

$trn =~ tr/BDGNv/bdgnf/;
$trn =~ s/ts/t s/g;
$trn =~ s/tS/t S/g;
$trn =~ s/dz/d z/g;
$trn =~ s/dZ/d Z/g;
$trn =~ s/uw/w/g;
$trn =~ s/y/j/g;

print "$wrd\t$pos\t$trn\n";
