#!/usr/bin/perl -wn

#if (/^#/) {
#   s/^#/;/;
#   print;
#   next;
#}



chomp;
if (3 != (($wrd, $pos, $trn) = split /\t/)) {
   print STDERR "Error in line: $_\n";
   next;
} 
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
print "(\"$wrd\" $pos $fest_trn)\n";


#'u - n i - k @ s =>
#("uniques" n (((u1) 1) ((n i) 0) ((k ax s) 0)))
#(((u1) 1) ((n i) 0) ((k ax s) 0))
#(((u1) 1) ((n i) 0) ((k ax s) 0))



#("uniques" n (        ((u1) 1) ((n i) 0) ((k ax s) 0)             )     )
