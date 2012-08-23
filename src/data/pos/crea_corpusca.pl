#!/usr/bin/perl  -w
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

use v5.14;
use warnings;

use XML::LibXML;

#open(my $file,"<",$ARGV[0]);
my $cosa = XML::LibXML->load_xml(line_numbers => 1, IO=>*STDIN);
#close $file;
my ($word, $lexema, $pos, $newpos, $punct,$tipo, $mood);
my $nodes = $cosa->findnodes('//*');
foreach my $node ($nodes->get_nodelist) {
  my $children;
  for ($node->findnodes('*')->size) {
    $children = $_;
    if ($children == 0 && 
       ( not(defined($node->getAttribute("elliptic"))) || not($node->getAttribute("elliptic") eq "yes"))  &&
       ( not(defined($node->getAttribute("missing"))) || not($node->getAttribute("missing") eq "yes"))  &&
       ( $node->localname() ne "morfema.pronominal" ) &&
       ( $node->localname() ne "grup.nom" ) ) {
       $word = $node->getAttribute("wd");
       $word =~ s/&quot;/"/g;
       $lexema = $node->getAttribute("lem");
       $lexema =~ s/&quot;/"/g;
#       if ($lexema eq "&quot;") {
#          $lexema = '"';
#       }
       $pos = $node->getAttribute("pos");

       if (not(defined($pos))) {
          $pos = $node->localname();

          if ($pos eq "f") {
             $punct = $node->getAttribute("punct"); 
             if (defined($punct)) {
                if($punct eq "quotation") {
                    if ($word eq "'" or $word eq '"') {
                        $pos = "Fe";
                    } else {
                      print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$punct\".Label?\n";
                      exit 1;
                    }
                } elsif ($punct eq "bracket") {
                    if ($word eq "(") {
                       $pos = "Fpa";
                    } elsif ($word eq ")") {
                       $pos = "Fpt";
                    } else {
                      print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$punct\".Label?\n";
                      exit 1;
                    }
                } elsif ($punct eq "period") {
                    if ($word eq ".") {
                       $pos = "Fp";
                    } else {
                      print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$punct\".Label?\n";
                      exit 1;
                    }
                } else {
                    print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$punct\".Label?\n";
                    exit 1;
                }
             } elsif ($word eq "(") {
                  $pos = "Fpa";
             } elsif ($word eq ")") {
                  $pos = "Fpt";
             } else {
                print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos\".Label?\n";
                exit 1;
             }
          } elsif ($pos eq "w") {
          } elsif ($pos eq "z") {
          } elsif ($pos eq "i") {
          } elsif ($pos eq "a") {
             $tipo = $node->getAttribute("postype");
             if (defined($tipo)) {
                if ($tipo eq "qualificative") {
                   $pos = "aq";
                } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$tipo\".Label?\n";
                   exit 1;
                }
             } else {
                print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos\".Label?\n";
                exit 1;
             }
          } elsif ($pos eq "v") {
             $tipo = $node->getAttribute("postype");
             if (defined($tipo)) {
                 if ($tipo eq "auxiliary") {
                     $pos .= "a";
                 } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$tipo\".Label?\n";
                   exit 1;
                 }
             } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos\".Label?\n";
                   exit 1;
             }
             $mood = $node->getAttribute("mood");
             if (defined($mood)) {
                 if ($mood eq "indicative") {
                     $pos .= "i";
                 } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$tipo,$mood\".Label?\n";
                   exit 1;
                 }
             } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$tipo\".Label?\n";
                   exit 1;
             }
          } elsif ($pos eq "n") {
             $tipo = $node->getAttribute("postype");
             if (defined($tipo)) {
                 if ($tipo eq "proper") {
                   $pos = "np";
                 } elsif ($tipo eq "common") {
                   $pos = "nc";
                 } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$tipo\".Label?\n";
                   exit 1;
                 }
             } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos\".Label?\n";
                   exit 1;
             }
          } elsif ($pos eq "s") {
             $tipo = $node->getAttribute("postype");
             if (defined($tipo)) {
                 if ($tipo eq "preposition") {
                   $pos = "sp";
#                 } elsif ($tipo eq "common") {
#                   $pos = "nc";
                 } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$tipo\".Label?\n";
                   exit 1;
                 }
             } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos\".Label?\n";
                   exit 1;
             }
          } elsif ($pos eq "d") {
             $tipo = $node->getAttribute("postype");
             if (defined($tipo)) {
                 if ($tipo eq "numeral") {
                   $pos = "dn";
#                 } elsif ($tipo eq "common") {
#                   $pos = "nc";
                 } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$tipo\".Label?\n";
                   exit 1;
                 }
             } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos\".Label?\n";
                   exit 1;
             }
          } elsif ($pos eq "c") {
             $tipo = $node->getAttribute("postype");
             if (defined($tipo)) {
                 if ($tipo eq "subordinating") {
                   $pos = "cs";
                 } elsif ($tipo eq "coordinating") {
                   $pos = "cc";
                 } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$tipo\".Label?\n";
                   exit 1;
                 }
             } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos\".Label?\n";
                   exit 1;
             }
          } elsif ($pos eq "p") {
             $tipo = $node->getAttribute("postype");
             if (defined($tipo)) {
               if ($tipo eq "relative" ) {
                  $pos = "pr";
               } elsif  ($tipo eq "personal") {
                  $pos = "pp";
               } elsif  ($tipo eq "interrogative") {
                  $pos = "pt";
               } elsif  ($tipo eq "numeral") {
                  $pos = "pn";
               } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos,$tipo\".Label?\n";
                   exit 1;
               }
             } else {
                   print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos\".Label?\n";
                   exit 1;
             }
          } else {
              print STDERR "Word: \"$word\" (line " . $node->line_number . "), classified as \"$pos\".Label?\n";
              exit 1;
          }
       } # end "if not defined pos"
         


       if (not(defined($word)) or not(defined($lexema)) or not(defined($pos))) {
           print STDERR "Error on line: " . $node->line_number  . "\n";
           exit 1;
       }
       # POS information needs some format transformation:
       $pos=uc($pos); # I've seen that we work on uppercase POS, except on POS which start by F or Z.
       if ( length($pos) > 1 and ( (substr($pos,0,1) eq "F") or (substr($pos,0,1) eq "Z")) ) {
          $newpos = substr($pos,0,1) . lc(substr($pos,1));
       } else {
          $newpos = $pos;
       }

       print $word . " " . $lexema . " " . $newpos .  "\n";
    }
  }
}

