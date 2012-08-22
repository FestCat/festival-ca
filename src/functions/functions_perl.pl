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

sub simplificapos {
 my $pos=shift;
$pos =~ s/^AO.*/AO/;
$pos =~ s/^AQ.*/AQ/;
$pos =~ s/^CC.*/CC/; # needed for CC-ET
#    $pos =~ s/^CS.*/CS/; # not needed
$pos =~ s/^DA.*/DA/;
$pos =~ s/^DD.*/DD/;
$pos =~ s/^DE.*/DE/;
$pos =~ s/^DI.*/DI/;
$pos =~ s/^DN.*/DN/;
$pos =~ s/^DP.*/DP/;
$pos =~ s/^DR.*/DR/;    
$pos =~ s/^DT.*/DT/;
#    $pos =~ s/^F.*/F/;  # Punctuation is not simplified
$pos =~ s/^NC.*/NC/;
$pos =~ s/^NP.*/NP/;
$pos =~ s/^P00.*/P0/;
$pos =~ s/^P01.*/P0/;
$pos =~ s/^P02.*/P0/;
$pos =~ s/^P03.*/P0/;
$pos =~ s/^PD.*/PD/;
$pos =~ s/^PE.*/PE/;
$pos =~ s/^PI.*/PI/;
$pos =~ s/^PN.*/PN/;
$pos =~ s/^PP.*/PP/;
$pos =~ s/^PR.*/PR/;
$pos =~ s/^PT.*/PT/;
$pos =~ s/^PX.*/PX/;

$pos =~ s/^RG.*/RG/; # needed for RG-CC
#    $pos =~ s/^RN.*/RN/; # not needed
$pos =~ s/^SP.*/SP/;
$pos =~ s/^VAG.*/VAG/;
$pos =~ s/^VAI.*/VAI/;
$pos =~ s/^VAM.*/VAM/;
$pos =~ s/^VAN.*/VAN/;
$pos =~ s/^VAP.*/VAP/;
$pos =~ s/^VAS.*/VAS/;

$pos =~ s/^VMG.*/VMG/;
$pos =~ s/^VMI.*/VMI/;
$pos =~ s/^VMM.*/VMM/;
$pos =~ s/^VMN.*/VMN/;
$pos =~ s/^VMP.*/VMP/;
$pos =~ s/^VMS.*/VMS/;

$pos =~ s/^VSG.*/VSG/;
$pos =~ s/^VSI.*/VSI/;
$pos =~ s/^VSM.*/VSM/;
$pos =~ s/^VSN.*/VSN/;
$pos =~ s/^VSP.*/VSP/;
$pos =~ s/^VSS.*/VSS/;

#    $pos =~ s/^I.*/I/; # not needed
#    $pos =~ s/^W.*/W/; # not needed
#    $pos =~ s/^Y.*/Y/; # not needed
 return $pos;
}

1;
