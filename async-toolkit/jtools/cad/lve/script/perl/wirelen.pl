#!/usr/intel/bin/perl -w
# Copyright 2004 Fulcrum Microsystems.  All rights reserved.
# Authors: Kai He

use FileHandle;

my $def="";
my $lef_files="";
my $output="netfile.txt";
my $rthreshold=1.2;
my $lthreshold=10.0;
my $sort_mode="ratio";

sub usage() {
  return "Usage: $0\n".
        "  --def=[$def]\n".
        "  --lef-files=[$lef_files] (lef files seperated by :) \n".
        "  --output=[$output]\n".
        "  --sort-mode=[ratio|length]\n".
        "  --ratio-threshold=[$rthreshold]\n".
        "  --length-threshold=[$lthreshold]\n";
}

while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)/) {
    ($flag,$value) = split('=',$1);
    if ($flag eq "def") {
        $def = $value;
        shift @ARGV;
    } elsif ($flag eq "lef-files") {
        $lef_files = $value;
        shift @ARGV;
    } elsif ($flag eq "output") {
        $output = $value;
        shift @ARGV;
    } elsif ($flag eq "ratio-threshold") {
        $rthreshold = $value;
        shift @ARGV;
    } elsif ($flag eq "sort-mode") {
        $sort_mode = $value;
        shift @ARGV;
    } elsif ($flag eq "length-threshold") {
        $lthreshold = $value;
        shift @ARGV;
    } else { 
    shift @ARGV;
    }
}
        
defined $def or die usage();
defined $lef_files or die usage();
if( ! -e $def ){
   die usage() . "Error: def file $def not exist.\n";
}
foreach my $file (split(/:/, $lef_files)) {
    if( ! -e $file ){ 
        die usage() . "Error: lef file $file not exist.\n";
    }
}
if( $sort_mode ne "ratio" && $sort_mode ne "length" ){ die usage() . "Error: unknown sort mode $sort_mode.\n"; }

my %subcell;

#process lef files
foreach my $file (split(/:/, $lef_files)) {
  print "processing $file... \n";
  open( LEF, "<$file") or die "Can't open '$file' for reading.\n";

  $. = 0;
  $pin=0;
  $macro=0;
  $pinname="";
  $layername="";
  $cellname="";

  while($line=<LEF>){
    if ($line=~/\\/){$line=~s/\\//g;}
    if ($line=~/^MACRO (\S+)\s*$/){
        $cellname=$1;
        $macro=1;
    } elsif ( $macro && $line=~/^\s*FOREIGN (\S+)\s+(\S+)\s+(\S+)/){
        $cellname=$1;
        if(!defined $subcell{"cellname"} || $subcell{"cellname"} eq "" ){
           $subcell{"$cellname"}="$2 $3";
        }
    } elsif ( $macro && $line=~/^\s*ORIGIN (\S+)\s+(\S+)/){
        if(!defined $subcell{"cellname"} || $subcell{"cellname"} eq "" ){
           $subcell{"$cellname"}="$1 $2";
        }
    } elsif ( $macro && $line=~/^\s*SIZE (\S+)\s+BY\s+(\S+)\s*;\s*$/){
        $subcell{"$cellname:size"}="$1 $2";
    } elsif ( $macro && $line=~/^\s*PIN (\S+)\s*$/){
        $pinname=$1;
        $pin=1;
    } elsif ( $pin && $line=~/^\s*LAYER (\S+)/){
        $layername=$1;
        if( defined $subcell{"$cellname:$pinname"} ){
            $subcell{"$cellname:$pinname"}.=":$layername";
        } else {
            $subcell{"$cellname:$pinname"}="$layername";
        }
    } elsif ( $pin && $line=~/^\s*POLYGON/){
        chomp $line;
        $thisPin=$line;
        while($line!~/;/){
          $line=<LEF>;
          chomp $line;
          $thisPin.=" $line";
        }
        $thisPin=~s/\s+/ /g;
        $thisPin=~s/^\s+//;
        if( defined $subcell{"$cellname:$pinname:$layername"} ){
           $subcell{"$cellname:$pinname:$layername"}.=":$thisPin";
        } else {
           $subcell{"$cellname:$pinname:$layername"}="$thisPin";
        }
    } elsif ( $pin && $line=~/^\s*RECT/){
        chomp $line;
        $thisPin=$line;
        while($line!~/;/){
          $line=<LEF>;
          chomp $line;
          $thisPin.=" $line";
        }
        $thisPin=~s/\s+/ /g;
        $thisPin=~s/^\s+//;
        if( defined $subcell{"$cellname:$pinname:$layername"} ){
           $subcell{"$cellname:$pinname:$layername"}.=":$thisPin";
        } else {
           $subcell{"$cellname:$pinname:$layername"}="$thisPin";
        }
    } elsif ( $pin && $line=~/^\s*END\s*$/){
        $layername="";
    } elsif ( $pin && $line=~/^\s*END (\S+)\s*$/){
        $pinname=$1;
        $pin=0;
    } elsif ( $macro && $line=~/^\s*END (\S+)\s*$/){
        $cellname=$1;
        $macro=0;
    } 
  }
}

my $powerfile="powermeter.pwr";

open( DEF, "<$def") or die "Can't open '$def' for reading.\n";


$. = 0;
$components=0;
$nets=0;
$pins=0;
$thisNet=0;
my %comp;
my %netlist;
my %pinlist;
$comp{"PIN"}="PIN";
my %netStats;
my %netLengths;

while($line=<DEF>){
    if ($line=~/\\/){$line=~s/\\//g;}
    if ($line=~/^DESIGN (\S*)/){
        $cellname=$1;

    } elsif ( $line=~/^UNITS DISTANCE MICRONS (\S+)\s*;\s*$/){
        $units=$1;
    } elsif ( $line=~/^COMPONENTS /){
        print "processing COMPONENTS... \n";
        $components=1;
    } elsif ( $components && $line=~/^- (\S*) (\S*).*\(\s+(\S+) (\S+) \) (\S+)/){
        $instname=$1;
        $cellname=$2;
        $x=$3/$units; $y=$4/$units;
        $comp{"$instname:x"}=$x; 
        $comp{"$instname:y"}=$y; 
        $comp{"$instname:orient"}=$5;
        $comp{"$instname"}="$cellname $x $y $5";
    } elsif ( $line=~/^END COMPONENTS$/){
        $components=0;

    } elsif ( $line=~/^PINS /){
        print "processing PINS... \n";
        $pins=1;
    } elsif ( $pins && $line=~/- (\S*) \+ NET (\S*) /){
        $pinname=$1;
        $netname=$2;
    } elsif ( $pins && $line=~/LAYER (\S+) \( (\S+) (\S+) \) \( (\S+) (\S+) \)/){
        $pinlist{"$pinname:layer"}="$1";
        $lx=$2/$units; $ly=$3/$units; $rx=$4/$units; $ry=$5/$units;
        $pinlist{"$pinname:bBox"}="$lx $ly $rx $ry";
        $pinlist{"$pinname"}="$1 $lx $ly $rx $ry";
    } elsif ( $pins && $line=~/FIXED \( (\S+) (\S+) \) (\S+) ;/){
        $x=$1/$units; $y=$2/$units;
        $pinlist{"$pinname:x"}=$x;
        $pinlist{"$pinname:y"}=$y;
        $pinlist{"$pinname:orient"}=$3;
        $pinlist{"$pinname"}.=" $x $y $3";
    } elsif ( $line=~/^END PINS$/){
        $pins=0;

    } elsif ( $line=~/^NETS /){
        print "processing NETS... \n";
        $nets=1;
    } elsif ( $line=~/^SPECIALNETS /){
        print "processing SPECIALNETS... \n";
        $nets=1;
    } elsif ( $nets && $line=~/^- (\S*)/){
        $netname=$1;
        $netStats{$1}=$1;
        $thisNet=1;
        $thisLength=0;
        $thisNetPoints="";
    } elsif ( $nets && $thisNet && $line=~/^(\s*)\( (\S*) (\S*) \)/){
        chomp($line);
        @word=split(' ',$line);
        for( $i=0; $i<$#word; $i++){
            if($word[$i]eq"("){
                $i++; $instname=$word[$i];
                $i++; $pinname=$word[$i];
                $i++; # get rid of ")"
                if( defined $netlist{"$netname"} ){
                  $netlist{"$netname"}.=":".$comp{"$instname"}." $pinname";
                } else {
                  $netlist{"$netname"}=$comp{"$instname"}." $pinname";
                }
                ($x, $y)=&findPinXY($instname, $pinname);
                if($thisNetPoints eq ""){ 
                   $thisNetPoints="$x $y";
                } else {
                   $thisNetPoints.=":$x $y";
                }                   
            }
        }
    } elsif ( $nets && $thisNet && $line=~/ROUTED M[1-9].*\( ([0-9]+) ([0-9]+) [0-9\s]*\) \( ([0-9*]+) ([0-9*]+) [0\s]*\)/){
      $x1=$1; $y1=$2; $x2=$3; $y2=$4;
      if($x2 eq "*" || $x2==$x1){
        $thisLength+=abs($y2-$y1)/$units;
      } elsif($y2 eq "*" || $y2==$y1){
        $thisLength+=abs($x2-$x1)/$units;
      } else {
          print "\nError: non straight wiring in $netname: $line";
      }       
    } elsif ( $nets && $thisNet && $line=~/NEW M[1-9].*\( ([0-9]+) ([0-9]+) [0\s]*\) \( ([0-9*]+) ([0-9*]+) [0-9\s]*\)/){
      $x1=$1; $y1=$2; $x2=$3; $y2=$4;
      if($x2 eq "*" || $x2==$x1){
        $thisLength+=abs($y2-$y1)/$units;
      } elsif($y2 eq "*" || $y2==$y1){
        $thisLength+=abs($x2-$x1)/$units;
      } else {
          print "\nError: non straight wiring in $netname: $line";
      }       
    } elsif ( $nets && $thisNet && $line=~/^\s*;/){
        $thisNet=0;
        $netStats{$netname}=$thisNetPoints;
        $netLengths{$netname}=$thisLength;
    } elsif ( $line=~/^END NETS/){
        $nets=0;
    } elsif ( $line=~/^END SPECIALNETS/){
        $nets=0;
    } 
}

close(DEF);


foreach my $net (keys(%netStats)){
  $buffer="$net: "; 
  if( defined $netLengths{$net} ){
      $buffer.= "  Actual: $netLengths{$net} ";
      $actual=$netLengths{$net};
  } else {
      $buffer.= "  Actual: ERROR ";
      $actual=0;
  }
  if( $netStats{$net} ne $net ){
      @netpoints=split(':', $netStats{$net});
      $first=1;
      foreach $point (@netpoints){
          ($x, $y)=split(' ', $point);
          if( $first==1 || $minX>$x){$minX=$x;}
          if( $first==1 || $minY>$y){$minY=$y;}
          if( $first==1 || $maxX<$x){$maxX=$x;}
          if( $first==1 || $maxY<$y){$maxY=$y;}
          $first=0;
      }
      $manhattan=$maxY-$minY+$maxX-$minX;
      $pointCount=$#netpoints+1;
      $buffer.= " Manhattan: $manhattan of $pointCount points ";
  } else {
      $buffer.= " Manhattan: ERROR ";
      $manhattan=0; $pointCount=0;
  }
  if( $actual==0 || $manhattan==0 ){$netRatio{$net}=0;}
  else { $netRatio{$net}=$actual/$manhattan; }  
  if( abs($actual-$manhattan)>=$lthreshold && $netRatio{$net}>=$rthreshold ){
    $netBuffer{$net}=$buffer. "Ratio $netRatio{$net}\n";
  } 
}

if( $sort_mode eq "ratio" ){
  @sorted = reverse sort { $netRatio{$a} <=> $netRatio{$b} } keys %netBuffer; 
} elsif( $sort_mode eq "length" ){
  @sorted = reverse sort { $netLengths{$a} <=> $netLengths{$b} } keys %netBuffer; 
} else {
  die "Error: unknown sort mode $sort_mode"; 
}

open( TARGET, ">$output");

foreach my $net (@sorted){
  print TARGET $netBuffer{$net}; 
}

close( TARGET );

print "output file $output created. \n";


sub findPinXY {
  my($instname,$pinname)= @_;
  if($instname ne "PIN"){
    my($cellname, $instX, $instY, $orient)=split(' ', $comp{"$instname"});
    if( !defined $subcell{"$cellname"}){
        die "\nError: subcell $cellname not defined in lef files\n";
        return( 0.0, 0.0 );
    }
    my($x0,$y0)=split(' ', $subcell{"$cellname"});
    my($sizeX,$sizeY)=split(' ', $subcell{"$cellname:size"});
    my @pinLayerNames=split(':', $subcell{"$cellname:$pinname"});
    my $layername=$pinLayerNames[0];
    my @thisPin=split(':', $subcell{"$cellname:$pinname:$layername"});
    my($tmp, $lx, $ly, $rx, $ry)=split(' ', $thisPin[0]); 
    return( &transform(($lx+$rx)/2+$x0, ($ly+$ry)/2+$y0, $instX, $instY, $sizeX,$sizeY,$orient)); 
  } else {
      if( defined $pinlist{"$pinname:x"} && defined $pinlist{"$pinname:y"}){
          return( $pinlist{"$pinname:x"}, $pinlist{"$pinname:y"} );
      } else {
          print "Pin $instname location not defined, set to (0,0)\n";
          return( 0.0, 0.0 );
      }
  } 
}

sub transform {
  my($x,$y,$instX,$instY,$sizeX,$sizeY,$orient)= @_;
#  print "$x,$y,$instX,$instY,$sizeX,$sizeY,$orient = ";
  if ($orient eq "N") {
    $y += $instY;
    $x += $instX;
  } elsif ($orient eq "S") {
    $y = $instY+$sizeY-$y;
    $x = $instX+$sizeX-$x;
  } elsif ($orient eq "FN") {
    $y += $instY;
    $x = $instX+$sizeX-$x;
  } elsif ($orient eq "FS") {
    $x += $instX;
    $y = $instY+$sizeY-$y;
  } 
  elsif ($orient eq "W") {
    my $tx = $instX+$sizeY-$y;
    my $ty = $instY+$x;
    $x = $tx;
    $y = $ty;
  }
  elsif ($orient eq "E") {
    my $tx = $instX+$y;
    my $ty = $instY+$sizeX-$x;
    $x = $tx;
    $y = $ty;
  }
  else {
    $y += $instY;
    $x += $instX;
    print STDERR "Invalid orientation $orient\n";
  }
#  print "$x,$y\n ";
  return ($x, $y);     
}
