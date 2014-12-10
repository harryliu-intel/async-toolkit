#!/usr/local/bin/perl
#
# Convert a GDS-II file into more easily parseable XML
# (and back again)
#

use Getopt::Std;
use strict;

our($opt_i,$opt_o);
getopt('io');

my $data = "";

my @rtypes = ( "HEADER","BGNLIB","LIBNAME","UNITS","ENDLIB","BGNSTR",
     "STRNAME","ENDSTR","BOUNDARY","PATH","SREF","AREF","TEXT","LAYER",
     "DATATYPE","WIDTH","XY","ENDEL","SNAME","COLROW","TEXTNODE","NODE",
     "TEXTTYPE","PRESENTATION","SPACING","STRING","STRANS","MAG","ANGLE",
     "UINTEGER","USTRING","REFLIBS","FONTS","PATHTYPE","GENERATIONS",
     "ATTRTABLE","STYPTABLE","STRTYPE","ELFLAGS","ELKEY","LINKTYPE",
     "LINKKEYS","NODETYPE","PROPATTR","PROPVALUE","BOX","BOXTYPE",
     "PLEX","BGNEXTN","ENDTEXTN","TAPENUM","TAPECODE","STRCLASS","RESERVED",
     "FORMAT","MASK","ENDMASKS","LIBDIRSIZE","SRFNAME","LIBSECUR"
             );

my @rtypes_dtype = ( 2,2,6,5,0,2,6,0,0,0,0,0,0,2,2,3,3,0,6,2,0,0,2,1,7,
                     6,1,5,5,7,7,6,6,2,2,6,6,2,1,3,7,7,2,2,6,0,2,3,3,3,
		     2,2,1,3,2,6,0,2,6,2
	 	   );

my @dtypes = ( "No Data", "Bit Array", "Two-Byte Signed Int",
               "Four-Byte Signed Int", "Four-Byte Real", "Eight-Byte Real",
	       "ASCII String", "Unknown"
             );

my %strfmt_lookup = (
   1 => "GDSII Archive Format",
   2 => "GDSII Filtered Format",
   3 => "EDSM Archive Format",
   4 => "EDSHI Filtered Format"
);

my %pathtype_lookup = (
   0 => "flush square",
   1 => "round",
   2 => "square extended",
   4 => "variable"
);

my $input;

if (!$opt_i) {
  print "Usage: gdsII2xml.pl -i input_file ( -o output_file )\n";
  exit 1;
}

open($input,"<$opt_i") or die "Error: Can't open file $opt_i, $!\n";

if ($opt_o) {
  open STDOUT, '>', "$opt_o" or die "Error: Can't write to $opt_o, $!\n";
} 

my %rtype_name = ();
foreach (0..59) {
  $rtype_name{$rtypes[$_]} = $_;
}

parse_gdsIIfile($input);
close($input);
close(STDOUT);

#
# Subroutines follow
#

sub parse_gdsIIfile {
  my $input = shift;

  my ($mod_time,$acc_time,$libdirsize,$srfname,$libname);
  my (@libsecur,@reflibs,@fonts,$uunits,$units,$generations);
  my ($attrtable);
  my ($len,$rt,$dt,@vals) = &read_record($input);
  die_if_not("HEADER",$rt);
  print qq(<gdsIIfile version=") . shift(@vals) . qq(">\n);
  ($len,$rt,$dt,@vals) = &read_record($input);

  die_if_not("BGNLIB",$rt);
  ($mod_time,$acc_time) = &parse_times(@vals);
  ($len,$rt,$dt,@vals) = &read_record($input);
 
  if ($rt == $rtype_name{"LIBDIRSIZE"}) {
    $libdirsize = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($input);
  }

  if ($rt == $rtype_name{"SRFNAME"}) {
    $srfname = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($input);
  }

  if ($rt == $rtype_name{"LIBSECUR"}) {
    @libsecur = @vals;
    ($len,$rt,$dt,@vals) = &read_record($input);
  }

  die_if_not("LIBNAME",$rt);
  $libname = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($input);

  if ($rt == $rtype_name{"REFLIBS"}) {
    @reflibs = @vals;
    ($len,$rt,$dt,@vals) = &read_record($input);
  }

  if ($rt == $rtype_name{"FONTS"}) {
    @fonts = @vals;
    ($len,$rt,$dt,@vals) = &read_record($input);
  }

  if ($rt == $rtype_name{"ATTRTABLE"}) {
    $attrtable = @vals;
    ($len,$rt,$dt,@vals) = &read_record($input);
  }

  if ($rt == $rtype_name{"GENERATIONS"}) {
    $generations = @vals;
    ($len,$rt,$dt,@vals) = &read_record($input);
  }

  die_if_not("UNITS",$rt);
  $uunits = shift(@vals);
  $units = shift(@vals); 
  ($len,$rt,$dt,@vals) = &read_record($input);

  print qq(<library name=") . $libname . qq(" units=") . $units .
        qq(" userunits=") . $uunits . qq(" mtime=") . $mod_time .
	qq(" atime=") . $acc_time . qq(");
  $libdirsize && print qq( libdirsize=") . $libdirsize . qq(");
  $srfname && print qq( srfname=") . $srfname . qq(");
  $generations && print qq( generations=") . $generations . qq(");
  $attrtable && print qq( attrtable=") . $attrtable . qq(");
  print qq(>\n);
  foreach (@libsecur) { print qq(  <libsecur val=") . $_ . qq("/>\n); }
  foreach (@reflibs) { print qq(  <reflib file=") . $_ . qq("/>\n); }
  foreach (@fonts) { print qq(  <font file=") . $_ . qq("/>\n); }

  while ($rt == $rtype_name{"BGNSTR"}) {
    read_structure($input,@vals);
    ($len,$rt,$dt,@vals) = &read_record($input);
  }

  die_if_not("ENDLIB",$rt);
  print qq(</library></gdsIIfile>\n);
}

sub die_if_not {
  my $expected_rt = shift;
  my $actual_rt = shift;
  ($rtype_name{$expected_rt} == $actual_rt) or die "Parse Error: " .
  "Expected $expected_rt record but found $rtypes[$actual_rt].\n";
}

sub read_structure {
  my $fh = shift;
  my @vals = @_;
  my ($len,$rt,$dt);
  my ($name,$class,$atime,$mtime,$strname,$strclass);
  
  ($mtime,$atime) = &parse_times(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  die_if_not("STRNAME",$rt);
  $strname = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  if ($rt == $rtype_name{"STRCLASS"}) {
    $strclass = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  print qq(  <structure name=") . $strname . qq(" mtime=") .  $mtime . 
        qq(" atime=") . $atime . qq(");
  $strclass && print qq( class=") . $strclass . qq(");
  print qq(>\n);

  while ($rt != $rtype_name{"ENDSTR"}) {
    if ($rt == 0x08)      { read_boundary($fh);
    } elsif ($rt == 0x09) { read_path($fh);
    } elsif ($rt == 0x0a) { read_sref($fh);
    } elsif ($rt == 0x0b) { read_aref($fh);
    } elsif ($rt == 0x0c) { read_text($fh);
    } elsif ($rt == 0x15) { read_node($fh);
    } elsif ($rt == 0x2d) { read_box($fh);
    } else { die "Parse Error: Found unexpected record type: $rt\n"; }
    ($len,$rt,$dt,@vals) = &read_record($fh);
  } 
  print qq(  </structure>\n);
}

sub read_boundary {
  my $fh = shift;
  my ($len,$rt,$dt,@vals) = &read_record($fh);
  my ($layer,$datatype,@elflags,$plex);

  if ($rt == $rtype_name{"ELFLAGS"}) {
    @elflags = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"PLEX"}) {
    $plex = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  die_if_not("LAYER",$rt);
  $layer = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  die_if_not("DATATYPE",$rt);
  $datatype = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  print qq(    <boundary layer=") . $layer . qq(" datatype=") . $datatype .
        qq(");
  $plex && print qq( plex=") . $plex . qq(");
  $elflags[15] && print qq( template="1");
  $elflags[14] && print qq( external="1");
  print qq(>\n);

  die_if_not("XY",$rt);
  while (@vals) {
    print qq(      <xy x=") . shift(@vals) . qq(" y=") . shift(@vals) . 
          qq("/>\n);
  }
  ($len,$rt,$dt,@vals) = &read_record($fh);

  while ($rt == $rtype_name{"PROPATTR"}) {
      my $pn = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      die_if_not("PROPVALUE",$rt);
      my $pv = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      print qq(      <property number=") . $pn . qq(">) . $pv .
            qq(</property>\n);
  }
  die_if_not("ENDEL",$rt);
  print qq(    </boundary>\n);
}

sub read_path {
  my $fh = shift;
  my ($len,$rt,$dt,@vals) = &read_record($fh);
  my ($layer,$datatype,@elflags,$plex);
  my ($bgnextn,$endextn,$width);

  if ($rt == $rtype_name{"ELFLAGS"}) {
    @elflags = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"PLEX"}) {
    $plex = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  die_if_not("LAYER",$rt);
  $layer = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  my $pathtype = 0;
  die_if_not("DATATYPE",$rt);
  $datatype = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  if ($rt == $rtype_name{"PATHTYPE"}) {
    $pathtype = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"WIDTH"}) {
    $width = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"BGNEXTN"}) {
    ($pathtype != 4) && die "Parse Error: BGNEXTN record but not" .
                            " PATHTYPE 4\n";
    $bgnextn = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == 0x31) {
    ($pathtype != 4) && die "Parse Error: ENDEXTN record but not" .
                            " PATHTYPE 4\n";
    $endextn = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  print qq(    <path layer=") . $layer . qq(" datatype=") . $datatype .
        qq(" pathtype=") . $pathtype . qq(");
  $width && print qq( width=") . $width . qq(");
  $bgnextn && print qq( bgnextn=") . $bgnextn . qq(");
  $endextn && print qq( endextn=") . $endextn . qq(");
  $plex && print qq( plex=") . $plex . qq(");
  $elflags[15] && print qq( template="1");
  $elflags[14] && print qq( external="1");
  print qq(>\n);

  die_if_not("XY",$rt);
  while (@vals) {
    print qq(      <xy x=") . shift(@vals) . qq(" y=") . shift(@vals) . 
          qq("/>\n);
  }
  ($len,$rt,$dt,@vals) = &read_record($fh);

  while ($rt == $rtype_name{"PROPATTR"}) {
      my $pn = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      die_if_not("PROPVALUE",$rt);
      my $pv = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      print qq(      <property number=") . $pn . qq(">) . $pv .
            qq(</property>\n);
  }
  die_if_not("ENDEL",$rt);
  print qq(    </path>\n);
}

sub read_sref {
  my $fh = shift;
  my ($len,$rt,$dt,@vals) = &read_record($fh);
  my ($mag,$angle,$sname,@strans,@elflags,$plex);

  if ($rt == $rtype_name{"ELFLAGS"}) {
    @elflags = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"PLEX"}) {
    $plex = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  die_if_not("SNAME",$rt);
  $sname = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  if ($rt == $rtype_name{"STRANS"}) {
    @strans = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);

    if ($rt == $rtype_name{"MAG"}) {
      $mag = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);
    }

    if ($rt == $rtype_name{"ANGLE"}) {
      $angle = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);
    }
  }

  print qq(    <sref ref=") . $sname . qq(");
  $mag && print qq( magnification=") . $mag . qq(");
  $angle && print qq( angle=") . $angle . qq(");
  $strans[0] && print qq( reflect="1");
  $strans[13] && print qq( absmag="1");
  $strans[14] && print qq( absangle="1");
  $plex && print qq( plex=") . $plex . qq(");
  $elflags[15] && print qq( template="1");
  $elflags[14] && print qq( external="1");
  print qq(>\n);

  die_if_not("XY",$rt);
  while (@vals) {
    print qq(      <xy x=") . shift(@vals) . qq(" y=") . shift(@vals) . 
          qq("/>\n);
  }
  ($len,$rt,$dt,@vals) = &read_record($fh);

  while ($rt == $rtype_name{"PROPATTR"}) {
      my $pn = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      die_if_not("PROPVALUE",$rt);
      my $pv = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      print qq(      <property number=") . $pn . qq(">) . $pv .
            qq(</property>\n);
  }
  die_if_not("ENDEL",$rt);
  print qq(    </sref>\n);
}

sub read_aref {
  my $fh = shift;
  my ($len,$rt,$dt,@vals) = &read_record($fh);
  my ($mag,$angle,$sname,@strans,@elflags,$plex,$col,$row);

  if ($rt == $rtype_name{"ELFLAGS"}) {
    @elflags = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"PLEX"}) {
    $plex = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  die_if_not("SNAME",$rt);
  $sname = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  if ($rt == $rtype_name{"STRANS"}) {
    @strans = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);

    if ($rt == $rtype_name{"MAG"}) {
      $mag = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);
    }

    if ($rt == $rtype_name{"ANGLE"}) {
      $angle = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);
    }
  }

  die_if_not("COLROW",$rt);
  ($col,$row) = @vals;
  ($len,$rt,$dt,@vals) = &read_record($fh);

  print qq(    <aref ref=") . $sname . qq(" cols=") . $col .
        qq(" rows=") . $row . qq(");
  $mag && print qq( magnification=") . $mag . qq(");
  $angle && print qq( angle=") . $angle . qq(");
  $strans[0] && print qq( reflect="1");
  $strans[13] && print qq( absmag="1");
  $strans[14] && print qq( absangle="1");
  $plex && print qq( plex=") . $plex . qq(");
  $elflags[15] && print qq( template="1");
  $elflags[14] && print qq( external="1");
  print qq(>\n);

  die_if_not("XY",$rt);
  while (@vals) {
    print qq(      <xy x=") . shift(@vals) . qq(" y=") . shift(@vals) . 
          qq("/>\n);
  }
  ($len,$rt,$dt,@vals) = &read_record($fh);

  while ($rt == $rtype_name{"PROPATTR"}) {
      my $pn = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      die_if_not("PROPVALUE",$rt);
      my $pv = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      print qq(      <property number=") . $pn . qq(">) . $pv .
            qq(</property>\n);
  }
  die_if_not("ENDEL",$rt);
  print qq(    </aref>\n);
}

sub read_text {
  my $fh = shift;
  my ($len,$rt,$dt,@vals) = &read_record($fh);
  my ($layer,$texttype,@elflags,$plex,@strans);
  my ($mag,$angle,$width);
  my ($pathtype,@pres);

  if ($rt == $rtype_name{"ELFLAGS"}) {
    @elflags = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"PLEX"}) {
    $plex = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  die_if_not("LAYER",$rt);
  $layer = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  die_if_not("TEXTTYPE",$rt);
  $texttype = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  if ($rt == $rtype_name{"PRESENTATION"}) {
    @pres = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"PATHTYPE"}) {
    $pathtype = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"WIDTH"}) {
    $width = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"STRANS"}) {
    @strans = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);

    if ($rt == $rtype_name{"MAG"}) {
      $mag = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);
    }

    if ($rt == $rtype_name{"ANGLE"}) {
      $angle = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);
    }
  }

  print qq(    <text layer=") . $layer . qq(" texttype=") . $texttype . qq(");
  $pathtype && print qq(" pathtype=") . $pathtype . qq(");
  $width && print qq( width=") . $width . qq(");
  $mag && print qq( magnification=") . $mag . qq(");
  $angle && print qq( angle=") . $angle . qq(");
  $strans[0] && print qq( reflect="1");
  $strans[13] && print qq( absmag="1");
  $strans[14] && print qq( absangle="1");
  !$pres[15] && !$pres[14] && print qq( halign="left");
  $pres[15] && !$pres[14] && print qq( halign="center");
  !$pres[15] && $pres[14] && print qq( halign="right");
  !$pres[13] && !$pres[12] && print qq( valign="top");
  $pres[13] && !$pres[12] && print qq( valign="middle");
  !$pres[13] && $pres[12] && print qq( valign="bottom");
  !$pres[11] && !$pres[10] && print qq( font="font0");
  $pres[11] && !$pres[10] && print qq( font="font1");
  !$pres[11] && $pres[10] && print qq( font="font2");
  $pres[11] && $pres[10] && print qq( font="font3");
  $plex && print qq( plex=") . $plex . qq(");
  $elflags[15] && print qq( template="1");
  $elflags[14] && print qq( external="1");
  print qq(>\n);

  die_if_not("XY",$rt);
  while (@vals) {
    print qq(      <xy x=") . shift(@vals) . qq(" y=") . shift(@vals) . 
          qq("/>\n);
  }
  ($len,$rt,$dt,@vals) = &read_record($fh);

  die_if_not("STRING",$rt);
  print qq(     <string>) . shift(@vals) . qq(</string>\n);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  while ($rt == $rtype_name{"PROPATTR"}) {
      my $pn = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      die_if_not("PROPVALUE",$rt);
      my $pv = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      print qq(      <property number=") . $pn . qq(">) . $pv .
            qq(</property>\n);
  }
  die_if_not("ENDEL",$rt);
  print qq(    </text>\n);
}

sub read_node {
  my $fh = shift;
  my ($len,$rt,$dt,@vals) = &read_record($fh);
  my ($layer,@elflags,$plex,$nodetype);

  if ($rt == $rtype_name{"ELFLAGS"}) {
    @elflags = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"PLEX"}) {
    $plex = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  die_if_not("LAYER",$rt);
  $layer = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  die_if_not("NODETYPE",$rt);
  $nodetype = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  print qq(    <node layer=") . $layer . qq(" nodetype=") . $nodetype .
        qq(");
  $plex && print qq( plex=") . $plex . qq(");
  $elflags[15] && print qq( template="1");
  $elflags[14] && print qq( external="1");
  print qq(>\n);

  die_if_not("XY",$rt);
  while (@vals) {
    print qq(      <xy x=") . shift(@vals) . qq(" y=") . shift(@vals) . 
          qq("/>\n);
  }
  ($len,$rt,$dt,@vals) = &read_record($fh);

  while ($rt == $rtype_name{"PROPATTR"}) {
      my $pn = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      die_if_not("PROPVALUE",$rt);
      my $pv = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      print qq(      <property number=") . $pn . qq(">) . $pv .
            qq(</property>\n);
  }
  die_if_not("ENDEL",$rt);
  print qq(    </node>\n);
}

sub read_box {
  my $fh = shift;
  my ($len,$rt,$dt,@vals) = &read_record($fh);
  my ($layer,@elflags,$plex,$boxtype);

  if ($rt == $rtype_name{"ELFLAGS"}) {
    @elflags = split('',shift(@vals));
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  if ($rt == $rtype_name{"PLEX"}) {
    $plex = shift(@vals);
    ($len,$rt,$dt,@vals) = &read_record($fh);
  }

  die_if_not("LAYER",$rt);
  $layer = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  die_if_not("BOXTYPE",$rt);
  $boxtype = shift(@vals);
  ($len,$rt,$dt,@vals) = &read_record($fh);

  print qq(    <box layer=") . $layer . qq(" boxtype=") . $boxtype .
        qq(");
  $plex && print qq( plex=") . $plex . qq(");
  $elflags[15] && print qq( template="1");
  $elflags[14] && print qq( external="1");
  print qq(>\n);

  die_if_not("XY",$rt);
  while (@vals) {
    print qq(      <xy x=") . shift(@vals) . qq(" y=") . shift(@vals) . 
          qq("/>\n);
  }
  ($len,$rt,$dt,@vals) = &read_record($fh);

  while ($rt == $rtype_name{"PROPATTR"}) {
      my $pn = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      die_if_not("PROPVALUE",$rt);
      my $pv = shift(@vals);
      ($len,$rt,$dt,@vals) = &read_record($fh);

      print qq(      <property number=") . $pn . qq(">) . $pv .
            qq(</property>\n);
  }
  die_if_not("ENDEL",$rt);
  print qq(    </box>\n);
}

sub parse_times {
  my @return = ();
  while (@_) {
    my $yr = shift;
    my $mo = shift;
    my $da = shift;
    my $ho = shift;
    my $mi = shift;
    my $se = shift;
    my $time = $yr+1900 . sprintf("-%02d-%02d %02d:%02d:%02d",$mo,$da,$ho,$mi,$se,);
    push(@return,$time);
  }
  return @return;
}

sub read_record {
  my $fh = shift;
  my $data = "";

  read $fh,$data,2;
  my $length = unpack 'n', $data;

  read $fh,$data,2;
  my ($rtype,$dtype) = unpack 'C2', $data;

  if ($rtypes_dtype[$rtype] != $dtype) {
    die "Parse Error: Resource type $rtypes[$rtype] should use
    $dtypes[$rtypes_dtype[$rtype]], but record uses $dtypes[$dtype]\n";
  }

  my @values = ();
  if ( $dtype == 1 ) { 
    read $fh,$data,$length-4;
    @values = unpack 'B*',$data; 
  } elsif ( $dtype == 2) { 
    @values = &decode_2bi($length-4,$fh); 
  } elsif ( $dtype == 3) { 
    @values = &decode_4bi($length-4,$fh);
  } elsif ( $dtype == 4) { 
    read $fh,$data,$length-4;
    die "Parse Error: 4-Byte Reals unsupported.\n";
  } elsif ( $dtype == 5) { 
    @values = &decode_8br($length-4,$fh);
  } elsif ( $dtype == 6) { 
    @values = &decode_string($length-4,$fh); 
  }

  return ($length,$rtype,$dtype,@values);
}

sub decode_2bi {
  my ($bytes,$fh) = @_;
  my $data = "";
  my @results = ();
  for (1..($bytes/2)) {
    read $fh,$data,2;
    my $bits = unpack 'B16',$data;
    my @bits = split(/ */,$bits);
    my $sign = shift(@bits);
    my $res;
    if ($sign) {
      $res = &tcbits2num(@bits);
    } else {
      $res = &bits2num(@bits);
    }
    push @results,$res;
  }
  return @results;
}

sub decode_4bi {
  my ($bytes,$fh) = @_;
  my $data = "";
  my @results = ();
  for (1..($bytes/4)) {
    read $fh,$data,4;
    my $bits = unpack 'B*',$data;
    my @bits = split(/ */,$bits);
    my $sign = shift(@bits);
    my $res;
    if ($sign) {
      $res = &tcbits2num(@bits);
    } else {
      $res = &bits2num(@bits);
    }
    push @results,$res;
  }
  return @results;
}

sub decode_8br {
  my ($bytes,$fh) = @_;
  my $data = "";
  my @results = ();
  for (1..($bytes/8)) {
    read $fh,$data,1;
    my $bits = unpack 'B8',$data;
    my @bits = split(/ */,$bits);
    my $sign = shift(@bits);
    my $exp = &bits2num(@bits);
    read $fh,$data,7;
    $bits = unpack 'B56',$data;
    @bits = split(/ */,$bits);
    my $mant = &bits2num(@bits);
    my $res = ($mant/(2**56)) * (16**($exp-64));
    if ($sign) { $res *= -1; }
    push @results,$res;
  }
  return @results;
}

sub decode_string {
  my ($bytes,$fh) = @_;
  my $data = "";
  my @results = ();
  read $fh,$data,$bytes;
  @results = split("\x00",$data);
  return @results;
}

sub bits2num {
  my $num = scalar @_;
  my $res = 0;
  foreach $b (@_) {
    $res += $b * (2**($num-1));
    $num--;
  }
  return $res;
}

sub tcbits2num {
  # Two's Complement Decode
  my $num = scalar @_;
  my $res = -1;
  foreach $b (@_) {
    $res -= (1-$b) * (2**($num-1));
    $num--;
  }
  return $res;
}

