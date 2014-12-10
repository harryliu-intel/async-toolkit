# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

# Verification back-end LvbeAssura
# and Assura Extraction Utility routine
#
# This back-end provides Assura verification and extraction.  The following
# nvpairs are used by this back-end:
#
# 	Name			Description
#	
#	AssuraSet		Assura avParameters 'set' entries
#	AssuraType		Type of assura verification: Lvs or Drc
#	AssuraRsfInclude	Run-specific-file include file	
#	CastObject		Cast object for top-level cell
#	CdsLib			Cadence library configuration file
#	LayoutCell		Top-level layout cell name to be checked
#	LayoutLibrary		Cadence library containing layout
#	LayoutView		Cadence layout view name for top-level cell
#	GDS2File                GDS2 layout
#	OpenOK 			The string "true" if open connections are OK.
#	RuleFile		Extraction rule file
#       BindingFile             Binding rule file
#       CompareFile             Compare rule file
#	RunName			Run name
#	SchematicFile		CDL schematic file
#	Technology		Name of the Technology.
#	TechLib			Technology library configuration file

#	WorkingDir		Working Directory
#
# AssuraRsfInclude, CastObject, RunName, and WorkingDir are optional.  TechLib
# defaults to the value of CdsLib if it is not specified.  OpenOK defaults to
# "false" if it is not specified, and only has an effect if AssuraType is
# "LVS".  SchematicFile is not used when AssuraType is Drc.  CdsLib,
# LayoutCell, LayoutLibrary, LayoutView, RuleFile, SchematicFile, and
# Technology may be defined implicitly by CastObject.  AssuraType is
# not used for Assura extraction (XXX: but SchematicFile may be required).
#
# Insert the following lines in a Perl file to invoke verification:
#
# use LvbeAssura;
# LvbeAssura::verify();
#
# Insert the following lines in a Perl file to invoke extraction:
#
# use LvbeAssura;
# LvbeAssura::extract();
#
# Package variables
#
#	$PackageName	Package name (used internally
#	$wd		Working directory
#	$rn		Run name

package LvbeAssura;

#
# Package variables
#

$PackageName = "Assura";
$wd = "";
$rn = "";

#
# Set up Assura run
#

sub setup {
    #
    # Add defaults for optionally defined values
    #

    Lvfe::header ("BACK-END DEFAULT NAME-VALUE PAIRS");

      Lvfe::add_nvpair ("RunName", "ThisRun", "set", $PackageName);
      Lvfe::add_nvpair ("AssuraRsfInclude", "", "set", $PackageName);
      Lvfe::add_nvpair ("OpenOK", "false", "set", $PackageName);

      #
      # Ensure required name-value pairs are defined
      #
      @LayoutFiles =  Lvfe::get_nvpairs ("LayoutFile");
      $GDS2File   =  Lvfe::get_nvpair ("GDS2File");
      $OutFile   =  Lvfe::get_nvpair ("OutFile");

      if( !Lvfe::is_defined(@LayoutFiles) ) {
          Lvfe::assert_defined ("LayoutCell");
            if( !Lvfe::is_defined($GDS2File) ) {
                Lvfe::assert_defined ("CdsLib");
                  Lvfe::assert_defined ("LayoutLibrary");
                  Lvfe::assert_defined ("LayoutView");
                  Lvfe::assert_defined ("RunName");
                  Lvfe::add_nvpair ("TechLib", 
                                    Lvfe::get_nvpair("CdsLib"),
                                    "set",
                                    $PackageName);
                  Lvfe::assert_defined ("TechLib");              
              }
        }

      #
      # Set working directory package-local variable
      #

      $wd = Lvbe::get_workingdir ();

      #
      # Set run name package-local variable
      #

      $rn = Lvfe::get_nvpair ("RunName");

      #
      # Start Assura RSF (run-specific file) with an optional inclusion
      #

      $include = Lvfe::get_nvpair ("AssuraRsfInclude");
      if(Lvfe::is_defined($include) && -e $include && !-d $include ) {
          `cat $include > $wd/$rn\.rsf`;
      }
      else {
          `echo "" > $wd/$rn\.rsf`;
      }
  }

sub verify {
    #
    # Set up Assura back-end
    #
    
    setup();
    
    Lvfe::assert_defined ("AssuraType");
    my $type = Lvfe::get_nvpair ("AssuraType");
    if( !( $type eq "Drc" ||
           $type eq "Lvs" ||
           $type eq "Nvn" ) ) {
        Lvfe::error ("Unknown AssuraType (should be Lvs or Drc)");
      }

    #
    # Add run-specific details to rsf file.
    #
    open RSF, ">>$wd/$rn\.rsf" or die "ERROR: can't append to $wd/$rn\.rsf\n";

    print RSF "avParameters(\n";
    #input
    if(Lvfe::is_defined($GDS2File) ) { #GDS2
        print RSF "  ?inputLayout (\"GDS2\" \""
            . Lvfe::get_nvpair("GDS2File") 
            . "\" )\n";
        print RSF "  ?cellName \"" . Lvfe::get_nvpair ("LayoutCell") . "\"\n";
    } elsif(!Lvfe::is_defined(@LayoutFiles)) { #layout
        print RSF "  ?inputLayout (\"df2\" \"" 
            . Lvfe::get_nvpair("LayoutLibrary") 
            . "\" )\n";           
        print RSF "  ?cellName \"" . Lvfe::get_nvpair ("LayoutCell") . "\"\n";
        print RSF "  ?viewName \"" . Lvfe::get_nvpair ("LayoutView") . "\"\n";
        print RSF "  ?cdslib \"" . Lvfe::get_nvpair ("CdsLib") . "\"\n";
        print RSF "  ?techLib \"" . Lvfe::get_nvpair ("TechLib") . "\"\n";
        $technology =  Lvfe::get_nvpair ("Technology");
        if (Lvfe::is_defined ($technology)) {
            print RSF "  ?technology \"" . "$technology" . "\"\n";
        }
    }
    if( $type eq "Drc" ||
        $type eq "Lvs" ) {
        Lvfe::assert_file ("RuleFile");
          print RSF "  ?rulesFile \"" . Lvfe::get_nvpair ("RuleFile") . "\"\n";
    }
    if ($type eq "Drc") {
        print RSF "  ?saveByRule nil \n";
    }
    else {
        print RSF "  ?saveByRule nil \n"; # used to be t. see bug 11104
    }
    # causes Assura to ignore text not at top level, needed for Nevada Sizing
    if( ( $type eq "Drc" ) and ( Lvfe::get_nvpair("RuleFile") =~ m:bias.rul$: ) ) {
        print RSF "  ?textPriOnly t \n";
    }
    print RSF "  ?runName \"" . Lvfe::get_nvpair ("RunName") . "\"\n";
    print RSF "  ?workingDirectory \"" . Lvbe::get_workingdir() . "\"\n";

    @sets = Lvfe::get_nvpairs("AssuraSet");    
    if (@sets != 0) {      
        @fsets = map(Lvfe::enquote,@sets);
        print RSF "  ?set (@fsets )\n";
    }
    print RSF "  ?avrpt t\n";	
    print RSF ")\n";


    my @layFilters=Lvfe::get_nvpairs("LayFilters");
    my @schFilters=Lvfe::get_nvpairs("SchFilters");
    $area =  Lvfe::get_nvpair ("Area");
    if (Lvfe::is_defined ($area) && !($area eq "all") ) {
        @area = split(",",$area);
        print RSF "verifyArea( @area )\n";
    }

    my $compare = Lvfe::get_nvpair ("CompareFile" );
    if( ( $type eq "Lvs" ||
          $type eq "Nvn" ) && 
        defined $compare ) {
        if ( Lvfe::is_defined($compare) ) {
            Lvfe::assert_file ("CompareFile");
              open COMPARE, "$compare" or die "$0 Can't open $compare";
              while( <COMPARE> ) {
                  print RSF $_;
              }
          }
#        print RSF "joinableNet( )\n"; # would allow multi labels
        print RSF "avCompareRules( \n";

        print RSF "layout(\n";
        if (Lvfe::is_defined("IgnoreBulk") and
               Lvfe::get_nvpair("IgnoreBulk") eq "1") {
            print RSF "   deleteCellPin(MOS \"B\")\n";
        }
        if(Lvfe::is_defined(@LayoutFiles)) {
            foreach my $arg (@LayoutFiles) {
                my ($file,$type) = split(":",$arg);
                $type = " cdl" if(!defined $type);
                print RSF "   netlist( $type \"$file\" )\n";
            }
        }
        print RSF ")\n";
        print RSF "schematic(\n";
        if (Lvfe::is_defined("IgnoreBulk") and
               Lvfe::get_nvpair("IgnoreBulk") eq "1") {
            print RSF "   deleteCellPin(MOS \"B\")\n";
        }
        foreach my $arg (Lvfe::get_nvpairs("SchematicFile")) {
            my ($file,$type) = split(":",$arg);
            $type = "cdl" if(!defined $type);
            print RSF "netlist( $type \"$file\" )\n";
        }
        foreach my $opt (@schFilters) {
            my @f = split (/,/, $opt);
            print RSF "filterOptions( ";
            foreach my $f (@f) {
                print RSF "\"$f\" ";
            }
            print RSF ")\n";
        }
        print RSF ")\n";
        if (Lvfe::is_defined(@layFilters)) {
            print RSF "layout(\n";
            foreach my $opt (@layFilters) {
                my @f = split (/,/, $opt);
                print RSF "filterOptions( ";
                foreach my $f (@f) {
                    print RSF "\"$f\" ";
                }
                print RSF ")\n";
            }
            print RSF ")\n";
        }
#        print RSF "layout(\n  filterOptions( \"I\" )\n)\n";
        $bindingFile = Lvfe::get_nvpair ("BindingFile");
        if (Lvfe::is_defined ($bindingFile)) {
            print RSF "bindingFile(\"" . "$bindingFile" . "\")";
        }
        print RSF " )\n";
        if (Lvfe::get_nvpair ("OpenOK") eq "true") {
            print RSF "joinableNet( )\n";
        }
    }
    
    #exclude signoff cells from cell flattening
    if( $type eq "Drc"  && ! $OutFile ) {
        writeSignoffInclude(\*RSF);
    }

    if ($type eq "Lvs") {
        print RSF "avLVS()";
    } elsif ($type eq "Drc") {
        print RSF "avDX()";
    }
    
    close RSF;

    #
    # Record input to Assura
    #

    Lvfe::file_with_header ("ASSURA RUN-SPECIFIC FILE", "rsf", 
                            "$wd/$rn\.rsf");

    #
    # Run Assura
    #
    
    my $cmd;
    if ($type eq "Nvn") {
        $cmd = "nvn";
    } else {
        $cmd = "assura";
    }
    my $assura = $ENV{ASSURA_SCRIPT} || "/p/rrc/tools/bin/assura_oa";
    my $restart = "";
    $restart = "-restart" if(Lvfe::get_nvpair("Restart"));
    system("$assura $cmd $restart $wd/$rn\.rsf 2>$wd/$rn\.log > $wd/$rn\.log");

    #
    # Record output of Assura
    #
    Lvfe::file_with_header ("Run Log", "log", "$wd/$rn\.log");

    #
    # Record summary results
    #

    if ($type eq "Lvs" ||
        $type eq "Nvn" ) {
        #
        # Ensure that cell matching detail file exists
        #
        
        -e "$wd/$rn.cls" or Lvfe::error ("Could not find $wd/$rn\.cls");
        
        #
        # Search cell matching detail file for text indicating success
        #
        
        $results = `cat $wd/$rn.cls`;
        if ($results =~ /Schematic and Layout Match/) {
            Lvfe::pass();
          } else {
              Lvfe::fail();
	    }
    } elsif($type eq "Drc") {
        #
        # Ensure that DRC error coordinate file exists
        #
        
        -e "$wd/$rn\.err" or Lvfe::error("Could not find $wd/$rn\.err");
        
        #
        # Check for DRC error coordinate entries
        #
        
        $results = `cat $wd/$rn\.err`;
        if ($results =~ /[a-z]/) {
            Lvfe::fail();
          } else {
              Lvfe::pass();
            }
    }
}


sub writeSignoffInclude {
    my ($RSF) = @_;
    my @signoffs = Lvfe::get_nvpairs("SignOff");
    
    return if(scalar @signoffs == 0 );

    print $RSF "avParameters(\n";
    my %signOffCells = ();
    foreach my $signoff (@signoffs) {
        open SIGNOFF, "<$signoff" or die "Can't open $signoff for read";
        my $rule;
        while( <SIGNOFF> ) {
            if( /^\s*\(\s*cell\s*\"(.*)\"\s*\)/ ) {
                $signOffCells{$1} = 1;
            }
        }
        close SIGNOFF;
        print $RSF "  ?exceptionFile \"$signoff\"\n";
    }

    if(scalar keys %signOffCells == 0 ) {
        print $RSF ")\n";
        return;
    }

    print $RSF "  ?expandUniqueCells nil\n";
    print $RSF "  ?expandCellToTop ( excludeCell( ";
    foreach $signOffCell (keys %signOffCells) {
        $signOffCell  =~ s/\^/\?/g;
        print $RSF "\"$signOffCell\" ";
    }
    print $RSF ") )\n";
    print $RSF "  ?expandCellToParent ( excludeCell( ";
    foreach $signOffCell (keys %signOffCells) {
        $signOffCell =~ s/\^/\?/g;
        print $RSF "\"$signOffCell\" ";
    }
    print $RSF ") ) )\n";
    
    print $RSF "procedure( avFlattenCell( cellName width height numShapes numInsts )\n";
    print $RSF "prog( ()\n";
    foreach $flattenCellRegEx (Lvfe::get_nvpairs("FlattenCellRegEx")) {
        print $RSF "when( rexMatchp( \"$flattenCellRegEx\" cellName ) return(t) )\n";
    }
    foreach $signOffCell (keys %signOffCells) {
        print $RSF "when( rexMatchp( \"$signOffCell\" cellName ) return() )\n";
    }

    printf $RSF "when( (width <= 1.0 && height <= 1.0 && numInsts == 0 ) t )\n";
    printf $RSF ") )\n";
}


#
# Perform Assura extraction
#

sub extract {
    #
    # Set up Assura back-end
    #

    setup();

    #
    # Ensure that RuleFile exists
    #
    
    Lvfe::assert_file ("RuleFile");

    #
    # Add run-specific details to rsf file.
    #

    open RSF, ">>$wd/$rn\.rsf";

    print RSF "avParameters(\n";
    print RSF "  ?inputLayout (\"df2\" \"" 
        . Lvfe::get_nvpair("LayoutLibrary") 
        . "\" )\n";
    print RSF "  ?cellName \"" . Lvfe::get_nvpair ("LayoutCell") . "\"\n";
    print RSF "  ?viewName \"" . Lvfe::get_nvpair ("LayoutView") . "\"\n";
    print RSF "  ?cdslib \"" . Lvfe::get_nvpair ("CdsLib") . "\"\n";
    print RSF "  ?rulesFile \"" . Lvfe::get_nvpair ("RuleFile") . "\"\n";
    print RSF "  ?runName \"" . Lvfe::get_nvpair ("RunName") . "\"\n";
    print RSF "  ?workingDirectory \"" . Lvbe::get_workingdir() . "\"\n";

    @sets = Lvfe::get_nvpairs("AssuraSet");    
    if (@sets != 0) {      
        @fsets = map(Lvfe::enquote,@sets);
        print RSF "  ?set (@fsets )\n";
    }

    print RSF "  ?avrpt t\n";
    
    print RSF "  ?techLib \"" . Lvfe::get_nvpair ("TechLib") . "\"\n";
    print RSF ")\n";

    print RSF "rcxParameters(\n";
    print RSF "  ?runName \"" . Lvfe::get_nvpair ("RunName") . "\"\n";
    print RSF "  ?outputFormat \"spice\"\n";
    print RSF "  ?output \"" . Lvbe::get_workingdir() . "/extract\"\n";
    
    print RSF ")\n";

    #
    # Add extraction-specific details to rsf file.
    #

    print RSF "avCompareRules( schematic( ";
    foreach my $arg (Lvfe::get_nvpairs("SchematicFile")) {
        my ($file,$type) = split(":",$arg);
        $type = "cdl" if(!defined $type);
        print RSF "netlist( $type \"$file\")\n";
    }
    print RSF " ))\n";
    if (Lvfe::get_nvpair ("OpenOK") eq "true") {
        print RSF "joinableNet( )\n";
    }
    print RSF "avLVS()\n";
    print RSF "avRCX()\n";

    close RSF;

    #
    # Record input to Assura
    #

    Lvfe::file_with_header ("ASSURA RUN-SPECIFIC FILE", "rsf", 
                            "$wd/$rn\.rsf");

    #
    # Run Assura
    #

    `assura $wd/$rn\.rsf 2>$wd/$rn\.log > $wd/$rn\.log`;

    #
    # Record output of Assura
    #

    Lvfe::file_with_header ("Run Log", "log", "$wd/$rn\.log");
    Lvfe::file_with_header ("Labels and text information", "erc",
                            "$wd/$rn\.erc");
    Lvfe::file_with_header("DRC error coordinates", "err", "$wd/$rn\.err");

    Lvfe::file_with_header("Cell Match Status", "csm", 
                           "$wd/$rn\.csm");
    Lvfe::file_with_header("LVS mismatches", "cls", "$wd/$rn\.cls");

    #
    # Ensure that cell matching detail file exists
    #

    -e "$wd/$rn.cls" or Lvfe::error ("Could not find $wd/$rn\.cls");

    #
    # Search cell matching detail file for text indicating success
    #

    $results = `cat $wd/$rn.cls`;
    if (!($results =~ /Schematic and Layout Match/)) {
        Lvfe::fail("Could not extract parasitics");
      }
}

#
# Perl 'require' requires that a module return a true value.
#

1;
