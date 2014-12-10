# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

# Verification back-end library
#
# The verification back-end library consists of routines shared among
# back-ends.  Routine names and descriptions:
#
#	Name			Description	
#
#	get_tempdir		Make a temporary directory which will be 
#				cleaned up 
#				when the program exits.  Takes no arguments.
#	get_workingdir		Get the name of the working directory for this 
#				run.  Takes no arguments.
#	nvpairs_to_skill	Get a skill expression which will encode the
#				name-value pairs as setq's.  'Lv' is prepended 
#				to all names.

package Lvbe;

#
# Load front-end library
#

use Lvfe;

#
# Add a temporary file or directory to the deletion list
#

sub add_file_to_deletion_list {
	($file) = @_;

	#
	# Check that the file is in the 'tmp' directory
	#

	$file =~ /\/tmp\/Lvbe/ or die "Invalid temporary file";
	
	#
	# Add the file to the deletion list
	#

	push @deletion_list, $file;
}


#
# Make a new temporary directory, and return its full path
#

# Uses package-local variable 'count'

sub get_tempdir {

	#
	# Set counter if necessary
	#

	defined $count or $count = 0;

	#
	# Find a count yielding an unused temporary filename
	#

	$file = "/tmp/Lvbe.$$.$count";

	while (-e $file) {
		$count++;
		$count != 0 or Lvfe::error ("No available temporary files");
		$file = "/tmp/Lvbe.$$.$count";
	}

	#
	# Make a temporary directory with this filename
	#
	
	`mkdir $file`;

	#
	# Add the new temporary directory to the deletion list
	#

	add_file_to_deletion_list ($file);

	#
	# Return the new temporary directory name
	#

	return $file;
}

#
# Get the full path for a technology library
#

sub get_techlibpath {
	my ($library) = @_;

	#
	# Use the value of CdsLib as a default value of TechLib
	#

	if (!Lvfe::is_defined (Lvfe::get_nvpair ("TechLib"))) {
		my $cdslib = Lvfe::get_nvpair ("CdsLib");
		Lvfe::add_nvpair ("TechLib", $cdslib, "set", "Default");
	}

	Lvfe::assert_defined ("TechLib");

	my $techlib = Lvfe::get_nvpair ("TechLib");
	my @techlib_contents=`cat $techlib`;
	my %libmap;

	foreach $line (@techlib_contents) {
		$line =~ s/^DEFINE\s+// or next;
		$line =~ /^(\S+)\s+(\S+)\/*$/ or next;
		$libmap{$1} = $2;
	}

	$path = $libmap{$library};

	Lvfe::error "Can't find library $library" if !defined $path;

	return $path;
}

#
# Get the full path for a library
#

sub get_libpath {
	my ($library) = @_;

	Lvfe::assert_defined ("CdsLib");

	my $cdslib = Lvfe::get_nvpair ("CdsLib");
	my @cdslib_contents = `cat $cdslib`;
	my %libmap;

	foreach $line (@cdslib_contents) {
		$line =~ s/^DEFINE\s+// or next;
		$line =~ /^(\S+)\s+(\S+)\/*$/ or next;
		$libmap{$1} = $2;
	}

	$path = $libmap{$library};

	Lvfe::error "Can't find library $library" if !defined $path;

	return $path;
}

#
# Get the working directory for this run
#

sub get_workingdir {
	
	#
	# Check 'WorkingDir' nvpair for directory definition
	#

	my $wd = Lvfe::get_nvpair ("WorkingDir");

	#
	# Set up a default working directory if necessary
	#

	if (!Lvfe::is_defined ($wd)) {
		$wd = get_tempdir ();
		Lvfe::header "SET UP DEFAULT WORKING DIRECTORY";
		Lvfe::add_nvpair ("WorkingDir", $wd, "set", "Default");
	}

	#
	# Flag an error if a non-directory file conflicts
	#

	!-e $wd or -d $wd or Lvfe::error "WorkingDir $wd name conflict";

	#
	# Make working directory if necessary
	#

	-e $wd or `mkdir -p $wd`;
	-e $wd or Lvfe::error "Cannot make WorkingDir $wd";

	#
	# Remove any trailing '/' from the directory name
	#

	$wd =~ s/\/$//;

	#
	# Flag an error if /tmp is used (XXX: stage one only).
	#

	Lvfe::error "Cannot use files in /tmp for WorkingDir in stage one." 
		if $wd =~ "^/tmp";

	return $wd;
}

#
# Get the RC extraction for a layout cell
#

sub get_extractfile {

	#
	# Check 'ExtractFile' nvpair for file definition
	#

	my $ef = Lvfe::get_nvpair ("ExtractFile");

	#
	# Arrange for extraction to occur if necessary
	#

	if (!Lvfe::is_defined ($ef)) {
		use LvbeAssura;
		$wd = get_workingdir();
		LvbeAssura::extract();
		$ef = "$wd\/extract";
		Lvfe::add_nvpair ("ExtractFile", $ef, "set", "Extracted");
	}

	#
	# Ensure that extract file exists
	#

	-e $ef or Lvfe::error "Cannot find extract file $ef";

	return $ef;
}

#
# Return a skill expression which encodes name-value pairs
#

# N.B. The 'value' of the nvpair is always a list, so it is usually appropriate
# to use expressions such as '(car LvFoo)' rather than just 'LvFoo'.

sub nvpairs_to_skill {

	$expression = "";

	@keys = Lvfe::get_names ();
	
	for $key (@keys) {
		$expression .= "(setq Lv$key (list";
		@values = get_nvpairs ($key);
		for $value (@values) {
			$expression .= " \"$value\"";
		}
		$expression .= "))\n";
	}
}

#
# Package finalization -- delete temporary files and directories
#

END {
	foreach $deletion_item (@deletion_list) {
		`rm -rf $deletion_item`;
	}
}

#
# Perl 'require' requires that a module return a true value.
#

1;
