# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

# Verification front-end library
#
# The verification front-end library contains routines used by the executable
# 'vfe' and the various back-ends to communicate with the user and with each
# other.  Shared routine names and descriptions:
#
#	Name			Description
#
#	doc			Print static informational message
#	info			Print informational message
#	doc_header		Print header for static information
#	header			Print informational header
#	file			Print a file
#	file_with_header	Print a file with an informational header
#	error			Exit with an error
#	pass			Pass a test
#	fail			Fail a test
#	get_names		Get an array of all names
#	get_nvpairs		Get an array of values associated with a name
#	get_nvpair		Get the first value associated with a name
#	is_defined		True if a value is defined
#	assert_defined		Ensure that a name has an associated definition
#	assert_file		Ensure that name represents a readable file
#	add_nvpair		Add a (name, value) pair (set or append)
#	add_nvpair_string	Add a (name, value) pair from a string
#	stage_one_hacks		A function to perform stage-one hacks
#
# More complete descriptions and subroutine parameters may be found with the
# corresponding subroutine definitions below.

package Lvfe;

#
# Separator for messages printed to standard output
#

$Separator = "*" x 70;

#
# Print arbitrary message with a given line prefix
#

sub message {
	my ($prefix, @message) = @_;

	#
	# Handle the case for an empty message
	#

	if (@message == 0) {
		print "$prefix:\n";
		return;
	}

	#
	# Handle the case for a non-empty message
	#

	foreach $message_element (@message) {
		@calculated_lines = split /\n/, $message_element;
		foreach $calculated_line (@calculated_lines) {
			print "$prefix: $calculated_line\n";
		}
	}
}

#
# Print static informational message
#

sub enquote {
    $arg = $_;
    return qq("$arg");
}

sub doc {
	message ("Doc", @_);
}

#
# Print informational message
#

sub info {
	message ("Info", @_);
}

#
# Print header for static information
#

sub doc_header {
	doc $Separator;
	doc @_;
	doc $Separator;
}

#
# Print informational header
#

sub header {
	info $Separator;
	info @_;
	info $Separator;
}

#
# Print a file with a given line prefix and file name.  Prefix will be
# prepended with a colon.
#

sub file {
	my ($prefix, $filename) = @_;
	-r $filename or error ("Cannot read $filename");
	@file_contents = `cat $filename`;
	message (":$prefix", @file_contents);
}

#
# Print a file with given line prefix and file name.  Prefix will be prepended
# with a colon.  Also, print an informational header.
#

sub file_with_header {
	my ($header, $prefix, $filename) = @_;
	header ("INCLUDED FILE: $header", 
		"FILENAME: $filename", "LINE PREFIX: $prefix");
	file ($prefix, $filename);
}

#
# Exit with an error
#

sub error {
	message ("ERROR", @_);
	exit 1;
}

#
# Pass a test
#

sub pass {
	message ("PASS", @_);
        exit 0;
}

#
# Fail a test
#

sub fail {
	message ("FAIL", @_);
        exit 2;
}

#
# Get an array of all values corresponding to a name
#

sub get_nvpairs {
	my ($name) = @_;

	return () unless $value = $nvpair_value{$name};

	return split /\|/, $value;
}

#
# Get first value corresponding to a name
#

sub get_nvpair {
	my ($name) = @_;
	@values = get_nvpairs ($name);
	if (@values == 0) {
		return "";
	} else {
		return $values[0];
	}
}

#
# Check whether a value is defined
#

sub is_defined {
	my ($value) = @_;
	return 0 if (!defined $value || $value eq "");
	return 1;
}

#
# Assert that a value is defined for a given name
#

sub assert_defined {
	my ($name) = @_;
	error "'$name' is not defined!" if !is_defined (get_nvpair($name));
}

#
# Assert that a name represents a readable file (and not a directory)
#

sub assert_files {
    foreach $value (get_nvpairs($name)) {
        error "'$name' is not defined" if !is_defined ($value);
        check_file($value);
    }
}

sub assert_file {
    my ($name) = @_;
    my ($value) = get_nvpair ($name);
    error "'$name' is not defined" if !is_defined ($value);
    check_file($value);
}

sub check_file {
    my ($value) = @_;
    error "File '$value' for '$name' doesn't exist" if !-e $value;
    error "File '$value' for '$name' is a directory" if -d $value;
    error "File '$value' for '$name' is not readable" if !-r $value;
}

#
# Subroutine to print a (name, value) assignment event
#

# TYPE is "set", "append", or "ignore".  SOURCE indicates the source of the
# assignment.

sub print_nvpair {
	($name, $value, $type, $source) = @_;

	if (defined $value && $value ne "") {
		$printable_value = $value;
	} else {
		$printable_value = "''";
	}

	Lvfe::message ("Pair", "($source, $type, $name, $printable_value)");
}

#
# Subroutine to clear a (name, value) pair.
#

# NAME indicates the name for the pair to be cleared.  After clearing, no
# values will be associated with NAME.  SOURCE describes the unit that is
# performing the clearing.

sub clear_nvpair {
	($name, $source) = @_;

	undef $nvpair_set{$name};
	undef $nvpair_value{$name};

	Lvfe::message ("Delp", "($name, $source)");
}

#
# Subroutine to add a (name, value) pair.
#

# TYPE is "set" or "append", indicating whether additional pairs with the
# same name should be ignored or appended.  NAME and VALUE form the (name,
# value) pair.  Global variables used include Lvfe::nvpair_value, which stores 
# the (name, value) pairs as a hash, and nvpair_set, which specifies whether
# additional pairs with the same name should be appended or ignored.

sub add_nvpair {
	($name, $value, $type, $source) = @_;

	if ($nvpair_set{$name}) {
		print_nvpair ($name, $value, "ignored", $source);
		return;
	}
	
	if (defined $Lvfe::nvpair_value{$name} && defined $value) {
		$Lvfe::nvpair_value{$name} .= "|" . $value;
	} elsif (defined $value) {
		$Lvfe::nvpair_value{$name} = $value;
	}

	if ($type eq "set") {
		$nvpair_set{$name} = 1;
	} elsif ($type ne "append") {
		Lvfe::error "add_nvpair: invalid type $type.";
	}

	print_nvpair ($name, $value, $type, $source);
}

#
# Subroutine to add a (name, value) pair as specified by a string.
#

# STRING should be "<NAME>=<VALUE>", to specify that additional values for the
# same name should be ignored, or "<NAME>.=<VALUE>", to specify that additional
# values for the same name should be appended.

sub add_nvpair_string {
	($string, $source) = @_;
	
	if ($string =~ /([^=]+)\.=(.*)/) {
		add_nvpair ($1, $2, "append", $source);
	} elsif ($string =~ /([^=]+)=(.*)/) {
		add_nvpair ($1, $2, "set", $source);
	} else {
		Lvfe::error "$source: Invalid pair $string";
	}
}

#
# Subroutine to get all names
#

sub get_names {
	return keys %nvpair_value;
}

#
# Subroutine to perform stage-one hacks
#

# Define CastFile and CastCell from LayoutCell.

sub stage_one_hacks {

	my $sig = "StageOneHacks";

	doc ("To perform stage-one hacks, define StageOne=true and LayoutCell");

	my $stage_one = get_nvpair ("StageOne");
	my $layout_cell = get_nvpair ("LayoutCell");
	my $cast_path = get_nvpair ("CastPath");
	my $cast_cell = get_nvpair ("CastCell");
	my $cast_file = get_nvpair ("CastFile");

	if (is_defined ($stage_one) && is_defined ($layout_cell)) {

		info ("Performing stage-one hacks");

		if (!is_defined ($cast_cell)) {
			$cast_cell = $layout_cell;
			$cast_cell =~ s/(.*)-//;
			my $module = $1;
			$cast_cell =~ s/##2d/\(/g;
			$cast_cell =~ s/##2e/\)/g;
			add_nvpair ("CastCell", $cast_cell, "set", $sig);
		}
		if (!is_defined ($cast_file)) {

			#
			# Run Mike T's find_cell script
			#

			chomp ($cast_file = 
                               `find_cell $cast_cell 2> /dev/null`);

			#
			# If more than one is found, then add a module
			# specifier.  (Note that this is _really_ a hack.)
			#

			if ($cast_file =~ /\n/) {
				chomp ($cast_file = 
				  `find_cell $module\/$cast_cell 2> /dev/null`);
			}

			if (!($cast_file =~ /\S/)) {
				error ("Stage-one hack was unable to find the",
                                       "value of CastFile through divination.",
				       "Specify CastFile manually.");
			}

			#
			# Keep only final module/file.cast text
			#

			$cast_file =~ s/.*\/([^\/]+)\/([^\/]+)/$1\/$2/;
	
			add_nvpair ("CastFile", $cast_file, "set", $sig);
		}
	}
}
