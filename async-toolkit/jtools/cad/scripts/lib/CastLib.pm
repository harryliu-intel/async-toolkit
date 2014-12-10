#
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id: CastLib.pm,v 1.07 2002/01/24 12:00:00 auto-migrate Exp $
#

package		CastLib;
require		Exporter;

use Graph;
use CastParser;
use Parse::RecDescent;
use strict;

my %file_parse_cache;
my %cell_parse_cache;
my $last_castver = 0;
my $all_parsed = 0;

my $DEBUG = 0;
my $DEBUGCELL = "";

# THIS IS OUT OF DATE, BUT SOMEWHAT CORRECT
#
# When parsing cast files, we wish to build the following data structures:
#
# % {
#   cell_name => % {
#     defined_in => file defined in
#     subcells => @(list of non-flattened instantiated subcells)
#     need_to_import => @(list of all instantiated subcells)
#     blocks => % {
#       block_name => "block contents"	// Name can be env.numberal.subcells
#     }
#   }
# }
#
# @cells => @(list of defined cells in order defined)
# header => "header text (everything before imports)"
# footer => "header text (everything before imports)"
#
# While parsing in the cells, if a duplicated cell name is discovered, all 
# subsequent definitions of that cell will be renamed to __###__CELLNAME
# where ### is a number starting at 2 and incremented for each duplicate
# definition  (So you would have CELL, __2__CELL, __3__CELL, etc)

sub print_header
{
	my $str = shift;

	$str = " *** " . $str . " *** ";
	my $length = length $str;

	my $leading = int(40 - ($length/2));
	my $trailing = 80 - $length - $leading;

	$str = ("-" x $leading) . $str . ("-" x $trailing);

	print "\n$str\n\n";
}

sub set_debug
{
	$DEBUG = shift;
	$DEBUGCELL = shift;
}

my %get_full_dir_path_cache;

sub get_full_dir_path
{
	my $dirname = shift;

	return $get_full_dir_path_cache{$dirname} if( defined $get_full_dir_path_cache{$dirname} );

	print STDERR "    look for $dirname\n" if( $DEBUG > 2 );

	$dirname =~ s{ ^ ~ ( [^/]* ) }
					 { $1
						? (getpwnam($1))[7]
						: ( $ENV{HOME} || $ENV{LOGDIR}
							|| (getpwuid($>))[7]
						  )
	}ex;

	return if( not -d $dirname );
	print STDERR "    dirname found: $dirname\n" if( $DEBUG > 2 );
	$dirname = `cd $dirname ; pwd`;
	chomp $dirname;
	print STDERR "    dirname: $dirname\n" if( $DEBUG > 2 );
	$get_full_dir_path_cache{$dirname} = $dirname;
	return $dirname;
}

my %get_full_file_path_cache;

sub get_full_file_path
{
	my $filename = shift;

	return $get_full_file_path_cache{$filename} if( defined $get_full_file_path_cache{$filename} );

	print STDERR "Expanding $filename\n" if( $DEBUG );
	my $dirname = `dirname \"$filename\"`;
	chomp $dirname;
	print STDERR "  dirname: $dirname\n" if( $DEBUG > 2 );
	$filename =~ s/$dirname[\/]*//;
	print STDERR "  filename: $filename\n" if( $DEBUG > 2 );
	$dirname = get_full_dir_path($dirname);
	print STDERR "  dirname: $dirname\n" if( $DEBUG > 2 );
	if( defined $dirname )
	{
		my $fullfile = "$dirname" . "/" . $filename;
		print STDERR "Expands to: $fullfile\n" if( $DEBUG );
		$get_full_file_path_cache{$filename} = $fullfile;
		return $fullfile; 
	}
	else
	{
		print STDERR "Expands to NULL\n" if( $DEBUG );
		return ;
	}
}



# Returns list of subcells 
sub parse_prs_subcells 
{
	my $subcells_block = shift;
	my $local_debug = shift;

    open TMPFILE, ">tmp_$$" or die "Could not open tmp_$$ for writing";
    print TMPFILE "$subcells_block\n";
    close TMPFILE;
        
	my $cell_contents = `cat tmp_$$ | cpp -P | grep flatten`;
    unlink "tmp_$$";

	my @lines = split ";",$cell_contents;

	my %subcells = ();
	foreach my $line (@lines)
	{
		if( $line =~ /flatten\s+([a-zA-Z0-9_]+)/ )
		{
			$subcells{$1} = "";
			print "FOUND subcell $1\n" if( $DEBUG > 2 and $local_debug );
		}
	}
	return sort keys %subcells;
}

# This function returns the contents of the specified block in the specified cell 
sub _parse_cast_file_castv2 
{
    my $castfile = shift;
	my $relative_file = shift;

    print STDERR "START PARSING $castfile\n" if ($DEBUG > 3);

	$castfile = get_full_file_path($castfile);

	my %cells;
	my @cell_list;

	my @block_list = ("none");
	my %blocks_found = ();
	my $level = 0;
	my $last_block_level = 2;
	my $parenlevel = 0;
	my $incomment = 0;
	my $incell = 0;
	my $indef = 0;
	my $inblock = 0;
	my $first_define = 0;
	my $curcell = "";
	my $curblock = "none";
	my $lastblock = "none";
	my $cellbuf = "";
	my $letterbuf = "";
	my $last_word = "";
	my $header = "";
	my $char = "";
    open CAST, "$castfile" or die "Could not open cast file $castfile";
	until (eof CAST)
	{
		read(CAST, $char, 1) == 1 or die "Error reading from $castfile";

		$cellbuf .= $char;
		$cells{$curcell}{blocks}{def} .= $char if( $indef );
		if( $inblock and ( not $char eq "{" or $parenlevel > 0 ) )
		{
			$cells{$curcell}{blocks}{$curblock} .= $char; 
		}
		if( $inblock and ($level > 1 or not $char eq "{") )
		{
			$curblock =~ /^([^\.]+)/;
			my $cur_top_block = $1;
			$cells{$curcell}{topblocks}{$cur_top_block} .= $char;
		}

		if( not $incomment )
		{
			$cellbuf = "" if( $char eq "}" );
			$level++ if( $char eq "{" );
			$level-- if( $char eq "}" );

			$parenlevel++ if( $char eq "(" );
			$parenlevel-- if( $char eq ")" );
	
	
			if( not $incell and $indef and $level == 1 )
			{
                $curblock = "none";
                print STDERR "ENTERED CELL: $curcell\n" if( $DEBUG > 3 );
				push @cell_list, $curcell;
				$incell = 1;
				$indef = 0;
				$cells{$curcell}{blocks}{def} = "(" . $cells{$curcell}{blocks}{def};
				$cells{$curcell}{blocks}{def} =~ s/\s*{\s*$//;
				chomp $cells{$curcell}{blocks}{def};
			}
			if( $incell and $level == 0 )
			{
                print STDERR "ABOUT TO PARSE SUBCELLS: $curcell\n" if( $DEBUG > 3);
				$incell = 0;
				my $subcells = $cells{$curcell}{blocks}{subcells};
				@{$cells{$curcell}{subcells}} = CastParser::parse_subcells($subcells,0,($DEBUG and $curcell eq $DEBUGCELL));
				@{$cells{$curcell}{need_to_import}} = CastParser::parse_subcells($subcells,1,($DEBUG and $curcell eq $DEBUGCELL));
				push @{$cells{$curcell}{need_to_import}}, parse_prs_subcells($cells{$curcell}{blocks}{prs},($DEBUG and $curcell eq $DEBUGCELL));
			}

			# Have we just entered a block?
			if( $incell and $char eq "{" and $parenlevel == 0 )
			{
				if( not $last_word =~ /^\s*$/s or not $letterbuf =~ /^\s*$/s )
				{
					$lastblock = $curblock;
					push @block_list, $curblock;

					$curblock = $last_word;
					$curblock = $letterbuf if( not $letterbuf =~ /^\s*$/s );
					$curblock =~ s/^\s*//;
					$curblock =~ s/\s*$//;
					$curblock =~ s/\s+/./g;

					$cells{$curcell}{blocks}{$lastblock} =~ s/\s*$curblock\s*$//;

					print STDERR "CELL $curcell BLOCK found: \"$curblock\"\n" if( $DEBUG > 1 and $curcell eq $DEBUGCELL );
					print STDERR "  lastblock: $lastblock\n" if( $DEBUG > 1 and $curcell eq $DEBUGCELL );
					$inblock = 1;
					$curblock = $lastblock . "." . $curblock if( not $lastblock eq "none" );
					print STDERR "NEED TO ADD \"$curblock\" to block_order\n" if( $DEBUG and $curcell eq $DEBUGCELL );
					my @skipblocks = split '\.', $curblock;
					for( my $i = 0; $i < @skipblocks; $i++ )
					{
						my $tempblock = "";
						for( my $j = 0; $j <= $i; $j++ )
						{
							$tempblock .= ("." . $skipblocks[$j]);
						}
						print STDERR "*** ADDING BLOCK \"$tempblock\" to block_order\n" if( $DEBUG and $DEBUGCELL eq $curcell );
						push @{$cells{$curcell}{block_order}}, $tempblock;
					}
					$last_block_level = $level;
					print STDERR "*** CELL $curcell entered BLOCK $curblock\n" if( $DEBUG and $curcell eq $DEBUGCELL );
				}
			}
		}
		
		# Should we add the current character to the "none" buffer?
		# In general, we should add it if we are not in a block, but we are
		# within a cell definition.  The exception is that we do not want to
		# add the "{" or "}" characters used to get into those states

		if( not $inblock and $incell )
		{
			# If it is a "{" and we are at level 1, then that means that this
			# was the brace that put us in the cell def, so ignore it

			if( not( $char eq "{" and $level == 1) or $incomment )
			{
				$cells{$curcell}{blocks}{none} .= $char; 
			}	
		}

		if( not $incomment )
		{
			# Just left a block?
			if( $inblock and $level < $last_block_level )
			{
				$cells{$curcell}{blocks}{$curblock} =~ s/^\s*//s;
				$cells{$curcell}{blocks}{$curblock} =~ s/\s*}\s*$//;
				$cells{$curcell}{blocks}{$curblock} =~ s/\s*$//s;
				if( $level == 1 )
				{
					$cells{$curcell}{topblocks}{$curblock} =~ s/\s*}\s*$//;
				}
				$curblock = pop @block_list;
#				$curblock = pop @block_list if( $level > 1 );
#				$curblock = "none" if( $level == 1 );
				$last_block_level = $level;
				print STDERR "*** CELL $curcell moved to block $curblock\n" if( $DEBUG and $curcell eq $DEBUGCELL and $level > 1 );
				print STDERR "*** CELL $curcell no longer in block\n" if( $DEBUG and $curcell eq $DEBUGCELL and $level == 1 );
				$inblock = 0 if( $level == 1 );
				if( not $parenlevel == 0 )
				{
					print "** SYNTAX ERROR IN CELL $curcell block $curblock: mismatched ()\n";
					$parenlevel = 0;
				}	
			}
		}

		if( not $incomment and $cellbuf =~ /import\s*\"[^"]*\";$/ )
		{
			$header .= $cellbuf if( not $first_define );
			$cellbuf = "";
		}

		# Here we check to see if we have found a cell definition...
		# If so, then our current buffer contains everything that was
		# found before this definition and after the previously defined cell
		# If this is the first cell, then this is the header of the file,
		# Otherwise it is the "before" block for the cell.

        if( not $incomment and $cellbuf =~ /(define|defchan|defspec)[\s]+([a-zA-Z0-9_]+)[\s]*\($/s and not $indef)
        {
			$indef = 1;
			$curcell = $2;
			$curcell =~ s/\s//g;
            print STDERR "FOUND CELL DEF: $curcell\n" if( $DEBUG > 3 );

			$cells{$curcell} = ();
			$cells{$curcell}{defined_in} = $relative_file;
			if( not $first_define )
			{
				$header .= $cellbuf;
				$header =~ s/define[\s]+$curcell[\s]*\($//;
				$first_define = 1;
			}
			my $before = $cellbuf;
			$before =~ s/define\s*$curcell\s*\($//;
			$cells{$curcell}{blocks}{before} = $before;
		}

		if( not $incomment )
		{
			if( $char =~ /[a-zA-Z 0-9_]/ )
			{
				$letterbuf .= $char;
			}
#			elsif( $char =~ /\s/s )
			else
			{
				$last_word = $letterbuf if( not $letterbuf =~ /^\s*$/s and not $letterbuf =~ /^[0-9]/ );
				$letterbuf = "";
			}
			if( $char =~ /[^a-zA-Z\/\*\s]/s )
			{
				$last_word = "";
			}
		}

		$incomment = 2 if( not $incomment and $cellbuf =~ /\/\/$/ );
		$incomment = 1 if( not $incomment and $cellbuf =~ /\/\*$/ );
		$incomment = 0 if( $incomment == 1 and $cellbuf =~ /\*\/$/ );
		$incomment = 0 if( $incomment == 2 and $char eq "\n" );
#		$letterbuf = "" if( $incomment );
	}

	close CAST;

	print STDERR "SYNTAX ERROR file $castfile: mismatched {}\n" if( $level != 0 );

	my $footer = $cellbuf;

#	print "**********************************************\n";
#	print "HEADER: \"$header\"\n";
	my @my_imports = ();
	$header =~ s/(import\s+[^;]+;.*)//s;
	my $imports = $1;

#	print "PRE-IMPORTS: \"$imports\"\n";
    print STDERR "Mucking with imports\n" if($DEBUG > 3);
	if( defined $imports )
	{
        open TMPFILE, ">tmp_$$" or die "Could not open tmp_$$ for writing";
        print TMPFILE "$imports\n";
        close TMPFILE;
        
		$imports = `cat tmp_$$ | cpp -P`;
        unlink "tmp_$$";
		my @lines = split '\n',$imports;
		foreach my $line (@lines)
		{
			next if( $line =~ /^\s*$/s );
			$line =~ /import\s+([^;]+)/;
			push @my_imports,$1;
		}
	}
#	print "IMPORTS: @my_imports\n";

#	print "cell_list: @cell_list\n";

	$header =~ /module[\s]+([^;]+);/;
	my $module = $1;
	$header =~ s/module[\s]+([^;]+);//;

	my %cast_data;
	@{$cast_data{imports}} = @my_imports;
	$cast_data{header} = $header;
	$cast_data{footer} = $footer;
	$cast_data{module} = $module;
	%{$cast_data{cells}} = %cells;
	@{$cast_data{cell_list}} = @cell_list;

    print STDERR "DONE PARSING CASTFILE $castfile\n" if($DEBUG > 3);
	return \%cast_data;
}
# This function returns the contents of the specified block in the specified cell 
sub _parse_cast_file_castv1 
{
    my $castfile = shift;
	my $relative_file = shift;

	$castfile = get_full_file_path($castfile);

	my %cells;
	my @cell_list;

	my @blocks_found = ();
	my $level = 0;
	my $parenlevel = 0;
	my $incomment = 0;
	my $incell = 0;
	my $indef = 0;
	my $inblock = 0;
	my $first_define = 0;
	my $curcell = "";
	my $curblock = "";
	my $cellbuf = "";
	my $letterbuf = "";
	my $last_word = "";
	my $header = "";
	my $char = "";
    open CAST, "$castfile" or die "Could not open cast file $castfile";
	until (eof CAST )
	{
		read(CAST, $char, 1) == 1 or die "Error reading from $castfile";


		$cellbuf .= $char;
		$cells{$curcell}{blocks}{def} .= $char if( $indef );
		$cells{$curcell}{blocks}{$curblock} .= $char if( $inblock );

#		$cells{$curcell}{blocks}{none} .= $char if( not $inblock and $incell );	

		if( not $incomment )
		{
			$cellbuf = "" if( $char eq "}" );
			$level++ if( $char eq "{" );
			$level-- if( $char eq "}" );

			$parenlevel++ if( $char eq "(" );
			$parenlevel-- if( $char eq ")" );
	
	
			if( not $incell and $indef and $level == 1 )
			{
				push @cell_list, $curcell;
				$incell = 1;
				$indef = 0;
				$cells{$curcell}{blocks}{def} = "(" . $cells{$curcell}{blocks}{def};
				$cells{$curcell}{blocks}{def} =~ s/\s*{\s*$//;
				chomp $cells{$curcell}{blocks}{def};
			}
			# Have we just left a cell?
			if( $incell and $level == 0 )
			{
				$incell = 0;

				foreach my $blockname (@blocks_found)
				{
					$cells{$curcell}{blocks}{none} =~ s/[ \t]*${blockname}\s*//g;
				}
				@blocks_found = ();
				$cells{$curcell}{blocks}{none} =~ s/^\s*//s;
				$cells{$curcell}{blocks}{none} =~ s/\s*$//s;
				@{$cells{$curcell}{subcells}} = CastParser::parse_subcells($cells{$curcell}{blocks}{none},0,($DEBUG and $curcell eq $DEBUGCELL));
				@{$cells{$curcell}{need_to_import}} = CastParser::parse_subcells($cells{$curcell}{blocks}{none},1,($DEBUG and $curcell eq $DEBUGCELL));
			}

			# Have we just entered a block?
			if( $incell and $level == 2 and $char eq "{" and $parenlevel == 0 )
			{
#				print "CELL $curcell BLOCK?  $last_word __ $letterbuf\n";
				if( not $last_word =~ /^\s*$/s or not $letterbuf =~ /^\s*$/s )
				{
					$curblock = $last_word;
					$curblock = $letterbuf if( not $letterbuf =~ /^\s*$/s );
#					print "*** CELL $curcell entered BLOCK $curblock ($last_word __ $letterbuf)\n";
					$inblock = 1;
					push @blocks_found,$curblock;
				}
			}
		}
		
		# Should we add the current character to the "none" buffer?
		# In general, we should add it if we are not in a block, but we are
		# within a cell definition.  The exception is that we do not want to
		# add the "{" or "}" characters used to get into those states

		if( not $inblock and $incell )
		{
			# If it is a "{" and we are at level 1, then that means that this
			# was the brace that put us in the cell def, so ignore it

			if( not( $char eq "{" and $level == 1) or $incomment )
			{
				$cells{$curcell}{blocks}{none} .= $char if( not $inblock and $incell );
			}	
		}

		if( not $incomment )
		{
			# Just left a block?
			if( $inblock and $level == 1 )
			{
				$inblock = 0;
				if( not $parenlevel == 0 )
				{
					print "** SYNTAX ERROR IN CELL $curcell block $curblock: mismatched ()\n";
					$parenlevel = 0;
				}	
				$cells{$curcell}{blocks}{$curblock} =~ s/^\s*//s;
				$cells{$curcell}{blocks}{$curblock} =~ s/\s*}\s*$//;
				$cells{$curcell}{blocks}{$curblock} =~ s/\s*$//s;
			}
		}

		if( not $incomment and $cellbuf =~ /import\s*\"[^"]*\";$/ )
		{
			$header .= $cellbuf if( not $first_define );
			$cellbuf = "";
		}

		# Here we check to see if we have found a cell definition...
		# If so, then our current buffer contains everything that was
		# found before this definition and after the previously defined cell
		# If this is the first cell, then this is the header of the file,
		# Otherwise it is the "before" block for the cell.

        if( not $incomment and $cellbuf =~ /define[\s]+([a-zA-Z0-9_]+)[\s]*\($/s and not $indef)
        {
			$indef = 1;
			$curcell = $1;
			$curcell =~ s/\s//g;

			$cells{$curcell} = ();
			$cells{$curcell}{defined_in} = $relative_file;
			if( not $first_define )
			{
				$header .= $cellbuf;
				$header =~ s/define[\s]+$curcell[\s]*\($//;
				$first_define = 1;
			}
			my $before = $cellbuf;
			$before =~ s/define\s*$curcell\s*\($//;
			$cells{$curcell}{blocks}{before} = $before;
		}

		if( not $incomment )
		{
			if( $char =~ /[a-zA-Z]/ )
			{
				$letterbuf .= $char;
			}
#			elsif( $char =~ /\s/s )
			else
			{
				$last_word = $letterbuf if( not $letterbuf =~ /^\s*$/s );
				$letterbuf = "";
			}
			if( $char =~ /[^a-zA-Z\/\*\s]/s )
			{
				$last_word = "";
			}
		}

		$incomment = 2 if( not $incomment and $cellbuf =~ /\/\/$/ );
		$incomment = 1 if( not $incomment and $cellbuf =~ /\/\*$/ );
		$incomment = 0 if( $incomment == 1 and $cellbuf =~ /\*\/$/ );
		$incomment = 0 if( $incomment == 2 and $char eq "\n" );
#		$letterbuf = "" if( $incomment );
	}

	close CAST;

	my $footer = $cellbuf;


	my @my_imports = ();
	$header =~ s/(import\s+\"[^"]+\";.*)//s;
	my $imports = $1;

	if( defined $imports )
	{
        open TMPFILE, ">tmp_$$" or die "Could not open tmp_$$ for writing";
        print TMPFILE "$imports\n";
        close TMPFILE;
		$imports = `cat tmp_$$ | cpp -P`;
        unlink "tmp_$$";
		my @lines = split '\n',$imports;
		foreach my $line (@lines)
		{
			next if( $line =~ /^\s*$/s );
			$line =~ /import\s+([^;]+)/;
			push @my_imports,$1;
		}
	}

#	print "cell_list: @cell_list\n";

	my %cast_data;
	@{$cast_data{imports}} = @my_imports;
	$cast_data{header} = $header;
	$cast_data{footer} = $footer;
	%{$cast_data{cells}} = %cells;
	@{$cast_data{cell_list}} = @cell_list;

	return \%cast_data;
}

my %_get_full_path_with_rel_cache;

sub _get_full_path_with_rel
{
	my $rel = shift;

	return $_get_full_path_with_rel_cache{$rel} if( defined $_get_full_path_with_rel_cache{$rel} );

	my $CAST_PATH = get_cast_path();
	my @PATHS = split ':',$CAST_PATH;
	my @castfiles = ();
	foreach my $rootdir (@PATHS)
	{
		my $filename = $rootdir . "/" . $rel;
		my $fullname = get_full_file_path($filename);
		push @castfiles, $fullname if( -f $fullname );
	}
	if( @castfiles == 0 )
	{
		print STDERR "$rel not found in CAST_PATH: \"$CAST_PATH\"\n";
		return;
	}
	if( @castfiles > 1 )
	{
		print STDERR "More than one match for $rel found in CAST_PATH: \"$CAST_PATH\"\n";
		return;
	}
	$_get_full_path_with_rel_cache{$rel} = $castfiles[0];
	return $castfiles[0];
}

my %parse_cast_file_cache;

sub parse_cast_file
{
    my $castfile = shift;

	my $file = _get_full_path_with_rel($castfile);
	$file = $castfile if( not defined $file and -f $castfile );
    if( not defined $file )
    {
        print STDERR "Unable to parse cast file $castfile\n";
        return;
    }

	return $parse_cast_file_cache{$file} if( defined $parse_cast_file_cache{$file} );

	my $castver = 1;
	open CAST, "<$file" or die "Could not read from $file";
	while(<CAST>)
	{
		$castver = 2 if( /^module .*;/ );
	}
	close CAST;
	if( $castver != $last_castver and $last_castver != 0 )
	{
		print STDERR "Warning!  You are mixing castv1 and castv2 files!\n" if( $DEBUG > 1 );
		print STDERR "  $file is castv" . $castver . ", previous file was castv" . $last_castver . "\n" if( $DEBUG > 1 );
	}
	$last_castver = $castver;
	print "Parsing castv" . $castver . " file $file\n" if( $DEBUG > 1 );
	$file_parse_cache{$file} = _parse_cast_file_castv1($file,$castfile) 
      if( $castver == 1 );
	$file_parse_cache{$file} = _parse_cast_file_castv2($file,$castfile) 
      if( $castver == 2 );
	foreach my $cell ( keys %{$file_parse_cache{$file}{cells}})
	{
		my $fullname;
		if( $castver == 1 )
		{
			$fullname = $cell;
		}
		elsif( $castver == 2 )
		{
			my $module = $file_parse_cache{$file}{module};
			$fullname = $module . "." . $cell;
		}
		else
		{
			print "Found some wacky non-castv1 or castv2 file: $file\n";
		}
        print STDERR "  Found definition of $cell ($fullname)\n" if( $DEBUG );
		push @{$cell_parse_cache{$fullname}}, $file_parse_cache{$file}{cells}{$cell};
	}
	$parse_cast_file_cache{$file} = $file_parse_cache{$file};
	return $file_parse_cache{$file};
}

my $get_cast_path_cache;

sub set_cast_path
{
	$get_cast_path_cache = shift;
}

sub get_cast_path
{
	my $CAST_PATH = shift;

	if( not defined $CAST_PATH )
	{
		return $get_cast_path_cache if( defined $get_cast_path_cache );
		if( defined $ENV{CAST_PATH} )
		{
			$CAST_PATH = $ENV{CAST_PATH};
			print STDERR "Using env CAST_PATH: \"$CAST_PATH\"\n" if( $DEBUG );
		}
		else
		{
			$CAST_PATH = "~/hw/cast";
			print STDERR "Using default CAST_PATH: \"$CAST_PATH\"\n" if( $DEBUG );
		}
		$get_cast_path_cache = $CAST_PATH;
	}
	else
	{
		print STDERR "Using specified CAST_PATH: \"$CAST_PATH\"\n" if( $DEBUG );
	}
	return $CAST_PATH;
}

my %list_castfiles_cache;

sub list_castfiles
{
	my $castver = shift;

	return @{$list_castfiles_cache{$castver}} if( defined $list_castfiles_cache{$castver} );

	my @filelist = ();
	my $CAST_PATH = get_cast_path;	

	my @PATHS = split ':',$CAST_PATH;
	foreach my $curdir (@PATHS)
	{
#		$curdir = get_full_dir_path($curdir);
		$curdir = glob($curdir) if( not -d $curdir );
		if( not -d $curdir )
		{
			print STDERR "CAST_PATH directory $curdir does not exist\n";
		}
		else
		{
			open CASTFILES, "find $curdir -maxdepth 2 -name \"*.cast\" |" or die "Could not get listing of cast files" if( $castver == 1 );
			open CASTFILES, "find $curdir -name \"*.cast\" |" or die "Could not get listing of cast files" if( $castver == 2 );
			while(<CASTFILES>)
			{
				chomp $_;
				s/$curdir//;
				s/^\///;
				next if( /standard/ );
				push @filelist, $_;
			}
		}
	}
	@{$list_castfiles_cache{$castver}} = @filelist;
	return @filelist;
}

sub _decode_castv_from_cell
{
	my $cellname = shift;

	return "1" if( $cellname =~ /^[a-zA-Z0-9_]+$/ );
	return "2" if( $cellname =~ /^[a-zA-Z0-9_\.]+$/ );
	die "ERROR: malformed cellname \"$cellname\" (@{$cellname})\n";
}

my %find_cell_cache;

sub find_cell
{
	my $cellname = shift;
	my $full = shift;
	my @results = ();

	return @{$find_cell_cache{$cellname}} if( defined $find_cell_cache{$cellname} );

	my $castver = _decode_castv_from_cell($cellname);
	if( $castver == 2 )
	{
		my $filename = $cellname;
		$filename =~ s/\.[^\.]+$//;
		$filename =~ s/\./\//g;
		$filename .= ".cast";
		parse_cast_file($filename);
	}
	elsif( not $all_parsed )
	{
		foreach my $castfile (list_castfiles($castver))
		{
			parse_cast_file($castfile);
		}
		$all_parsed = 1;
	}
	foreach my $celldata (@{$cell_parse_cache{$cellname}})
	{
		push @results, $celldata->{defined_in};
	}
	@{$find_cell_cache{$cellname}} = @results;
	return @results if( not $full );
	my @full_results;
	foreach my $file (@results)
	{
		push @full_results, _get_full_path_with_rel($file);
	}
	return @full_results;
}

my %parse_cast_cell_cache;

sub parse_cast_cell
{
	my $cellname = shift;

	return $parse_cast_cell_cache{$cellname} if( defined $parse_cast_cell_cache{$cellname} );

    print STDERR "Parsing cell $cellname\n" if( $DEBUG );
	my @filenames = ();
	my $castver = _decode_castv_from_cell($cellname);
	@filenames = find_cell($cellname) if( $castver == 1 );
	if( $castver == 2 )
	{
		my $filename = $cellname;
		$filename =~ s/\.[^\.]+$//;
		$filename =~ s/\./\//g;
		$filename .= ".cast";
		push @filenames, $filename;
	}
	if( @filenames == 0 )
	{
		print STDERR "$cellname is undefined\n";
		return;
	}
	elsif( @filenames > 1 )
	{
		print STDERR "$cellname has multiple definitions:\n";
		foreach my $filename (@filenames)
		{
			print STDERR "  $filename\n";
		}
		return;
	}
	else
	{
		parse_cast_file($filenames[0]);
		$parse_cast_cell_cache{$cellname} = $cell_parse_cache{$cellname}[0];
        print STDERR "Parsed empty cell $cellname???\n" if( not defined
            $cell_parse_cache{$cellname}[0] );
		return $cell_parse_cache{$cellname}[0];
	}
}

my %has_block_cache;

sub has_block
{
	my $cellname = shift;
	my $blockname = shift;

	return $has_block_cache{$cellname}{$blockname} if( defined $has_block_cache{$cellname}{$blockname} );

	my $celldata = parse_cast_cell($cellname);
	if( not defined $celldata )
	{
		print STDERR "$cellname does not exist\n";
		return;
	}

	my $result = (exists $celldata->{blocks}{$blockname});
	$has_block_cache{$cellname}{$blockname} = $result;
	return $result;	
}

my %is_fragment_cache;

sub is_fragment
{
	my $cellname = shift;

	return $is_fragment_cache{$cellname} if( defined $is_fragment_cache{$cellname} );

	my $celldata = parse_cast_cell($cellname);

	if( defined $celldata )
	{
		my $result = 0;
		$result = 1 if( $celldata->{blocks}{def} =~ /\s+node\s+/ );
		$is_fragment_cache{$cellname} = $result;
		return $result;
	}
	else
	{
		print STDERR "$cellname does not exist\n";
		return;
	}
}

my %is_leaf_cache;

sub is_leaf
{
	my $cellname = shift;
	my $fragment = shift;
	
	$fragment = 1 if( not defined $fragment );

	return $is_leaf_cache{$fragment}{$cellname} if( defined $is_leaf_cache{$fragment}{$cellname} );

	my $result = 1;
    print "Checking $cellname...";
    print "fragment\n" if( is_fragment($cellname));
    print "\n" if( not is_fragment($cellname));
    print "---------------------------------------------------------------\n";
    print get_block_contents($cellname,"prs");
    print "\n---------------------------------------------------------------\n";
    print get_block_contents($cellname,"subcells");
    print "\n===============================================================\n";
    print "\n";
	$result = 0 if( not $fragment and is_fragment($cellname) );
	$result = 0 if( not get_block_contents($cellname,"prs"));
	$result = 0 if( get_block_contents($cellname,"subcells"));
	$is_leaf_cache{$fragment}{$cellname} = $result;
	return $result;
}

my %is_byte_cache;

sub is_byte
{
	my $cellname = shift;
	my $fragment = shift;

	$fragment = 1 if( not defined $fragment );
	
	return $is_byte_cache{$fragment}{$cellname} if( defined $is_byte_cache{$fragment}{$cellname} );

	my $result = 1;
	$result = 0 if( not $fragment and is_fragment($cellname) );
	$result = 0 if( not get_block_contents($cellname,"prs"));
	$result = 0 if( not get_block_contents($cellname,"subcells"));
	$is_byte_cache{$fragment}{$cellname} = $result;
	return $result;
}

my %is_mid_cache;

sub is_mid
{
	my $cellname = shift;

	return $is_mid_cache{$cellname} if( defined $is_mid_cache{$cellname} );

	my $result = 1;
	$result = 0 if( get_block_contents($cellname,"prs"));
	$result = 0 if( not get_block_contents($cellname,"subcells"));
	$is_mid_cache{$cellname} = $result;
	return $result;
}

my @list_all_cells_cache;

sub list_all_cells
{
	my $castver = shift;

	$castver = "2" if( not defined $castver );
	return @list_all_cells_cache if( @list_all_cells_cache );

	foreach my $filename (list_castfiles($castver))
	{
        print "Parsing $filename\n" if( $DEBUG > 2 );
		parse_cast_file($filename);
	}
	@list_all_cells_cache = keys %cell_parse_cache;
	return sort @list_all_cells_cache;	
}

my %get_block_contents_cache;

sub get_block_contents
{
	my $cellname = shift;
	my $block = shift;

	return $get_block_contents_cache{$cellname}{$block} if( defined $get_block_contents_cache{$cellname}{$block} );

	my $celldata = parse_cast_cell($cellname);
	return if( not defined $celldata );

	return if( not defined $celldata->{blocks}{$block} and not defined $celldata->{topblocks}{$block} );

	my $result = $celldata->{blocks}{$block};
	$result = $celldata->{topblocks}{$block} if( defined $celldata->{topblocks}{$block} );

	$get_block_contents_cache{$cellname}{$block} = $result;
	return $result;
}

sub _format_lines
{
	my $level = shift;
	my @lines = @_;

	my $output = "";
	foreach my $line (@lines)
	{
		$line =~ s/\t/  /g;
		$line =~ /^(\s*)/;
		my $spaces = $1;

		$output .= " " x (($level*2) - length $spaces) if( length $spaces < ($level*2) );
		$output .= $line;
		$output .= "\n";
	}
	return $output;
}

sub _format_block
{
	my $blockname = shift;
	my $contents = shift;
	my @lines = @_;

	my @levels = split '\.',$blockname;
	my $indent = (@levels+1);

	if( @lines == 1 and not $contents =~ /\/\// )
	{
		return "  $blockname { $contents }\n\n";
	}
	else
	{
		my $output = _format_lines($indent,@lines);
		return "  $blockname {\n$output  }\n";
	}
}

# This takes a string containing a csp block, and formats it for output
sub format_block
{
	my $blockname = shift;
	my $contents = shift;

	$contents =~ s/^\s*//s;
	chomp $contents;

	my @lines = split '\n',$contents;
	return _format_block($blockname,$contents,@lines);
}

my %blocks_processed = ();

sub _process_templated_block
{
	my $curblock = shift;
	my $celldata = shift;
	my $template = shift;
	my $template_data = shift;
	my $level = shift;

	my $subcontents = "";
	my $num_subblocks = 0;

	$blocks_processed{$curblock} = 1;

	foreach my $temp_block (@{$template_data->{block_order}})
	{
		if( $temp_block =~ /^($curblock\.[^\.]+)$/ )
		{
			my $subblock = $1;
			my $_subcontents = _process_templated_block($subblock,$celldata,$template,$template_data,$level+1) if( not defined $blocks_processed{$subblock} );
			if( $_subcontents ne "" )
			{
				$subcontents .= $_subcontents;
				$num_subblocks++;
			}
			$subcontents .= _process_templated_block($subblock,$celldata,$template,$template_data,$level+1) if( not defined $blocks_processed{$subblock} );
		}
	}
	my $index_block = $curblock;
	$index_block =~ s/^\.//;
	my $baby_block = $index_block;
	$baby_block =~ s/.*\.([^\.]+)$/$1/;
	$index_block = "none" if( $index_block eq "" );

	my $contents = "";
	my $tempcontents = $template_data->{blocks}{$index_block};
	my $cellcontents = $celldata->{blocks}{$index_block};

	my @templines = split '\n',$tempcontents;
	my @celllines = split '\n',$cellcontents;
	my $newtemp = "";
	foreach my $templine (@templines)
	{
		my $found = 0;
		foreach my $cellline (@celllines)
		{
			$cellline =~ s/^\s*//;
			$cellline =~ s/\s*$//;
			$cellline =~ s/[ \t]+/ /g;

			my $cmp_temp = $templine;
			$cmp_temp =~ s/^\s*//;
			$cmp_temp =~ s/\s*$//;
			$cmp_temp =~ s/[ \t]+/ /;

			$found = 1 if( $cmp_temp eq $cellline );
		}
		$newtemp .= ($templine . "\n") if( not $found );
	}
	chomp $newtemp;

	$contents .= _format_lines($level,split ('\n',$newtemp)) if( not $newtemp eq "" );
	$contents .= _format_lines($level,split ('\n',$cellcontents)) if( not $cellcontents eq "" );
	$contents .= $subcontents if( not $subcontents eq "" );

	return if( $contents eq "" and not defined $celldata->{blocks}{$index_block});
	if( not $baby_block eq "" )
	{
		my $leading_white = " " x (($level-1)*2);
		if( $num_subblocks == 1 and $cellcontents eq "" and $tempcontents eq "" )		# SQUISHINESS!!!
		{
			$contents =~ s/^\s*//;			# Remove leading whitespace
			$contents =~ s/\s*}\s*$//;		# Remove trailing } and whitespace
			$contents =~ s/\n  /\n/g;		# Bring in 2 spaces
			$contents = ($leading_white . "$baby_block " . $contents . "\n" . $leading_white . "}\n");
		}
		else
		{
			$contents = ($leading_white . "$baby_block {\n" . $contents . $leading_white . "}\n");
		}
	}
	return $contents;
}

sub format_celldata_with_template 
{
	my $celldata = shift;
	my $template = shift;

	return if( not is_exist($template) );

	my $template_data = parse_cast_cell($template);

	foreach my $cell_block (keys %{$celldata->{blocks}})
	{
		next if( $cell_block eq "none" or $cell_block eq "def" or $cell_block eq "before" );
		my $found = 0;
		foreach my $temp_block (@{$template_data->{block_order}})
		{
			$found = 1 if( ".$cell_block" eq $temp_block );
		}
		die "Fatal error: template $template does not match celldata (missing block $cell_block)" if( not $found )
	}

	%blocks_processed = ();
	return _process_templated_block("",$celldata,$template,$template_data,1);
}

sub format_cell_with_template 
{
	my $cellname = shift;
	my $template = shift;

	return if( not is_exist($cellname) );
	return if( not is_exist($template) );

	my $celldata = parse_cast_cell($cellname);
	my $template_data = parse_cast_cell($template);

	foreach my $cell_block (keys %{$celldata->{blocks}})
	{
		next if( $cell_block eq "none" or $cell_block eq "def" or $cell_block eq "before" );
		my $found = 0;
		foreach my $temp_block (@{$template_data->{block_order}})
		{
			$found = 1 if( ".$cell_block" eq $temp_block );
		}
		die "Fatal error: template $template does not match cell $cellname (missing block $cell_block)" if( not $found )
	}

	%blocks_processed = ();
	return _process_templated_block("",$celldata,$template,$template_data,1);
}

sub is_exist
{
	my $cellname = shift;

	my $celldata = parse_cast_cell($cellname);
	return 0 if( not defined $celldata );
	return 1;
}

sub list_valid_instantiations
{
	my $cellname = shift;

	return if( not is_exist($cellname) );

	my $nicename = $cellname;
	$nicename =~ s/[\.\,\[\]\(\)]/_/g;

	my @instances;
	if( list_metaparams($cellname) eq "" )
	{
		push @instances, "$cellname $nicename;";
		return @instances;
	}

	# FIXME: Doing this now because of some bug
#	return if( list_metaparams($cellname) );

	my @valid_metaparams = list_valid_metaparams($cellname);
	my $valid = 0;
	foreach my $metaparam(@valid_metaparams)
	{
		if( $metaparam =~ /[^0-9\,\{\}]/ )
		{
			print STDERR "Discarding unknown metaparams ($metaparam) for cell $cellname\n" if( $DEBUG );
		}
		elsif( $metaparam ne "" )
		{
			$valid = 1;
			my $nice_metaparam = $metaparam;
			$nice_metaparam =~ s/[\.\,\[\]\(\)\{\}]/_/g;
			push @instances, "$cellname($metaparam) ${nicename}_${nice_metaparam}_;";	
		}
	}
	if( not $valid )
	{
		print STDERR "Unable to determine any valid uses of $cellname metaparams: (" . list_metaparams($cellname) . ")\n" if( $DEBUG );
	}
	return @instances;
}

sub list_metaparams
{
	my $cellname = shift;

	return if( not is_exist($cellname) );
	my $def = get_block_contents($cellname,"def");
	if( $def =~ /\(([^)]+)\)\s*\(.*/ )
	{
		return $1;
	}
}

sub list_valid_metaparams
{
	my $cellname = shift;

	return if( not is_exist($cellname) );
	if( not list_metaparams($cellname) )
	{
#		print STDERR "$cellname does not have metaparams\n";	
		return ();
	}

	my @parents = list_parents($cellname);
    print "\n\n";
	my %uses;
	foreach my $parent (@parents)
	{
#		print STDERR "Looking for use of $cellname in $parent\n";
		foreach my $instance (list_subcells($parent,1,$cellname))
		{
#			print STDERR "  $instance\n";
			if(	$instance =~ /\((.*)\)/ )
			{
				my $metaparams = $1;
				$uses{$metaparams} = "";
			}
			else
			{
#				print STDERR "Unknown metaparam error on $cellname\n";
#                print STDERR "  parent cell: $parent\n";
#                print STDERR "  instance: $instance\n";
			}
		}
	}	
	return sort keys %uses;
}

my %parents_cache;

# FIXME: Currently does not care about what you are importing
sub list_parents
{
	my $cellname = shift;

	return if( not is_exist($cellname) );

	return @{$parents_cache{$cellname}} if( defined $parents_cache{$cellname} );

#    print STDERR "Looking for parents of $cellname\n";

	my @allcells = list_all_cells("2");

	my @parents;
	foreach my $cell(@allcells)
	{
		my @subcells = list_subcells($cell,0,$cellname);
#        print STDERR "  $cell\n" if(@subcells > 0);
		push @parents, $cell if(@subcells > 0);
	}
	@{$parents_cache{$cellname}} = @parents;
	return @{$parents_cache{$cellname}};	
}

sub list_leaves 
{
	my $cellname = shift;

	return if( not is_exist($cellname) );
	my @process_me;
	push @process_me, $cellname;
	my %already_processed;
	my @leaves;
	while( my $cell = shift @process_me )
	{
        $already_processed{$cell} = 1;
        if( is_leaf($cell) )
        {
			push @leaves, $cell;
        }
        else
        {
            foreach my $subcell (list_subcells_fullname($cell))
            {
                push @process_me, $subcell;
            }
        }
	}	
	return @leaves;
}

sub list_subcells_fullname
{
	my $cellname = shift;
	return if( not is_exist($cellname) );
	my %subcells = ();
	parse_cast_cell($cellname);
	my @filename = find_cell($cellname,1);
	my @imports = @{$file_parse_cache{$filename[0]}->{imports}};
	my @types = list_subcells($cellname);

	foreach my $type (@types)
	{
		my $found = 0;
		foreach my $import (@imports)
		{
			if( $import =~ /\.$type$/ )
			{
				$subcells{$import} = "";
				$found = 1;
			}
		}
		if( not $found )
		{
			my @files = find_cell($cellname,0);
			my $full = $files[0];
			$full =~ s/\//\./g;
			$full =~ s/\.cast//;
			$full .= ".$type";
			$subcells{$full} = "";
		}
	}
	return sort keys %subcells;
}

# If flatten is TRUE, then obey flatten keyword
# If flatten is FALSE, ignore flatten keyword
# FIXME: Does not work if more than one BUF_1of4, for example
# FIXME: If inline keyword, expand.  If flatten give error
# If fullname is FALSE or undefined, return short name
sub list_subcells
{
	my $cellname = shift;
	my $metaparams = shift;
	my $filter_cell = shift;
	my $flatten = shift;

	return if( not is_exist($cellname) );
	my %subcells = ();

	$filter_cell =~ /([a-zA-Z0-9_]+)$/;
	my $cellroot = $1;

	if( is_mid( $cellname ) )
	{
		my $subcell_block = CastParser::new_parse_subcells( $cellname );
		foreach my $inst (@{$subcell_block->{instances}})
		{
			my $type = $inst->{type};
			if( not $filter_cell or ($type eq $filter_cell or $type eq $cellroot) )
			{
#				print STDERR "MATCH: " . CastParserLib::format_instantiation($inst) . "\n";
				$type .= CastParserLib::format_metaparams($inst->{metaparams}) if( $metaparams );
				if( not $flatten or not defined $inst->{keywords} )
				{
					$subcells{$type} = "";
				}
			}
		}
	}
	elsif( is_leaf( $cellname ) )
	{
		my $prs_block=get_block_contents($cellname,"prs");
		return parse_prs_subcells($prs_block);
# FIXME: Need to parse prs block and get flattened subcells
	}
	else
	{
#		print STDERR "Do not know how to get subcells for $cellname\n";
	}

	return sort keys %subcells;
}

sub old_list_subcells
{
	my $cellname = shift;
	my $flatten = shift;

	my $celldata = parse_cast_cell($cellname);
	return if( not defined $celldata );

	return [] if( is_leaf($cellname) );		# list_subcells does not support this yet
	
	return $celldata->{subcells} if( $flatten );
	return $celldata->{need_to_import} if( not $flatten );
}

sub list_portlist
{
	my $cellname = shift;
	my $val = shift;

	return if( not is_exist($cellname) );
	my @portlist;

	my $def_block = CastParser::parse_def( $cellname );
	
	foreach my $port (@{$def_block->{portlist}})
	{
		push @portlist, expand_nodes(CastParserLib::format_port($port),$val);
	}

	return sort @portlist;
}

sub list_internal_nodes
{
	my $cellname = shift;
	my $val = shift;

	return if( not is_exist($cellname) );
	my @nodes = ();
	if( is_leaf($cellname) )
	{
		my $prs_block = CastParser::parse_prs($cellname);
		foreach my $decl (@{$prs_block->{decl}})
		{
			push @nodes, expand_nodes(CastParserLib::format_whole_decl($decl),$val);
		}
	}
	elsif( is_mid($cellname) )
	{
		my $subcell_block = CastParser::new_parse_subcells($cellname);
		foreach my $decl (@{$subcell_block->{declarations}})
		{
			push @nodes, expand_nodes(CastParserLib::format_whole_decl($decl),$val);
		}
	}
	else
	{
		print STDERR "Do not know how to get internal nodes for $cellname\n";
	}
	return sort @nodes;
}

sub list_all_nodes
{
	my $cellname = shift;
	my $val = shift;

	return if( not is_exist($cellname) );
	my @nodes;
	push @nodes, list_portlist($cellname,$val);
	push @nodes, list_internal_nodes($cellname,$val);

	return sort @nodes;
}

my %list_instances_cache;

# If flatten is TRUE, then obey flatten keyword
# If flatten is FALSE, ignore flatten keyword
sub list_instances
{
	my $cellname = shift;
	my $flatten = shift;

	return if( not is_exist($cellname) );

	return @{$list_instances_cache{$cellname}{$flatten}} if( defined $list_instances_cache{$cellname}{$flatten} );

	my %instances;
	my %need_to_import;

	my $subcell_block = CastParser::parse_subcells( $cellname );
	
	foreach my $inst (@{$subcell_block->{instances}})
	{
		my $instance = $inst->{instance};
		my $inst_name = @{$instance}[0];
		$inst_name .= "\[@{@{$instance}[1]}\]" if( @{$instance} > 1 );
		$instances{$inst_name} = "" if( not CastParser::_is_subcell_flattened($inst));
		$need_to_import{$inst_name} = "";
	}

	push @{$list_instances_cache{$cellname}{"0"}}, sort keys %need_to_import;
	push @{$list_instances_cache{$cellname}{"1"}}, sort keys %instances;

	return @{$list_instances_cache{$cellname}{$flatten}};
}

sub expand_nodes
{
	my $str = shift;
	my $val = shift;		# Use to evaluate if variable encountered

	my ($type,$name) = split ' ',$str;

	my @mydims = ();
	if( $name =~ /\[(.*)\]/ )
	{
		my $dim_range = $1;
		my @dims = split ',',$dim_range;
		foreach my $dim (@dims)
		{
			if( $dim =~ /^[0-9]+$/ )
			{
				push @mydims, [$dim,$dim];
			}
			else
			{
				my ($low,$foo,$high) = split '\.',$dim;
				$low = CastParser::_eval_expr($low,$val) if( defined $val );
				$high = CastParser::_eval_expr($high,$val) if( defined $val );
				if( not $low =~ /[0-9]+/ or not $high =~ /[0-9]+/ )
				{
					print STDERR "Do not know how to handle range in $name\n";
					return ();
				}
				else
				{
					push @mydims, [$low,$high];
				}
			}
		}
	}

	if( $type =~ /\[([^\]]+)\]/ )
	{
		my ($low,$foo,$high) = split '\.',$1;
		$low = CastParser::_eval_expr($low,$val) if( defined $val );
		$high = CastParser::_eval_expr($high,$val) if( defined $val );
		if( not $low =~ /^[0-9]+$/ or not $high =~ /^[0-9]+$/ )
		{
			print STDERR "Could not expand channel $type\n";
			return ();
		}
		else
		{
			my @tmp = @mydims;
			@mydims = [$low,$high];			
			push @mydims, @tmp;
		}
	}

	$type =~ s/\[.*//;

	my $basename = $name;
	$basename =~ s/\[.*//;
	my @nodes = recurse_expand_nodes($basename,\@mydims);
	my @results = ();

	foreach my $node (@nodes)
	{
		if( $type =~ /^node$/ )						# node
		{
			push @results, $node;
		}
		elsif( $type =~ /^1of([0-9]+)$/ )			# 1of(N)
		{
			my $range = $1;
			for( my $i = 0; $i < $range; $i++ )
			{
				push @results, "$node.$i";
			}
		}
		elsif( $type =~ /^1of\(([0-9]+)\)$/ )		# 1ofN
		{
			my $range = $1;
			for( my $i = 0; $i < $range; $i++ )
			{
				push @results, "$node.$i";
			}
		}
		elsif( $type =~ /^_1of([0-9]+)$/ )			# _1ofN
		{
			my $range = $1;
			for( my $i = 0; $i < $range; $i++ )
			{
				push @results, "$node.$i";
			}
		}
		elsif( $type =~ /^e1of([0-9]+)$/ )			# e1ofN
		{
			my $range = $1;
			for( my $i = 0; $i < $range; $i++ )
			{
				push @results, "$node.$i";
			}
			push @results, "$node.e";
		}
		elsif( $type =~ /^e1of\(([0-9]+)\)$/ )		# e1of(N)
		{
			my $range = $1;
			for( my $i = 0; $i < $range; $i++ )
			{
				push @results, "$node.$i";
			}
			push @results, "$node.e";
		}
		elsif( $type =~ /^s1of([0-9]+)$/ )			# e1ofN
		{
			my $range = $1;
			for( my $i = 0; $i < $range; $i++ )
			{
				push @results, "$node.$i";
			}
		}
		else
		{
			print STDERR "Do not know how to expand nodes in: $type\n";
		}
	}

	return @results;
}

sub recurse_expand_nodes
{
	my $name = shift;
	my $dims = shift;
	my $level = shift;

	$level = 1 if( not defined $level );

	my @nodes = ();
	if( @{$dims} == 0 )
	{
		return $name;
	}
	else
	{
		my ($low,$high) = @{pop @{$dims}};
		foreach my $node (recurse_expand_nodes($name,$dims,$level++))
		{
			for( my $i = $low; $i <= $high; $i++ )
			{
				push @nodes,"$node\[$i\]";
			}
		}
		return @nodes;
	}
}

# FIXME: For now just list all imports of file where cell defined
# FIXME: Handle multiple defines differently?
sub list_imports
{
	my $cellname = shift;

	my @defs = find_cell($cellname);
	return if( @defs != 1 );
	my $castfile = _get_full_path_with_rel($defs[0]);
	parse_cast_file($defs[0]);
	my @imports = @{$file_parse_cache{$castfile}{imports}};
	return @imports;
}

1;
