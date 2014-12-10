#
# FileIO
#
# Supersize module for reading from / writing to external files.
#
#

package Supersize::FileIO;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::Util;
use Supersize::TypeUtil;

sub get_module_data {
  return { NAME => "FileIO",
           DESC => "Commands for reading/writing data from/to external files.",
           COMMANDS => {
    "read_file" => {
        SUBREF => \&read_file,
        USAGE  => "read_file " . underline("file") . "|" .
                    underline("file_list"),
        DESC   => 
          "Reads the specified file(s), adding each line to the 'lines' ".
          "return list variable.  Blank lines and text following the " .
          "character '#' are ignored.",
        RV     =>
          { lines => { TYPE => $TYPE_LIST } } },
    "write_file" => {
        SUBREF => \&write_file,
        USAGE  => "write_file " . underline("file") . " " .
                    underline("list") . "|" . underline("map"),
        DESC   => 
          "Writes the contents of " . underline("list") . " or " . 
          underline("map") . " to the specified file.  Each element " .
          "will be written on its own line, with a single space separating ".
          "the key & value in the case of a map.  Any map values containing ".
          "spaces will be quoted (with \")." }
    }
  };
}

#
# read_file
#
sub read_file {
    my $SS_r = shift;
    command_die($SS_r, "Insufficient arguments to read_file") 
        if (num_args(\@_)==0);
    my @files;
    while (num_args(\@_)) {
        push @files, @{shift_next_list(\@_)};
    }
    my @lines;
    foreach my $file (@files) {
        if (!open (FILE, $file)) {
            command_die($SS_r, "Could not read file $file.");
        }
        while (<FILE>) {
            s/\s*(#.*)?$//;
            next if (/^\s*#/ || /^\s*$/);
            chomp;
            push @lines, $_;
        }
        close FILE;
    }
    print "Read " . scalar(@lines) . " lines.\n";

    # Return the file's lines in read_list.lines variable
    set_cmd_return_variable($SS_r, "lines", [ $TYPE_LIST, \@lines ]);
}

#
# write_file
#
sub write_file {
    my $SS_r = shift;
    my $file = shift_next_scalar(\@_);
    if (!defined $file) {
        command_die($SS_r, "Bad filename argument to write_file.");
    }
    my ($type, $val) = shift_next_arg(\@_);
    if (!defined $type || $type == $TYPE_ERROR) {
        command_die($SS_r, "Bad list/map argument to write_file.");
    }
    elsif ($type == $TYPE_SCALAR) {
        $type = $TYPE_LIST;
        $val = [ $val ];
    }
    print "Warning: Overwriting existing $file.\n" if (-e $file);
    if (!open (FILE, ">$file")) {
        command_die($SS_r, "Couldn't write to $file.");
    }

    # Write elements to file
    my $count = 0;
    if ($type == $TYPE_LIST) {
        foreach my $line (@{$val}) {
            print FILE $line . "\n";
            $count++;
        }
    }
    elsif ($type == $TYPE_MAP) {
        foreach my $key (sort keys %{$val}) {
            my $line = quote_scalar($key);
            my ($vtype, $vval) = @{$val->{$key}};
            if ($vtype == $TYPE_SCALAR) {
                $line .= " " . quote_scalar($vval);
            }
            elsif ($vtype == $TYPE_LIST) {
                foreach my $velement (@{$vval}) {
                    $line .= " " . quote_scalar($velement);
                }
            }
            else {
                print "don't know what $vtype is\n";
            }
            print FILE $line . "\n";
            $count++;
        }
    }
    close FILE;
    print "Wrote $count lines to $file.\n";
}

1;
