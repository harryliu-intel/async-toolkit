#!/usr/intel/bin/perl -w

#processas a signoff file so that it matches gds2 and cadence names

use FileHandle;
use IPC::Open2;

sub usage {
    print "Usage: $0 <signoff file>\n";
    exit 2;
}

$signOffFile = $ARGV[0];
defined $signOffFile or usage();

$rename = "rename";

my @cells = ();
open(IN,"<$signOffFile") or die "Can't read $signOffFile";
while( <IN> ) {
    if( /^\s*\(\s*cell\s*\"([^^]*)\"\s*\)/ ) {
        my ($cell) = split(" ",$1);
        push @cells,$cell;
    }
}

my %rename = ();
open2(*Reader,*Writer,"$rename --from=cadence --to=gds2 --type=cell");
foreach my $cell (@cells) {
    print Writer "$cell\n";
    my $new = <Reader>;
    chomp $new;
    $rename{$cell} = $new;
}

open(IN,"<$signOffFile") or die "Can't read $signOffFile";
while( <IN> ) {
    my $line = $_;
    if( /^\s*\(\s*cell\s*\"(.*)\"\s*\)/ ) {
        my ($old,$view,$lib) = split(" ",$1);
        if(defined $view) { $view = " $view"; } else { $view = ""; }
        if(defined $lib) { $lib = " $lib"; } else { $lib = ""; }
        if(defined $old && defined $rename{$old} ) {
            my $new = $rename{$old};
            print " ( cell \"^$old.*$view$lib\")\n";
            print " ( cell \"^$new.*\")\n";
        } else {
            print $line;
        }
    }
    else {
        print $line;
    }
}
