#!/usr/bin/perl

while (<STDIN>) {
    next if ($_ =~ /\/sva\//);      # strip out bound assertion files in /sva/ directories
    print "lappend VERILOG_SOURCE_FILES $_";
}
