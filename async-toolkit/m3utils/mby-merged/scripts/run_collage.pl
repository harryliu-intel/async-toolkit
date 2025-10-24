#!/usr/intel/pkgs/perl/5.14.1/bin/perl
#run_collage.pl
# Owner: Ram Koganti
# Date: 2/6/2017
# Desc: This script is a wrapper around collage_cache.
# It checks the collage cache and runs collage only if there is a
# a cache miss.  It has ono option -nocache, which disables
# cache check and always runs collage.

my $opt_nocache = 0;
my $run_collage = 1;

foreach (@ARGV) {
  if ($_ eq "-nocache") {
    $opt_nocache = 1;
  }
}

if (exists $ENV{COLLAGE_UPF_GEN}) {
  $opt_nocache =  ($ENV{COLLAGE_UPF_GEN} > 0);
}


my $cachecmd   = `ToolConfig.pl get_tool_exec collage_cache_cmd`;
my $collagecmd = `ToolConfig.pl get_tool_exec collage_cmd`;



if (!$opt_nocache) {
  print "executing: $cachecmd\n";
  system($cachecmd);
  print $?, "\n";
  if ($? == -1) {
    print "failed to execute: $!\n";
    $run_collage = 0;
    exit ($? >> 8);
  }
  elsif ($? & 127) {
    printf "child died with signal %d, %s coredump\n",
	($? & 127),  ($? & 128) ? 'with' : 'without';
    $run_collage = 0;
    exit ($? >> 8);
  } else {
    my $cachestatus = $? >> 8;
    $run_collage = ($cachestatus == 1 || $opt_nocache == 1) ? 1 : 0;
  }
} else {
  print "Not executing $cachecmd\n";
}

if ($run_collage) {
  #printf "child exited with value %d\n", $? >> 8;
  print "executing: $collagecmd\n";
  system($collagecmd);
  if ($? == -1) {
    print "failed to execute: $!\n";
    exit ($? >> 8);
  }
  elsif ($? & 127) {
    printf "child died with signal %d, %s coredump\n",
      ($? & 127),  ($? & 128) ? 'with' : 'without';
    exit ($? >> 8);
  }
  else {
    #printf "child exited with value %d\n", $? >> 8;
    exit ($? >> 8);
  }
} else {
  print "Not executing: $collagecmd\n";
}

exit 0;

