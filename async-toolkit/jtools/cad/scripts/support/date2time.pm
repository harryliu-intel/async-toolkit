#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;

sub date2time {
   my ($mo,$md,$yr,$hr,$mn,$se)=@_;
   return 0 if ! defined $mo;
   my ($te,$my,$monthyear,$test);
   my $time=time;
   my ($sec,$min,$hour,$mday,$mon,$year)=localtime($time);
   $se=0 if (! defined ($se));
   $mn=0 if (! defined ($mn));
   $hr=0 if (! defined ($hr));
   $yr=$year if (!defined ($yr) || ! ($yr =~ /^\d+$/));
   # eliminate non valid date strings
   if ( $yr < 70 ) {
      $yr += 100;
   }
   if ($yr > 1900) {
      $yr -= 1900;
   }
   # month range 0..11
   $mo--;
   # te num seconds in the month
   $te=$se+$mn*60+$hr*3600+$md*3600*24;
   # my num months
   $my=$yr*12+$mo;
   # NOW is starting point
   ($sec,$min,$hour,$mday,$mon,$year)=localtime($time);
   #first get the monthyear correct
   $monthyear=$year*12+$mon;
   my $n=0;
   while ($my != $monthyear) {
       $time += ($my-$monthyear)*28*3600*24;
       ($sec,$min,$hour,$mday,$mon,$year)=localtime($time);
       $monthyear=$year*12+$mon;
       $n++;
       return 0 if ($n > 5);
   }
   # calculate the delta T:
   $test=$se+$mn*60+$hr*3600+$md*3600*24-
      ($sec+$min*60+$hour*3600+$mday*3600*24);
   $time += $test;
   ($sec,$min,$hour,$mday,$mon,$year)=localtime($time);
   $monthyear=$year*12+$mon;
   # account for DST
   $time += ($my - $monthyear)*3600;
   ($sec,$min,$hour,$mday,$mon,$year)=localtime($time);
   $test=$se+$mn*60+$hr*3600+$md*3600*24-
      ($sec+$min*60+$hour*3600+$mday*3600*24);
   $time += $test;
   ($sec,$min,$hour,$mday,$mon,$year)=localtime($time);
   $test=$se+$mn*60+$hr*3600+$md*3600*24-
      ($sec+$min*60+$hour*3600+$mday*3600*24);
#   if ($test != 0) {
#      print STDERR "Warning: did not find time on first pass.";
#   }
   if ( $year != $yr || $mon != $mo || $md != $mday || $hr != $hour || $min != $mn || $sec != $se ) {
#      print "($year != $yr || $mon != $mo || $md != $mday || $hr != $hour || $min != $mn || $sec != $se ) ";
#      print STDERR "Mismatch";
#      my $fdate=localtime ($time);
#      print STDERR $fdate;
      return 0;
   }
   $time;
}
1;
