#!/usr/intel/bin/perl  
package common::ace_lib_utils;
use Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(remove_element_from_array ip_to_stub add_ip get_stub_mode get_hsopt);

sub remove_element_from_array
{
    
      my $element = $_[0];  
      my $arr = $_[1];   
      my $arr_size = @{$arr};
      my @index_arr;
      my $l_debug = 0;
      my $dbg = 0;
 
      my $index = 0; 
      #while (($$arr[$index] ne $element) && ($arr_size > $index) )
      while ($arr_size > $index)
      {
         if ($$arr[$index] eq $element) {
             push(@index_arr, $index);
         }
         $index++
      }
      
      #if ($index >= $arr_size)
      if (!(@index_arr))
      {
        print "$element is not found, so will not remove\n";
        return 1;
      }   
      else
      {
        my $offset = 0;
        my $inc = 0;
        foreach my $tmp (@index_arr) {
            $offset = $tmp - $inc;
            print "$element is at index $offset\n" if ($dbg);
            splice(@{$arr}, $offset, 1);
            $inc+=1;
        }
        return 0;
      }


}

sub ip_to_stub {
    my $ace_arg_counter=0;
    my @ip_stubs = ();
    my $ace_ip_stub_position;
    my $l_debug = 0;
    my @stubs;
    my $dbg = $debug || $l_debug;
    
    foreach my $ace_arg (@ARGV) 
    {
      print "ACE arg = $ace_arg \n" if ($dbg);
      if (
          ($ace_arg =~ /^-ip_to_stub$b/)||($ace_arg =~ /^-ip_to_stub$/)||($ace_arg =~ /^-ip_to_stub$/)||($ace_arg =~ /^-ip_to_stub$/) ||
          ($ace_arg =~ /^-ip_to_stub=/)||($ace_arg =~ /^-ip_to_stub=/)||($ace_arg =~ /^-ip_to_stub=$/)||($ace_arg =~ /^-ip_to_stub=/)
         ) 
      {
    
        if ($ace_arg =~ /\=/) 
        {
          @stubs = split(/\=/,$ace_arg,2);
          print "Stubs argument = $stubs[1] \n" if ($dbg);

          if ($stubs[1] =~ /\,/)
          {
              @ip_stubs = split(",",$stubs[1]);
              print("More than one IP will be stubbed\n") if ($dbg);
          }
          else
          {
              $ip_stubs[0] = $stubs[1];
              print("only one IP will be stubbed \n") if ($dbg);
          }
        } 
        else 
        {
            print ("Didn't find ip_to_stub=xxxx,yyyy") if ($dbg);
            return 1;
        }
      } 
      $ace_arg_counter++;
    }
    
    
    if (scalar(@ip_stubs) == 0)
    {
        $ip_stubs[0] = "no_stub";
    }
    
    chomp(@ip_stubs);
    print ("IPs to be stubbed = @ip_stubs\n") if ($dbg);
    return (@ip_stubs);
}


sub add_ip {
    my $ace_arg_counter=0;
    my @ip_adds = ();
    my $ace_ip_add_position;
    my $l_debug = 0;
    my @stubs;
    my $dbg = $debug || $l_debug;
    
    foreach my $ace_arg (@ARGV) 
    {
      print "ACE arg = $ace_arg \n" if ($dbg);
      if (
          ($ace_arg =~ /^-variant$b/)||($ace_arg =~ /^-variant$/)||($ace_arg =~ /^-variant$/)||($ace_arg =~ /^-variant$/) ||
          ($ace_arg =~ /^-variant=/)||($ace_arg =~ /^-variant=/)||($ace_arg =~ /^-variant=$/)||($ace_arg =~ /^-variant=/)
         ) 
      {
    
        if ($ace_arg =~ /\=/) 
        {
          @stubs = split(/\=/,$ace_arg,2);
          print "Stubs argument = $stubs[1] \n" if ($dbg);

          if ($stubs[1] =~ /\,/)
          {
              @ip_adds = split(",",$stubs[1]);
              print("More than one IP will be stubbed\n") if ($dbg);
          }
          else
          {
              $ip_adds[0] = $stubs[1];
              print("only one IP will be stubbed \n") if ($dbg);
          }
        } 
        else 
        {
            print ("Didn't find variant=xxxx,yyyy") if ($dbg);
            return 1;
        }
      } 
      $ace_arg_counter++;
    }
    
    
    if (scalar(@ip_adds) == 0)
    {
        $ip_adds[0] = "fc";
    }
    
    chomp(@ip_adds);
    print ("IPs to be added = @ip_adds\n") if ($dbg);
    return (@ip_adds);

}

sub get_stub_mode {
    my $ace_arg_counter=0;
    my $l_debug = 1;
    my $dbg = $debug || $l_debug;
    my @stubs;
    my $stub_mode = "val";

    foreach my $ace_arg (@ARGV) 
    {
      print "ACE arg = $ace_arg \n" if ($dbg);
      if (
          ($ace_arg =~ /^-stub_mode$b/)||($ace_arg =~ /^-stub_mode$/)||($ace_arg =~ /^-stub_mode$/)||($ace_arg =~ /^-stub_mode$/) ||
          ($ace_arg =~ /^-stub_mode=/)||($ace_arg =~ /^-stub_mode=/)||($ace_arg =~ /^-stub_mode=$/)||($ace_arg =~ /^-stub_mode=/)
         ) 
      {
    
        if ($ace_arg =~ /\=/) 
        {
          @stubs = split(/\=/,$ace_arg,2);
          $stub_mode = $stubs[1];
          print "Stubs argument = $stub_mode \n" if ($dbg);

          if (!($stub_mode eq "rtl" || $stub_mode eq "val")) {
               print ("FATAL - Invalid stub_mode specification. Please specify either -stub_mode=rtl/val") if ($dbg);
               return 1;
          }
        } 
        else 
        {
            print ("FATAL - Invalid stub_mode specification. Please specify either -stub_mode=rtl/val") if ($dbg);
            return 1;
        }
      }
      
      $ace_arg_counter++;
    }
    
    return $stub_mode;
}

sub get_hsopt {
    my $ace_arg_counter=0;
    my $l_debug = 1;
    my $dbg = $debug || $l_debug;
    my $hsopt_mode = 0;
    my @hsopt_tmp = ();

    foreach my $ace_arg (@ARGV) 
    {
      print "ACE arg = $ace_arg \n" if ($dbg);
      if (
          ($ace_arg =~ /^-hsopt$b/)||($ace_arg =~ /^-hsopt$/)||($ace_arg =~ /^-hsopt$/)||($ace_arg =~ /^-hsopt$/) ||
          ($ace_arg =~ /^-hsopt=/)||($ace_arg =~ /^-hsopt=/)||($ace_arg =~ /^-hsopt=$/)||($ace_arg =~ /^-hsopt=/)
         ) 
      {
    
        if ($ace_arg =~ /\=/) 
        {
          @hsopt_tmp = split(/\=/,$ace_arg,2);
          $hsopt_mode = $stubs[1];
          print "HSOPT argument = $hsopt_mode \n" if ($dbg);

        } 
        else { 
          $hsopt_mode = 1;
        }
      }
      
      $ace_arg_counter++;
    }
    
    return $hsopt_mode;
}


1;



