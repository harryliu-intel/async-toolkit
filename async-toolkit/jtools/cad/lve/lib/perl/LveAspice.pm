
package LveAspice;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        &open_aplot &write_aplot &read_aplot &flush_aplot &free_aplot &close_aplot
        &measure_minv &measure_maxv &measure_avg &measure_rms
        &measure_power &measure_frequency &measure_frequency_from_history
        &measure_skew &measure_slew &measure_aplot
        &read_fanin &read_inverses &read_named_resistors &read_start_times
        &measure_delay &make_png &coverage_merging  &measure_worst_avg_rms
        &characterize_delay &characterize_cap_delay &measure_charge &read_delay_scenarios
        &measure_charge_wcap &generate_pwl &evaluate_bumps_threshPercent &characterize_thresh
        &aplot_file_cmd &read_bump_scenarios
    );
}

use strict;
use FileHandle;
use IPC::Open2;
use File::Spec;
use File::Spec::Functions;
use LveUtil;
use LveMultinoise;
my  $aplotpid;
my $DEBUG=0;
my $dbgnr=0;
local (*AD);
my @fail_type=("pass",
               "max_thresh_percent",
               "thresh_resp_bound",
               "no_such_fanout",
               "no_thresh_scenario",
               "no_such_node",
               "no_node_out_file",
               "no_such_bump",
               "half_vdd",
               "vstatic13",
               "vstatic16",
               "no_switchingVoltages",
               "no_any_fanout_found",
               "no_such_fanin",
               "bump_width",
               "thresh_resp_bound:additive_resp",
               "no_thresh_scenario_to_estimate");

my $trace_archive="aspice_trace.zip";
my $resp_archive="thresh_resp.zip";


# spawn aplot with in/out pipes for measurements
sub open_aplot {
    my ($aplot) = @_;
    $aplotpid = open2(*APLOT_IN,*APLOT_OUT,$aplot);
    if ($DEBUG) {
        while ( -e "aplot.dbg.$dbgnr") {
            $dbgnr++;
        }
        open (AD, ">aplot.dbg.$dbgnr");
    }
}

# write a line to aplot
sub write_aplot {
    my ($line) = @_;
    print APLOT_OUT "$line\n";
    print AD ">$line\n" if $DEBUG;
}

# read a line from aplot
sub read_aplot {
    my $line = <APLOT_IN>;
    chomp($line);
    print AD "<$line\n" if $DEBUG;
    return $line;
}

# make sure to eat all output from aplot
sub flush_aplot {
    print APLOT_OUT "echo ***\n";
    my $line;
    do {
        $line = <APLOT_IN>;
        defined($line) or die "ERROR: broken pipe from aplot\n";
        chomp($line);
    } while (! ($line eq " ***"));
}

# free cached names in aplot
sub free_aplot {
    write_aplot "free";
    flush_aplot();
}

# close aplot
sub close_aplot {
    close APLOT_IN;
    close APLOT_OUT;
    # kill the zombie
    close AD if $DEBUG;
    waitpid $aplotpid, 0;
}

# measure the min bump for alint
sub measure_minv {
    my ($run,$node) = @_;
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range";
    write_aplot "minv $node";
    my $out = read_aplot();
    flush_aplot();
    return split(" ",$out);
}

# measure the max bump for alint
sub measure_maxv {
    my ($run,$node) = @_;
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range";
    write_aplot "maxv $node";
    my $out = read_aplot();
    flush_aplot();
    return split(" ",$out);
}

sub measure_bump_height{
    my($node,$start,$end,$dir)=@_;
    my @aplot_cmd    = ( "minv", "maxv" );
    
    if($node =~ /:F(\d+)$/){
      write_aplot "resistive off";
    }
    write_aplot "range $start:$end";
    print "range $start:$end\n" if($DEBUG);
    my $aplot_cmd = "$aplot_cmd[$dir] $node";
    print "$aplot_cmd\n" if($DEBUG);
    write_aplot($aplot_cmd);
    my $out = read_aplot();
    print "$out\n" if ($DEBUG);
    write_aplot "resistive on";
    flush_aplot();
    my($bump_time,$bump_v)=split(" ",$out);
    if (!is_numeric($bump_v)) {return ("FAIL",0); }
    return ($bump_time,$bump_v);

}
sub measure_end_time{
    my($start,$end,$node,$dir)=@_;
    my @peak_cmd    = ( "minv", "maxv" );
    my @end_cmd    = ( "maxv", "minv" );
    print "range $start:$end\n" if($DEBUG);
    write_aplot "$peak_cmd[$dir] $node";
    my $out = read_aplot();
    print "$out\n" if ($DEBUG);
    flush_aplot();
    my($peak_time,$v)=split(" ",$out);
    $peak_time=$peak_time-1e-3;
    write_aplot "range $peak_time:$end";
    print "range $peak_time:$end\n" if($DEBUG);
    write_aplot "$end_cmd[$dir] $node";
    $out = read_aplot();
    print "$out\n" if ($DEBUG);
    flush_aplot();
    (my $end_time,$v)=split(" ",$out);
    if (!is_numeric($end_time)) {
       $end_time = $end;
    }
    
    return $end_time;    
}
sub transfer_bump2ziggurat{
    my($bump_nearest_end,$power_sag_v,$startV,$node,$dir,$start,$end,$peak_v,$peak_time,$vddPercentRef,$threshPercRef,$last_time)=@_;
    my $zig_waveform="";
    #my $timePercentDn={};
    my @timePercentDn;
    my @timePercentUp;
    if($last_time == 1){
      $end = measure_end_time($start,$end,$node,$dir);
    }
    my $trigger_cmd="trigger_minv";
    $trigger_cmd="trigger_maxv" if ($dir);
    if($bump_nearest_end){
      if($trigger_cmd eq "trigger_maxv"){
        $trigger_cmd="trigger_minv";
      }else{
        $trigger_cmd="trigger_maxv";
      }
    }
    if($node =~ /:F(\d+)$/){
      write_aplot "resistive off";
    }
    write_aplot "range $start:$end";
    print "range $start:$end\n" if($DEBUG);
    my @vddPercent = @$vddPercentRef;
    my @threshPercent = @$threshPercRef;
    $zig_waveform .= "$start $startV 0\n";                            
    my $peak_perc=-1;
    for(my $i=0;$i<$#vddPercent;$i++){      
       my $v=$vddPercent[$i];
       if($dir==0){
          $v=$v+$power_sag_v;
          next if($v > $startV);
       }else{
          $v=$v-$power_sag_v;
          next if($v < 0);
       }
       my $aplot_cmd = "$trigger_cmd $node=$v";
       print "$aplot_cmd\n" if($DEBUG);
       write_aplot($aplot_cmd);
       my $triggers = read_aplot();
       print "$triggers\n" if ($DEBUG);
       flush_aplot();
       if ($triggers =~ /^WARNING/) {
           flush_aplot();
           $aplot_cmd = "trigger $node=$v";
           print "RETRY $aplot_cmd\n" if $DEBUG;
           write_aplot ($aplot_cmd);
           $triggers = read_aplot();
           print "$triggers\n" if $DEBUG;
           if ($triggers =~ /^WARNING/i) {
               $triggers = 0;
           }
       }
       if($triggers > 0){
           #bump is heigher or equal to this voltage          
           my $event=1;
           while ($event != 0){
               $aplot_cmd = "$trigger_cmd $node=$v $event";
               print "$aplot_cmd\n" if($DEBUG);
               write_aplot($aplot_cmd);
               my $out = read_aplot();
               print "$out\n" if ($DEBUG);
               flush_aplot();
               if ($out =~ /^WARNING/) {
                  flush_aplot();
                   $aplot_cmd = "$trigger_cmd $node=$v";
                   print "RETRY $aplot_cmd\n" if $DEBUG;
                   write_aplot ($aplot_cmd);
                   $out = read_aplot();
                   print "$triggers\n" if $DEBUG;
                   if ($out =~ /^WARNING/i) {
                       $out = "NA";
                   }
               }
               if($out ne "NA"){
                  if (!is_numeric($out)) {
                    warn "NON Valid value of trigger event\n";
                  }else{
                    if($event ==1){
                       if($out <= $peak_time){
                           #go up to peak                       
                           push @timePercentUp,[$i, $out];
                       }else{
                          #down hill event
                          unshift(@timePercentDn,[$i,$out]);
                       }                        
                    }else{
                          unshift(@timePercentDn,[$i,$out]);
                    }
                  }
               }
               if($event==$triggers){
                 $event=0;
               }else {$event=$triggers;}
           }
           $peak_perc=$i;
       }
    }
    write_aplot "resistive on";
    my $start_time=$start;
    my $end_time=$end;
    my $index_VP=-1;
    foreach my $upref (@timePercentUp){
        ($index_VP,$end_time)=@$upref;
        $zig_waveform .= $start_time." ".$vddPercent[$index_VP]." ".$threshPercent[$index_VP]."\n"; 
        if($end_time < $peak_time){
            $zig_waveform .= $end_time." ".$vddPercent[$index_VP]." ".$threshPercent[$index_VP]."\n";               
        }
        $start_time=$end_time;        
    }
    $zig_waveform .= $start_time." ".$vddPercent[$index_VP+1]." ".$threshPercent[$index_VP+1]."\n"; 
    my $prev_index_vp = $index_VP+1;
    foreach my $dnref (@timePercentDn){

        ($index_VP,$end_time)=@$dnref;
        if($prev_index_vp != $index_VP){
          #write previous end time
          $zig_waveform .= $end_time." ".$vddPercent[$prev_index_vp]." ".$threshPercent[$prev_index_vp]."\n";
          #write current start time
          $zig_waveform .= $end_time." ".$vddPercent[$index_VP]." ".$threshPercent[$index_VP]."\n"; 
        }
          $prev_index_vp=$index_VP;
    }
    
    $zig_waveform .= $end." ".$vddPercent[$prev_index_vp]." ".$threshPercent[$prev_index_vp]."\n";
    $zig_waveform .= "$end $startV 0\n";                            
    return $zig_waveform;
}
sub find_fastest_resp{
    my($thresh_resp,$timearrayref)=@_;
    my @timearray = @$timearrayref;

    my @fastresp; 
    my $max_vth=1;
    for my $vth (1..100){
        for my $i (0..$#timearray){
            my ($start,$end)=@{$timearray[$i]};
            if(not defined $thresh_resp->{"$start:$end"}){
                warn "Error: cannot find range $start:$end in thresh scenario\n";
            }
            if($vth==1){
               my $max_t=$#{$thresh_resp->{"$start:$end"}};
               $max_vth=$max_t if($max_t >$max_vth);
            }
            if(defined $thresh_resp->{"$start:$end"}->[$vth]){
                my $x=$thresh_resp->{"$start:$end"}->[$vth];
                if(not defined $fastresp[$vth]){
                    $fastresp[$vth]=$x; 
                }else{
                    $fastresp[$vth]=$x if($x<$fastresp[$vth]);
                }
            }
        }
        last if($vth==$max_vth);
    }

    return @fastresp;
}

sub measure_thresh_resp{
    my($Vdd,$node,$dir,$file,$timearrayref)=@_;
    my @aplot_cmd    = ( "minv", "maxv" );
    my %outresp;
    #print "find fastest response for $input->$node ($dir)\n";
    my @timearray = @$timearrayref;

    my $filecmd=aplot_file_cmd($file);
    if($file eq $filecmd){
      next if (! -e "$file.trace");
    }else{
      $file=$filecmd;
    }
    print "File = $file\n" if ($DEBUG);
    write_aplot("file \"$file\"");

    for(my $i=0;$i<scalar(@timearray);$i++){
        my ($start,$end)=@{$timearray[$i]};
        write_aplot "range $start:$end";
        print "range $start:$end\n" if($DEBUG);
        ${$outresp{"$start:$end"}}[0]=0;
        for(my $vth=1;$vth<=100;$vth++){
           my $v = $Vdd*$vth/100;
           $v = $Vdd-$v if($dir==0); #dn
        
           my $aplot_cmd = "trigger $node=$v 1";
           print "$aplot_cmd\n" if($DEBUG);
           write_aplot($aplot_cmd);
           my $out = read_aplot();
           print "$out\n" if ($DEBUG);
           flush_aplot();
           if ($out =~ /^WARNING/) {
               flush_aplot();
               $aplot_cmd = "trigger $node=$v 1";
               print "RETRY $aplot_cmd\n" if $DEBUG;
               write_aplot ($aplot_cmd);
               $out = read_aplot();
               print "$out\n" if $DEBUG;
               if ($out =~ /^WARNING/i) {
                   $out = "NA";
               }
           }
           #use 1s as infinity
           if($out ne "NA"){
              if (!is_numeric($out)) {
                push @{$outresp{"$start:$end"}},1e9;
                last;
              }else{          
                push @{$outresp{"$start:$end"}},$out-$start;          
              }
           }else{
              push @{$outresp{"$start:$end"}},1e9;
              last;
           }
        }
    }
    return %outresp;
}

#Signoff bumps by the result of output trace files in different percentage of Vdd
sub evaluate_bumps_threshPercent{
    my ($working_dir,$fh,$bump_debug,$bump_nearest_end,$power_sag,$thresh_resp_bound_ref,$source_pvt,$current_pvt,$cell,$node,$TP_data,$fanoutref, $alint_path, $raw_path,$raw_cell,$is_fanin_check,$alint_bin,$threshPercentRef, $additive_resp,$threshCC_ref, $threshTau_ref, $node_check_status,$no_power_sag) = @_;
    my ($process_s,$Vdd_s,$temp_s) = split(":",$source_pvt);
    my ($process_c,$Vdd_c,$temp_c) = split(":",$current_pvt);
    $Vdd_s =~s/V$//g;
    $Vdd_c =~s/V$//g;
    my %bumpdir=("bump_dn",0,
                 "bump_up",1);
    my %bump_check_status=();
    #find bump source node
    my $log="";

     #my $fanincell_str="";
     #my $faninnode_str="";
     #my $node_str=" node=$node";
     #$fanincell_str = " fanin_cell=$cell" if($is_fanin_check);
     #$faninnode_str = " fanin_node=$node" if($is_fanin_check);
     #$node_str      = "" if($is_fanin_check);
     my $finfo="$cell/$node";
     $finfo=$node if($cell eq $raw_cell);

     my $node_path="$alint_path/alint.bin.$alint_bin/$node"; 
     if(! -d $node_path ){
         warn "Error: Cannot find node directory $node_path\n";
         $bump_check_status{"FAIL"}++;

         my $fail_str=$fail_type[5];
         $fail_str=$fail_type[13] if($is_fanin_check);
         print $fh "FAIL alint $raw_cell $raw_path".
                  " fanin=$finfo" .
                  " source_pvt=$source_pvt".
                  " fail_type=$fail_str\n";
         return;
     }

     #read bump file
     my $scenarios={};
     my $out_file="$node_path/out";
     $out_file="$node_path/out.gz" if(-e "$node_path/out.gz");
     if ( -e $out_file) {
         $scenarios = read_bump_scenarios($out_file);
     }
     else {
         warn "Error: Cannot find corresponding out file in $node_path\n";
         $bump_check_status{"FAIL"}++;
         print $fh "FAIL alint $raw_cell $raw_path".
                  " fanin=$finfo" .
                  " source_pvt=$source_pvt".
                  " fail_type=$fail_type[6]\n";
         return;    
     }

     #ziggurat_bumps($working_dir,$zig_near_end,$power_sag,$Vdd_s,"$alint_path/alint.bin.$alint_bin",$node,$threshPercentRef,$scenarios);
     foreach my $fanoutinforef (@$fanoutref){
       my ($inst_path,$fanoutcell,$in_node,$out,$fanout_type,$thresh_up, $thresh_dn) = @$fanoutinforef;
       my $node_instname=$node;
       if(defined $inst_path){
          $node_instname=$in_node;
          $node_instname="$inst_path.$in_node" if ($inst_path ne "");
          
       }
       my $zig_near_end=0;
       $zig_near_end=1 if ((defined $no_power_sag->{pairs}->{"$cell/$node"} and scalar(keys %{$no_power_sag->{pairs}->{"$cell/$node"}})> 0 ) 
                             or $bump_nearest_end==1 );
       ziggurat_bumps_by_instpath($working_dir,$zig_near_end,$power_sag,$Vdd_s,"$alint_path/alint.bin.$alint_bin",$node,$node_instname,$threshPercentRef,$scenarios);
       #check all bumps
       foreach my $cctaucap (keys %$scenarios){         
           my ($bump_dn,$bump_dn_t,$bump_up,$bump_up_t)=(1e6,0,0,0);
           foreach my $bump_updn (keys %{$scenarios->{$cctaucap}}){
               my $fileprefix="$node_instname:$bump_updn:$cctaucap";
               next if(not defined $scenarios->{$cctaucap}->{$bump_updn} or scalar(@{$scenarios->{$cctaucap}->{$bump_updn}}) <= 0);
               my($fails, $passes)=bumpzig_check(0,undef,$working_dir,$raw_path,$bump_debug,$fh,$thresh_resp_bound_ref,$source_pvt,$current_pvt,$cell,$raw_cell,$node,$bump_updn, $fileprefix,$threshPercentRef, $fanoutinforef, $TP_data,$is_fanin_check,$additive_resp,$threshCC_ref, $threshTau_ref, $node_check_status,$no_power_sag);
               $bump_check_status{"FAIL"}+=$fails;
               $bump_check_status{"PASS"}+=$passes;
               if($zig_near_end){
                 my($fails, $passes)=bumpzig_check(1,!$bump_nearest_end,$working_dir,$raw_path,$bump_debug,$fh,$thresh_resp_bound_ref,$source_pvt,$current_pvt,$cell,$raw_cell,$node,$bump_updn, $fileprefix,$threshPercentRef, $fanoutinforef, $TP_data,$is_fanin_check,$additive_resp,$threshCC_ref, $threshTau_ref, $node_check_status,$no_power_sag);
                 $bump_check_status{"FAIL"}+=$fails;
                 $bump_check_status{"PASS"}+=$passes;
               }
               if($power_sag>0){
                  $fileprefix.=":s$power_sag";
                  my($fails, $passes)=bumpzig_check(0,undef,$working_dir,$raw_path,$bump_debug,$fh,$thresh_resp_bound_ref,$source_pvt,$current_pvt,$cell,$raw_cell,$node,$bump_updn, $fileprefix,$threshPercentRef, $fanoutinforef, $TP_data,$is_fanin_check,$additive_resp,$threshCC_ref, $threshTau_ref, $node_check_status,$no_power_sag);
                  $bump_check_status{"FAIL"}+=$fails;
                  $bump_check_status{"PASS"}+=$passes;
                  if($bump_nearest_end){
                    my($fails, $passes)=bumpzig_check(1,!$bump_nearest_end,$working_dir,$raw_path,$bump_debug,$fh,$thresh_resp_bound_ref,$source_pvt,$current_pvt,$cell,$raw_cell,$node,$bump_updn, $fileprefix,$threshPercentRef, $fanoutinforef, $TP_data,$is_fanin_check,$additive_resp,$threshCC_ref, $threshTau_ref, $node_check_status,$no_power_sag);
                    $bump_check_status{"FAIL"}+=$fails;
                    $bump_check_status{"PASS"}+=$passes;
                  }
               }
           }#foreach my $bump_updn (keys %{$scenarios->{$cctaucap}})
       }
       
     }

    return %bump_check_status;


}


#from bump ziggurat, generating the output response waveform from threshPercent response file
#and check the bump
sub bumpzig_check{
  my ($bump_nearest_end,$signoff_nearest_end,$working_dir,$path,$bump_debug,$fh,$thresh_resp_bound_ref,$source_pvt,$current_pvt,$fanin_cell,$cell,$node,$bump_dir,$fileprefix,$threshPercentRef, $fanoutInfoRef, $TP_data,$is_fanin_check, $additive_resp,$threshCC_ref, $threshTau_ref, $node_check_status,$no_power_sag)=@_;
  my ($process_s,$Vdd_s,$temp_s) = split(":",$source_pvt);
  my ($process_c,$Vdd_c,$temp_c) = split(":",$current_pvt);
  $Vdd_s =~s/V$//g;
  $Vdd_c =~s/V$//g;

  my @threshCC=@$threshCC_ref;
  my @threshTau=@$threshTau_ref;
  my $fail_status=0;
  my $pass_status=0;
  my @threshPercent=@$threshPercentRef;
  my $log="";
  my $thresh_dir="thresh_dn";
  $thresh_dir="thresh_up" if ($bump_dir eq "bump_dn");
  
  my $bumpfilename="$working_dir/$fileprefix.bumpzig";
  $bumpfilename.=".nd" if ($bump_nearest_end);
  my($status, %bumpdata)=parse_bumpzig($bumpfilename);
  my($node_instname,$bump_updn,$bump_cc,$bump_tau,$powersag)=split(":",$fileprefix);  
  my $power_sag_str="";
  if(defined $powersag){
     if($powersag =~ /s(\d+)/){
        $power_sag_str=" power_sag=$1";
     }
  }
  my $source_pvt_str="";
  if($source_pvt ne $current_pvt){
     $source_pvt_str=" source_pvt=$source_pvt";
  }
  my $nearest_end_str="";
  $nearest_end_str=" bump_curve=nearest_end" if($bump_nearest_end);
  my $fin="$fanin_cell/$node";
  $fin=$node if($fanin_cell eq $cell);

  if(defined $powersag){
    #disable powersag when fanin is localnode
    return ($fail_status,$pass_status) if(defined $no_power_sag->{cells}->{$fanin_cell});
  }
  if($status eq "FAIL"){
    warn "CANNOTN find $bumpfilename\n" if ($DEBUG);
    $fail_status++;
    print $fh "FAIL alint $cell $path".
      " fanin=$fin" .
      "$power_sag_str" .
      "$source_pvt_str" .
      "$nearest_end_str" .
      " fail_type=$fail_type[7]:$fileprefix\n";       
    return ($fail_status,$pass_status);
  }
  
  my %max_Vperc;
  my %max_bump_v;
  my %max_bump_time;
  my @thresh_error_msg=();
  foreach my $bumpinfo (keys %bumpdata){
     my($bump_peak,$bump_peak_time)=split("@",$bumpinfo);
     my $bump_v_perc=$bump_peak;
     $bump_v_perc=$Vdd_s-$bump_peak if($bump_dir eq "bump_dn");
     $bump_v_perc = int(100*$bump_v_perc/$Vdd_s + 0.5);
     my $bump_time_ps = $bump_peak_time;
     my $s = ns_to_ps($bump_time_ps);             
     print "\n==========================================================\n" if ($DEBUG);
     print "$bump_dir for $node $bump_updn:$bump_cc:$bump_tau $power_sag_str $bumpinfo\n" if ($DEBUG);
     print "==========================================================\n" if ($DEBUG);
     if($bump_v_perc > $threshPercent[$#threshPercent]){
         $fail_status++;
         print $fh "FAIL alint $cell $path".
                 " fanin=$fin" .
                 " cc=$bump_cc" .
                 " tau=$bump_tau" .
                 "$power_sag_str" .
                 "$source_pvt_str".
                 " $bump_updn=${bump_v_perc}\@${bump_time_ps}".
                 " resp_bound=$thresh_resp_bound_ref->[1]".
                 " bump_bound=$threshPercent[$#threshPercent]".
                 " fail_type=$fail_type[1]\n";
         next;                              
     }

     my $get_bump_resp=sub{
       my ($fanoutcell,$in, $out)=@_;
       print "* ${node}'s fanout: $fanoutcell/$in/$out\n" if ($DEBUG);
       if(not defined $TP_data->{$fanoutcell}->{$in}->{$out}->{$thresh_dir}){
           return ("FAIL:$fail_type[4]:$in:$thresh_dir");
       }
       if(defined $TP_data->{$fanoutcell}->{$in}->{$out}->{$thresh_dir} and $TP_data->{$fanoutcell}->{$in}->{$out}->{$thresh_dir}==-1){
           return ("IGNORE");
       }
       my %br_fh;
       if($bump_debug){
          if(! -d "$working_dir/$fanoutcell/$out"){
             system("mkdir -p  \"$working_dir/$fanoutcell/$out\"");
          }
       }
       my %max_Vperc_;
       my %resp_Vperc;
       my %resp_time;
       my %resp;
       for my $zigdurref (@{$bumpdata{$bumpinfo}}){
           my ($dt,$perc)=@$zigdurref;
           foreach my $threshcc (@threshCC){
              foreach my $threshtau (@threshTau){
                my $cctau="$threshcc:$threshtau";
                if(not defined $TP_data->{$fanoutcell}->{$in}->{$out}->{$thresh_dir}->{$cctau}){
                   #no thresh response found for this cc and tau.
                   
                   return ("FAIL:$fail_type[4]:$in:$thresh_dir:$cctau");
                }
                if(not defined $TP_data->{$fanoutcell}->{$in}->{$out}->{$thresh_dir}->{$cctau}->{$perc}){
                  #find the close one to interprete
                  my $perc_est=undef;
                  my @perc_avaliable = sort {$a <=> $b}  keys %{$TP_data->{$fanoutcell}->{$in}->{$out}->{$thresh_dir}->{$cctau}};
                  foreach my $perc_t (@perc_avaliable){
                      if ($perc_t >= $perc){
                        $perc_est = $perc_t;
                        last;
                      }
                  }
                  if(not defined $perc_est){
                    return ("FAIL:$fail_type[16]:$in:$thresh_dir:$cctau:$perc");
                  }
                  $perc=$perc_est;
                }
                if($bump_debug){
                     if(not defined $br_fh{$cctau}) {
                         my $dir="$working_dir/$fanoutcell/$out/$thresh_dir:$cctau";
                         system("mkdir -p  \"$dir\"");
                         my $respfile="$dir/${bump_v_perc}\@${bump_time_ps}.bumpresp";
                         $br_fh{$cctau}=new FileHandle;
                         $br_fh{$cctau}->open(">$respfile") or die "cannot write $respfile\n";
                     }
                }
                $resp_Vperc{$cctau}=0 if (not defined $resp_Vperc{$cctau});
                $resp_Vperc{$cctau}=linearInterp_response($resp_Vperc{$cctau},$dt,$TP_data->{$fanoutcell}->{$in}->{$out}->{$thresh_dir}->{$cctau}->{$perc});
                $resp_time{$cctau}=0 if (not defined $resp_time{$cctau});
                $resp_time{$cctau}=$resp_time{$cctau}+$dt;
                print " zigdur=$dt,cc,tau=$cctau $perc% ---> respV=$resp_Vperc{$cctau}% time=$resp_time{$cctau}\n" if ($DEBUG);
                print {$br_fh{$cctau}} "$resp_time{$cctau} $resp_Vperc{$cctau}\n" if ($bump_debug);
                push @{$resp{$cctau}}, [$resp_time{$cctau},$resp_Vperc{$cctau}];
                if($resp_time{$cctau} > 1.0) { #bump width is larger than 1ns.
                  $max_Vperc_{$cctau}=$resp_Vperc{$cctau};
                  last;
                }
                if(not defined $max_Vperc_{$cctau} or $resp_Vperc{$cctau} >= $max_Vperc_{$cctau}){
                  $max_Vperc_{$cctau}=$resp_Vperc{$cctau};
                }else{
                  last;
                }
                 
              }
           }
       }#for my $zigdurref (@{$bumpdata{$bumpinfo}})
       if($bump_debug) {foreach my $fh (keys %br_fh) {$br_fh{$fh}->close();}}
       return ("SUCCESS", \%resp_time,\%max_Vperc_, \%resp);
     };
     
     #start to check fanout response
     my ($inst_path,$fanoutcell,$in_node,$out,$fanout_type,$thresh_up, $thresh_dn) = @$fanoutInfoRef;
     if(defined $powersag or $bump_nearest_end==0){
           if(defined $no_power_sag->{pairs}->{"$fanin_cell/$node"}->{"$fanoutcell/$in_node/$out"}){
              #1. if power sag is on, directive signoff for non-power-sag pairs
              #2. if running farest end, directive signoff for non-power-sag (near-end) pairs
              next ;
           }
     }
     if($bump_nearest_end and $signoff_nearest_end){
         if(not defined $no_power_sag->{pairs}->{"$fanin_cell/$node"}->{"$fanoutcell/$in_node/$out"}){
           #If nearest end is on and signoff mode is on, only run nearest end for directive signoff pairs
           next;
         }
     }
     my $fanout_type_str="";
     $fanout_type_str=" fanout_type=PO" if ($fanout_type eq "OUT");
     my $fout="$fanoutcell/$out";
     $fout=$out if($fanoutcell eq $cell);
     my @in_nodes=($in_node);
     push @in_nodes, '*' if ($additive_resp);
     foreach my $in (@in_nodes){
       my ($status, $resp_time_ref, $max_Vperc_ref, $resp_ref)=$get_bump_resp->($fanoutcell,$in,$out);
       next if ($status =~/IGNORE/);
       if($status =~ /FAIL/){
         my $fail_msg=$status;
         $fail_msg=~s/^FAIL://;
         $node_check_status->{$out}->{fanin}->{fail}=1 if ($fanoutcell eq $cell);
         $node_check_status->{$node}->{fanout}->{fail}=1 if ($fanin_cell eq $cell);

         my $msg= "FAIL alint $cell $path".
                 " fanin=$fin" .
                 " fanout=$fout" .
                 " cc=$bump_cc" .
                 " tau=$bump_tau" .
                 "$power_sag_str" .
                 "$source_pvt_str".
                 "$fanout_type_str" .
                 "$nearest_end_str" .
                 " fail_type=$fail_msg\n";
         push @thresh_error_msg,$msg;
       
         next;
       }
       foreach my $cctau (keys %$resp_time_ref){
           my ($threshCC,$threshTau)=split(":",$cctau);
           my $resp_time=$resp_time_ref->{$cctau};
           my $max_Vperc_=$max_Vperc_ref->{$cctau};
           my $thresh_resp_bound=$thresh_resp_bound_ref->[$threshCC];
           my $signoff=0;
           
           my $resp_str="resp_up";
           $resp_str="resp_dn" if ($bump_dir eq "bump_up");
           if($in eq '*'){
               $resp_str="additive_".$resp_str;
           }
           my $max_Vperc_int = int($max_Vperc_+0.5);
           if($resp_time > 1.0 and $max_Vperc_int > 10){
               print "FAIL bump width\n" if ($DEBUG);
               $fail_status++;
               $node_check_status->{$out}->{fanin}->{fail}=1 if ($fanoutcell eq $cell);
               $node_check_status->{$node}->{fanout}->{fail}=1 if ($fanin_cell eq $cell);
               print $fh "FAIL alint $cell $path".
                       " fanin=$fin" .
                       " fanout=$fout" .
                       " cc=$bump_cc" .
                       " tau=$bump_tau" .
                       " threshCC=$threshCC" .
                       " threshTau=$threshTau" .
                       "$power_sag_str" .
                       "$source_pvt_str".
                       "$fanout_type_str" .
                       " $resp_str=$max_Vperc_int" .
                       " $bump_dir=${bump_v_perc}\@${bump_time_ps}".
                       "$nearest_end_str" .
                       " resp_bound=$thresh_resp_bound".
                       " bump_bound=$threshPercent[$#threshPercent]".
                       " fail_type=$fail_type[14]\n";
               last;
           }
           if($max_Vperc_int > $thresh_resp_bound){
               if (defined $thresh_dn and $thresh_dn !=-1 and $bump_dir eq "bump_up"){
                   $thresh_resp_bound=$thresh_dn;
               }
               if (defined $thresh_up and $thresh_up !=-1 and $bump_dir eq "bump_dn"){
                   $thresh_resp_bound=$thresh_up;
               }
               if($max_Vperc_int <= $thresh_resp_bound){
                 $signoff=1;
               }else{
                 $fail_status++;
                 my $fail_type=$fail_type[2];
                 if($in eq '*'){
                    $fail_type=$fail_type[15];
                 }
                 $node_check_status->{$out}->{fanin}->{fail}=1 if ($fanoutcell eq $cell);
                 $node_check_status->{$node}->{fanout}->{fail}=1 if ($fanin_cell eq $cell);
                 print $fh "FAIL alint $cell $path".
                         " fanin=$fin" .
                         " fanout=$fout" .
                         " cc=$bump_cc" .
                         " tau=$bump_tau" .
                         " threshCC=$threshCC" .
                         " threshTau=$threshTau" .
                         "$power_sag_str" .
                         "$source_pvt_str".
                         "$fanout_type_str" .
                         " $resp_str=$max_Vperc_int" .
                         " $bump_dir=${bump_v_perc}\@${bump_time_ps}".
                         "$nearest_end_str" .
                         " resp_bound=$thresh_resp_bound".
                         " bump_bound=$threshPercent[$#threshPercent]".
                         " fail_type=$fail_type\n";
               }


           }
           my $signoff_str="";
           $signoff_str="SIGNOFF;fanout=$fout;" if($signoff);
           my $type="$signoff_str$resp_str;threshCC=$threshCC;threshTau=$threshTau";
           $max_Vperc{$type} = $max_Vperc_ if(not defined $max_Vperc{$type} or $max_Vperc_ > $max_Vperc{$type});
           $max_bump_v{$type}=-1 if(not defined $max_bump_v{$type} );
           if(($max_Vperc_ > $max_Vperc{$type}) or (($max_Vperc_ == $max_Vperc{$type}) and $bump_v_perc > $max_bump_v{$type})){
               $max_Vperc{$type} = $max_Vperc_;
               $max_bump_v{$type}= $bump_v_perc;
               $max_bump_time{$type}= $bump_time_ps;
               my $bump_scen="$bump_dir:$bump_cc:$bump_tau";
               LveMultinoise::save_bump_resp($fanoutcell, $in, $out, "${bump_v_perc}\@${bump_time_ps}", $resp_ref->{$cctau},
                                             $bump_scen, "$threshCC:$threshTau", $power_sag_str, $source_pvt_str, $fanout_type_str);
           }
           my $type_t = $type.";cc=$bump_cc;tau=$bump_tau;$power_sag_str;$source_pvt_str;$nearest_end_str;resp_bound=$thresh_resp_bound"; $type_t=~s/\s//g;
           if ($fanoutcell eq $cell){
             my $type_t_fin = $type_t.";$fanout_type_str"; $type_t_fin=~s/\s//g;
             if(defined $node_check_status->{$out}->{fanin}->{$type_t_fin}){
               my ($Vperc, $bump)=@{$node_check_status->{$out}->{fanin}->{$type_t_fin}};
               if($Vperc < $max_Vperc_){
                   $node_check_status->{$out}->{fanin}->{$type_t_fin}=[$max_Vperc_, "$bump_dir=$bump_v_perc\@$bump_time_ps"];
               }elsif($Vperc == $max_Vperc_ ){
                   my ($bd,$bump_h, $bump_t)=split(/[\=|@]/, $bump);
                   if($bump_h < $bump_v_perc){
                     $node_check_status->{$out}->{fanin}->{$type_t_fin}=[$max_Vperc_, "$bump_dir=$bump_v_perc\@$bump_time_ps"];
                   }		
               }
             }else{
               $node_check_status->{$out}->{fanin}->{$type_t_fin}=[$max_Vperc_, "$bump_dir=$bump_v_perc\@$bump_time_ps"];
             }
           }
           if ($fanin_cell eq $cell){
             if(defined $node_check_status->{$node}->{fanout}->{$type_t}){
               my ($Vperc, $bump)=@{$node_check_status->{$node}->{fanout}->{$type_t}};
               if($Vperc < $max_Vperc_){
                   $node_check_status->{$node}->{fanout}->{$type_t}=[$max_Vperc_, "$bump_dir=$bump_v_perc\@$bump_time_ps"];
               }elsif($Vperc == $max_Vperc_ ){
                   my ($bd,$bump_h, $bump_t)=split(/[\=|@]/, $bump);
                   if($bump_h < $bump_v_perc){
                     $node_check_status->{$node}->{fanout}->{$type_t}=[$max_Vperc_, "$bump_dir=$bump_v_perc\@$bump_time_ps"];
                   }		
               }
             }else{
               $node_check_status->{$node}->{fanout}->{$type_t}=[$max_Vperc_, "$bump_dir=$bump_v_perc\@$bump_time_ps"];
             }
           }
         
       }
     }
  }  
  my %thresh_err_msg = map { $_ => 1 } @thresh_error_msg;
  foreach my $msg (keys %thresh_err_msg){
      $fail_status++;
      print $fh $msg;
  }
  #Report PASS at upper level.
  return ($fail_status,$pass_status);
}


sub linearInterp_response{
    my($last_v,$dt,$threshRespref)=@_;    
    my $respVp=0;
    my $max_v= scalar(@$threshRespref)-1;
    if($threshRespref->[$max_v] == 1e9){
      $max_v--;
    }
    my $max_t= $threshRespref->[$max_v];

    my $last_v_f=int($last_v);
    my $last_v_c=int($last_v)+1;

    if($last_v_f>=$max_v || $last_v_c>=$max_v){
    #    print "reach stable voltage, return stable voltage:$max_v%\n";
        return $max_v;
    }

    #do linear interpolation
    my $last_t_f = $threshRespref->[$last_v_f];
    my $last_t_c = $threshRespref->[$last_v_c];
      
    my $delta_t = ($last_t_c-$last_t_f)*($last_v-$last_v_f);
    #print "delta t= ($last_t_c - $last_t_f)*($last_v-$last_v_f)=$delta_t\n";
    
    my $resp_t = $last_t_f+$delta_t+$dt;
    #search for corresponding Voltage
    if($resp_t > $max_t){
      #reach stable voltage
      return $max_v;
    }

    $respVp=binary_search_thresh_resp($resp_t,$last_v_f,$max_v,$threshRespref);
    return $respVp;
}

sub binary_search_thresh_resp{
    my($t,$V1,$V2,$table)=@_;
    if($V1==($V2-1)){
        my $t1=$table->[$V1];
        my $t2=$table->[$V2];
        return $V2 if($t>$t2);
        my $V=$V1+($t-$t1)/($t2-$t1);
        return ($V);
    }
    return $V1 if($V1==$V2);

    my $Vmid=int(($V1+$V2)/2);
    if($t >= $table->[$Vmid]){
      my($V)=binary_search_thresh_resp($t,$Vmid,$V2,$table);
      return ($V);
    }else{
      my($V)=binary_search_thresh_resp($t,$V1,$Vmid,$table);
      return ($V);
    }
}

#parsing bumpzig file
sub parse_bumpzig{
    my($file)=@_;
    my %data=();
    print "file \"$file\"\n" if($DEBUG);
    if (! -e "$file" ) {
        return ("FAIL", %data);
    }

    my $prev_v=undef;
    my $prev_vp=undef;
    my $prev_t=undef;
    my $node="";
    my $bump_dir="";
    my $cctauslew="";
    my $peak_v="";
    my $peak_t="";
    open BZ, "<$file" or die "CANNOT open file $file\n";
    while(<BZ>){
      chomp;
      if(/^#/){
        if(! s/^#BUMP INFO:\s*//){next;}
        #start a new bump
        ($node, $bump_dir, $cctauslew,$peak_v,$peak_t) = split(/\s+/,$_);
        $prev_v=undef;
        $prev_t=undef;
      }else{    
        #time voltage voltagePercent
        my($t,$v,$vp)=split(/\s+/,$_);
        if(defined $prev_v){
          if($prev_vp != $vp){
            #drop it;
            $prev_vp = $vp;
            $prev_v = $v;
            $prev_t = $t;
          }else{
            #calculate time      
            my $step_dur=$t-$prev_t;
            push @{$data{"$peak_v\@$peak_t"}}, [$step_dur,$vp];
          }
        }else{
          $prev_v = $v;
          $prev_vp = $vp;
          $prev_t = $t;
        }
      }
    }
    return ("SUCESS",%data);
    close BZ;
}

#characterize_thresh is called by lve_threshresp before archive all resp files.
sub characterize_thresh{
    my ($Vdd,$alint_dir,$localprops,%skipnodes) = @_;
    my @thresh_dir=("thresh_dn", "thresh_up");
    my %hash_dir=("thresh_dn",0,
                  "thresh_up",1);
    
    %skipnodes=() if ! %skipnodes;
    my $alt_alint_dir = $alint_dir;
    $alt_alint_dir =~ s/\.tmp$//;

    $localprops = $alint_dir if ! defined $localprops;
    my @localnodes=();
    if ( -f "$localprops" or -l "$localprops") {
        @localnodes = read_localnodes($localprops);
    }
    else {
        @localnodes = ($localprops);
    }
    for my $node (@localnodes) {
        next if $skipnodes{$node};
        free_aplot();
        my $out_file="$alint_dir/$node/out";
        $out_file="$alint_dir/$node/out.gz" if(-e "$alint_dir/$node/out.gz");
        if(! -e $out_file){
           $out_file="$alt_alint_dir/$node/out" if(-e "$alt_alint_dir/$node/out");
           $out_file="$alt_alint_dir/$node/out.gz" if(-e "$alt_alint_dir/$node/out.gz");
        }
        if (! -e $out_file) {
            warn "No alint data found for local node $node. Skipping " .
                 "delay characterization.\n";
            next;
        }
        my $node_path="";
        my %scenarios=();
        my %timearray=();
           %scenarios = read_delay_scenarios("$out_file",0,"thresh",\%timearray);

        foreach my $scenario (keys %scenarios){
            my($thresh_updn,$cc,$tau,$perc)=split(":",$scenario);
            my $hashref = $scenarios{$scenario};
            my $timearrayref = \@{$timearray{$scenario}};
            my %thresh_resp=measure_thresh_resp($Vdd,$node,$hash_dir{$thresh_updn},"$alint_dir/$node/$scenario",$timearrayref);
            my %name_map=thresh_name_maps( "$alint_dir/$node/$scenario.names");

            my %add_resp;
            my @alias_names;
            open AF, ">$alint_dir/$node/$scenario.resp" or die "cannot write file $alint_dir/$node/$scenario.resp\n";
            for my $input (keys %$hashref) {
                my $alias_names;
                if(defined $name_map{$input}){
                  $alias_names="#$input ";
                  $alias_names .= join(" ",@{$name_map{$input}});
                } else {
                    print STDERR "WARNING: missing name_map\n";
                    print STDERR "  alint_dir=$alint_dir\n";
                    print STDERR "  node=$node\n";
                    print STDERR "  scenario=$scenario\n";
                    print STDERR "  input=$input\n";
                }
                my@fastresp=find_fastest_resp(\%thresh_resp,$hashref->{$input});                
                shift @fastresp;
                my $outresp_result = join ("\n",@fastresp);
                open RF, ">$alint_dir/$node/$input:$scenario.resp" or die "cannot write file $alint_dir/$node/$input:$scenario.resp\n";
                print RF "$outresp_result\n";
                print RF "$alias_names\n" if (defined $alias_names);
                push @alias_names, "$alias_names" if (defined $alias_names) ;
                close RF;
                archive_addfile("$alint_dir/$node/$resp_archive", "$alint_dir/$node/$input:$scenario.resp");
                system("rm -f \'$alint_dir/$node/$input:$scenario.resp\'");

                #prepare addition response
                unshift (@fastresp,0);
                if ((scalar keys %add_resp) == 0) {
                  %add_resp = map { $fastresp[$_] => $_ } 0..$#fastresp;
                }else{
                  my $v_pre=0;
                  my $v_base=0;
                  my $v_add=0;
                  my $max_v=$#fastresp;
                  if ( $fastresp[$#fastresp] == 1e9 ){
                        $max_v=$max_v-1;
                  }
                  foreach my $t_base (sort keys %add_resp){
                      
                      last if ($t_base == 1e9);
                      if($v_add==100) {
                        $add_resp{$t_base}=100;
                        last;
                      }
                      #find voltage % at threshresp
                      my $v=binary_search_thresh_resp($t_base,$v_pre,$max_v,\@fastresp);
                      $v_base=$add_resp{$t_base};
                      $v_add=$add_resp{$t_base}+$v;
                      $v_add=100 if ($v_add > 100);
                      $add_resp{$t_base}=$v_add;
                      $v_pre=POSIX::floor($v);
                      #print AF "addition $t_base find: $v @ $input:$scenario $v_base -> $v_add\n";
                  }
                  #continue add response
                  for(my $i=$v_pre+1; $i<=$max_v;$i++){
                    $v_add=$i+$v_base;
                    $add_resp{$fastresp[$i]}=$v_add;
                    if ($v_add > 100){
                      $add_resp{$fastresp[$i]}=100;
                      last;
                    }
                    #print AF "Continue addition t:$fastresp[$i] find:$i -> $add_resp{$fastresp[$i]}\n";
                  }
                }
            }
            my $v_pre=0;
            my $v_bin=1;
            my $t_pre=0;
            foreach my $t_base (sort keys %add_resp){
                next if($t_base == 0);
                if($t_base == 1e9){
                  #print AF "$t_base $v_pre\n";
                  print AF "$t_base\n";
                  last;
                }elsif($add_resp{$t_base}>=100){
                  #print AF "1000000000 100\n";
                  print AF "1000000000\n";
                  last;
                }else{
                  #print AF "$t_base $add_resp{$t_base}\n";
                  while($v_bin <= $add_resp{$t_base} and $v_bin <=100){
                     my $t=$t_pre+($t_base-$t_pre)*(($v_bin-$add_resp{$t_pre})/($add_resp{$t_base}-$add_resp{$t_pre}));
                     #print AF "$t $v_bin\n";
                     print AF "$t\n";
                     $v_bin++;
                  }
                }
                $v_pre=$add_resp{$t_base};
                $t_pre=$t_base;
            }
            print AF join("\n", @alias_names);
            close AF;
            archive_addfile("$alint_dir/$node/$resp_archive", "$alint_dir/$node/$scenario.resp");
            system("rm -f \'$alint_dir/$node/$scenario.resp\'");
        }
   }
}

sub thresh_name_maps{
  my($file)=@_;
  my @lines=();
  if(-e $file){
    open NF,"<$file" or die "cannot open file $file\n";
    @lines=<NF>;
    close(NF);
  }else{
    my @paths=split("/",$file);
    my $zipfile=$file;
    $zipfile=~s/\/$paths[$#paths]$/\/$trace_archive/;
    my $lines=archive_getcontent($zipfile, $paths[$#paths]);
    @lines=split("\n", $lines);
  }
  my $name="";
  my %name_map;
  foreach my $l (@lines){
    chomp($l);
    if($l=~/^=(\S+)/){
      push @{$name_map{$name}}, $1;
    }elsif($l=~/(\S+)/){
      $name=$1;
      @{$name_map{$name}}=();
    }
  }
  return %name_map;
}
#ziggurat bumps by the argument of threshPercent
#output corresponding .bumpzig file for each scenario
sub ziggurat_bumps_by_instpath{
    my ($working_dir,$zig_near_end,$power_sag,$Vdd,$alint_dir,$node,$node_instname,$threshPercentRef,$scenarios) = @_;
    # $node is digital alias; $node_instname is the name with instance path to the transistor node.
    my $zig_raw="";
    my @curve_names=();
    my %bumpdir=("bump_dn",0,
                 "bump_up",1);
    my %analog_name_map=();
    my @threshPercent=sort {$a <=> $b} @$threshPercentRef;
    
    my %VddPercent=();
    foreach my $perc (@threshPercent){
      push @{$VddPercent{"bump_up"}}, ($Vdd*$perc/100);
      push @{$VddPercent{"bump_dn"}}, $Vdd-($Vdd*$perc/100);
    }
    

    free_aplot();
    foreach my $cctaucap (keys %$scenarios){
        foreach my $bump_updn (keys %{$scenarios->{$cctaucap}}){
            my $fileprefix="$node_instname:$bump_updn:$cctaucap";
            next if (-e "$working_dir/$fileprefix.bumpzig");
            my $zig_wf="";
            my $zig_wf_s="";
            my $zig_wf_nd="";
            my $zig_wf_s_nd="";
            my $run="$bump_updn:$cctaucap";
            my $file="$alint_dir/$node/$run";
            my $filecmd=aplot_file_cmd($file);
            if($file eq $filecmd){
              if (! -e "$file.trace" ) {
                 print "no trace file: $file.trace\n" if ($DEBUG);
                 next;
              }
            }else{
              $file=$filecmd;
            }
            #read names file to get analog name
            %analog_name_map=thresh_name_maps( "$alint_dir/$node/$run.names") if (! %analog_name_map);
            #get all analog names with postfix :F<d+>
            if(! @curve_names){
              my @analog_names=();
              my $digital_name="";
              foreach my $n (keys %analog_name_map){
                 my $node_fix = $node_instname;
                 $node_fix =~ s/\[/\\\[/g;
                 $node_fix =~ s/\]/\\\]/g;
                 $node_fix =~ s/\./\\\./g;
                push @analog_names, $n if($n=~/^$node_fix:F(\d+)/);
                foreach my $alias (@{$analog_name_map{$n}}){
                  push @analog_names, $n if($n=~/^$node_fix:F(\d+)/);
                }
                if($n !~ /:F(\d+)$/){
                  if($node_instname eq $n){ #digital alias
                     $digital_name=$node_instname;
                  }
                }
              }
              if(! @analog_names){
                print STDERR "Warning: This $node_instname does not have analog name. Use digital alias $node!!\n";
                $digital_name=$node;
              }
              if($digital_name ne ""){
                @curve_names=($digital_name);
              }else{
                @curve_names=@analog_names;
              }
            }

            print "file \"$file\"\n" if($DEBUG);
            write_aplot "file \"$file\"";
            my $start=0;
            my $end=0;
            my @timearray=@{$scenarios->{$cctaucap}->{$bump_updn}};
            my $last_time=0;
            my $bumpdir=$bumpdir{$bump_updn};

            # find farest or nearest curve
            my $find_curve=sub{
              my ($start,$end,$near_far)=@_; #near_far: 0:near, 1:far
              my $bump_time=undef; my $bump_v=undef;
              my $curve;
              # assume the curve with highest voltage is the worst bump curve
              foreach my $cn (@curve_names){
                 my ($bump_t_,$bump_v_)=measure_bump_height($cn,$start,$end,$bumpdir);
                 if(($bumpdir and $near_far) or (!$bumpdir and $near_far)){
                    if(not defined $bump_v or $bump_v_ > $bump_v){
                      $bump_v=$bump_v_;
                      $bump_time=$bump_t_;
                      $curve=$cn;
                    }
                 }else{
                    if(not defined $bump_v or $bump_v_ < $bump_v){
                      $bump_v=$bump_v_;
                      $bump_time=$bump_t_;
                      $curve=$cn;
                    }
                 }
              }
              return ($curve,$bump_time,$bump_v);
            };
            for(my $i=0; $i<scalar(@timearray);$i++) {
               $start=$timearray[$i];
               if($i==$#timearray){
                   $last_time=1;
                   $end=$start+5;
               }else{
                   $end=$timearray[$i+1];
               }
               print "Ziggurat bump in range:$start ~ $end of $run\n" if($DEBUG);
               
               my ($worst_curve,$bump_time,$bump_v)=$find_curve->($start,$end,1);
               $zig_wf .= "#BUMP INFO: $node $bump_updn $cctaucap $bump_v $bump_time\n";
               $zig_wf .= "#NODE: bump curve analog name: $worst_curve\n";
               $zig_wf .= "#FORMAT: time voltage VddPercentage\n";
               $zig_wf_s .= "#BUMP INFO: $node $bump_updn $cctaucap $bump_v $bump_time $power_sag%\n";
               $zig_wf_s .= "#NODE: bump curve analog name: $worst_curve\n";
               $zig_wf_s .= "#FORMAT: time voltage VddPercentage\n";
               my $startV=$Vdd; $startV=0 if($bumpdir==1);
               $zig_wf .= transfer_bump2ziggurat(0,0,$startV,$worst_curve,$bumpdir,$start,$end,$bump_v,$bump_time,\@{$VddPercent{$bump_updn}},\@threshPercent,$last_time);
               my ($near_curve,$bump_time_n,$bump_v_n)=$find_curve->($start,$end,0) if ($zig_near_end);
               if($zig_near_end){
                  $zig_wf_nd .= "#BUMP INFO: $node $bump_updn $cctaucap $bump_v_n $bump_time_n nearest_end\n";
                  $zig_wf_nd .= "#NODE: bump curve analog name: $near_curve\n";
                  $zig_wf_nd .= "#FORMAT: time voltage VddPercentage\n";
                  $zig_wf_nd .= transfer_bump2ziggurat(1,0,$startV,$near_curve,$bumpdir,$start,$end,$bump_v_n,$bump_time_n,\@{$VddPercent{$bump_updn}},\@threshPercent,$last_time);
               }
               if($power_sag>0){
                  my $power_sag_v=$Vdd*($power_sag/100);
                  $zig_wf_s .= transfer_bump2ziggurat(0,$power_sag_v,$startV,$worst_curve,$bumpdir,$start,$end,$bump_v,$bump_time,\@{$VddPercent{$bump_updn}},\@threshPercent,$last_time);
                  if($zig_near_end){
                    $zig_wf_s_nd .= "#BUMP INFO: $node $bump_updn $cctaucap $bump_v $bump_time $power_sag% nearest_end\n";
                    $zig_wf_s_nd .= "#NODE: bump curve analog name: $near_curve\n";
                    $zig_wf_s_nd .= "#FORMAT: time voltage VddPercentage\n";
                    $zig_wf_s_nd .= transfer_bump2ziggurat(1,$power_sag_v,$startV,$near_curve,$bumpdir,$start,$end,$bump_v_n,$bump_time_n,\@{$VddPercent{$bump_updn}},\@threshPercent,$last_time);
                  }
               }
               print "Ziggurat bump height:$bump_v @ $bump_time\n" if($DEBUG);
            }#for(my $i=0; $i<scalar(@timearray);$i++)
            open BZG, ">$working_dir/$fileprefix.bumpzig" or die "cannot write file $working_dir/$fileprefix.bumpzig\n";
            print BZG $zig_wf;
            close BZG;
            if($zig_near_end){
              open BZG, ">$working_dir/$fileprefix.bumpzig.nd" or die "cannot write file $working_dir/$fileprefix.bumpzig.nd\n";
              print BZG $zig_wf_nd;
              close BZG;
            }
            if($power_sag>0){
              $fileprefix.=":s$power_sag";
              open BZG, ">$working_dir/$fileprefix.bumpzig" or die "cannot write file $working_dir/$fileprefix.bumpzig\n";
              print BZG $zig_wf_s;
              close BZG;
              if($zig_near_end){
              open BZG, ">$working_dir/$fileprefix.bumpzig.nd" or die "cannot write file $working_dir/$fileprefix.bumpzig.nd\n";
              print BZG $zig_wf_s_nd;
              close BZG;
              }
            }
        }#foreach my $bump_updn (keys %{$scenarios->{$cctaucap}})
    }
   # cleanUp_workdir($extract_workdir) if (-d $extract_workdir);

}

# measure the avg value in range
sub measure_avg {
    my ($run,$node,$from,$to) = @_;
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range $from $to";
    write_aplot "avg $node";
    my $out = read_aplot();
    flush_aplot();
    return split(" ",$out);
}

# measure the rms value in range
sub measure_rms {
    my ($run,$node,$from,$to) = @_;
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range $from $to";
    write_aplot "rms $node";
    my $out = read_aplot();
    flush_aplot();
    return split(" ",$out);
}

# measure the peak value in range
sub measure_peak {
    my ($run,$node,$from,$to) = @_;
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range $from $to";
    write_aplot "minv $node";
    my ($p1_t, $p1) = split(" ", read_aplot());
    my $ph=$p1/2;
    if($p1 > 0){
      write_aplot "mindelay $node > $ph  $node < $ph ";
    }else{
      write_aplot "mindelay $node < $ph  $node > $ph ";
    }
    (my $p1_et, $p1_t) = split(" ", read_aplot());
    
    write_aplot "maxv $node";
    my ($p2_t, $p2) = split(" ", read_aplot());
    $ph=$p2/2;
    if($p2 > 0){
      write_aplot "mindelay $node > $ph  $node < $ph ";
    }else{
      write_aplot "mindelay $node < $ph  $node > $ph ";
    }
    (my $p2_et, $p2_t) = split(" ", read_aplot());

    flush_aplot();
    $p1=abs($p1); $p2=abs($p2);
    return ($p1_t, $p1) if($p1>= $p2);
    return ($p2_t, $p2);
}

# measure average and peak power consumption
sub measure_power {
    my ($run, $true, $reset) = @_;
    $reset *= 1e9; # convert from s to ns
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range $reset";
    write_aplot "avg IVdd|";
    my $m = read_aplot();
    flush_aplot();
    my ($t,$I) = split(" ",$m);
    my $avg = "FAIL";
    if (is_numeric($I)) { $avg = $I * $true; }
    write_aplot "maxv IVdd|";
    $m = read_aplot();
    flush_aplot();
    ($t,$I) = split(" ",$m);
    my $peak = "FAIL";
    if (is_numeric($I)) { $peak = $I * $true; }
    return ($avg, $peak);
}

# measure frequency from trace file with aplot
sub measure_frequency {
    my ($run, $node, $thresh, $start) = @_;
    $start *= 1e9; # convert from s to ns
    
    # set file and range
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range $start";
    
    # get number of transitions
    write_aplot "trigger $node < $thresh";
    my $cycles = read_aplot();
    flush_aplot();
    if (!is_numeric($cycles)) { return ("FAIL",0); }
    
    # measure frequency
    write_aplot "trigger $node < $thresh 1 $cycles";
    my $freq = read_aplot();
    flush_aplot();
    if (!is_numeric($freq)) { return ("FAIL",0); }
    ($freq) = split(" ",$freq);
    
    # return results
    return ($freq * 1e9, $cycles - 1); # adjust to Hz and cycle offset
}

# arbitrary measure command
sub measure_aplot {
    my ($run, $from, $to, $command) = @_;

    # set file and range
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range $from $to";
    
    # measure
    write_aplot "$command";
    my $out = read_aplot();
    flush_aplot();
    return split(" ",$out);
}

# measure frequency from history file
sub measure_frequency_from_history {
    my ($file, $start, $digital_time_unit) = @_;
    my $first_time;
    my $first_tcount;
    my $last_time;
    my $last_tcount;
    open HISTORY, "<$file" or die "Cannot read $file.\n";
    while (my $line = <HISTORY>) {
        if ($line =~ /^ \@(\d+) \#(\d+) (\S+)\:(\d)+/) {
            my $time   = $1 * $digital_time_unit;
            my $tcount = $2;
            my $node   = $3;
            my $value  = $4;
            if (($value == 0) && ($time >= $start)) { # falling after $start time
                if (!defined($last_tcount)) {
                    $last_time   = $time;
                    $last_tcount = $tcount;
                }
                $first_time   = $time;
                $first_tcount = $tcount;
            }
        }
    }
    close HISTORY;
    if (!defined($first_tcount) || !defined($first_time) ||
        !defined($last_tcount)  || !defined( $last_time) ||
        ($last_time <= $first_time)) {
        return ("FAIL",0);
    }
    my $cycles = ($last_tcount - $first_tcount)/2;
    my $freq   = $cycles / ($last_time - $first_time);
    return ($freq, $cycles);
}

# measure skew between resistive subnets at given voltage threshold
sub measure_skew {
    my ($run,$node,$midV) = @_;
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range";
    write_aplot "skew $node $midV";
    my $out = read_aplot();
    flush_aplot();
    return split(" ",$out);
}

# measure min/max slew for direction (>/<) between two voltage thresholds
sub measure_slew {
    my ($run, $minmax, $dir, $node, $fromV, $toV) = @_;
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    write_aplot "range";
    write_aplot "$minmax $node $dir $fromV $node $dir $toV";
    my $out = read_aplot();
    flush_aplot();
    return split(" ",$out);
}

# return list of fanin gates for alint scenarios
sub read_fanin {
    my ($file) = @_;
    my @fanin = ();
    if($file=~/\.gz$/){
      open (IN, "gzip -cd \'$file\'|") or die "Cannot read $file.\n";
    }else{
      open (IN, "<$file") or die "Cannot read $file.\n";
    }
    while (my $line = <IN>) {
        if ($line =~ /^  gate (\S+)/) {
            push @fanin, $1;
        }
    }
    close IN;
    return @fanin;
}

# return list of inverses of the victim
sub read_inverses {
    my ($file) = @_;
    my @inverses = ();
    if($file=~/\.gz$/){
      open (IN, "gzip -cd \'$file\'|") or die "Cannot read $file.\n";
    }else{
      open (IN, "<$file") or die "Cannot read $file.\n";
    }
    while (my $line = <IN>) {
        if ($line =~ /^  inverse (\S+)/) {
            push @inverses, $1;
        }
    }
    close IN;
    return @inverses;
}

# return list of named resistors, if any
sub read_named_resistors {
    my ($file) = @_;
    my @resistors = ();
    if($file=~/\.gz$/){
      open (IN, "gzip -cd \'$file\'|") or die "Cannot read $file.\n";
    }else{
      open (IN, "<$file") or die "Cannot read $file.\n";
    }
    while (my $line = <IN>) {
        if ($line =~ /^  res ([^\(]+)\(.*\((\S+)\)/ ) {
            push @resistors, "$1 $2";
        }
    }
    close IN;
    return @resistors;
}

# return hash of test:cc:suffix to string of space separated start times
sub read_start_times {
    my ($file) = @_;
    my %times;
    my $test = "";
    my $start = 0;
    if($file=~/\.gz$/){
      open (IN, "gzip -cd \'$file\'|") or die "Cannot read $file.\n";
    }else{
      open (IN, "<$file") or die "Cannot read $file.\n";
    }
    while (<IN>) {
        if ($test eq "" && /^Alint (\S+) simulation \(CC=(\d) Tau=([\d\.e-]+) Cap=([\d\.e-]+)\):$/) {
            $test = "$1:$2:" . int($3/1e-12 + 0.5);
            $test .= ":".sprintf("%g", $4*1e15) if $4 > 0;
            $times{$test} = "";
        }
        elsif ($test eq "" && /^Alint (\S+) simulation \(CC=(\d) Tau=([\d\.e-]+)\):$/) {
            # for re-raw
            $test = "$1:$2:" . int($3/1e-12 + 0.5);
            $times{$test} = "";
        }
        elsif ($test ne "" && /^  \S+\s+([\d\.]+)/) {
            if ($times{$test} ne "") { $times{$test} .= " "; }
            $times{$test} .= $1;
        }
        else {
            $test = "";
        }
    }
    close IN;
    return %times;
}


# measure min_avg, max_avg, max_rms for all scenario ranges
sub measure_worst_avg_rms {
    my ($file,$node,@times) = @_;
    my $min_avg;
    my $max_avg;
    my $max_rms;
    my $max_peak;
    for (my $i = 0; $i<@times; $i++) {
        my $from = $times[$i];
        my $to = ""; # end of simulation
        if ($i+1<@times) { $to = $times[$i+1]; }
        my ($wavg,$avg) = measure_avg($file,$node,$from,$to);
        $wavg *= 1e-9; # ns to s
        $avg = $avg * $wavg;
        my ($wrms,$rms) = measure_rms($file,$node,$from,$to);
        $wrms *= 1e-9; # ns to s
        $rms = $rms * $rms * $wrms;
        my ($wpeak,$peak) = measure_peak($file,$node,$from,$to);
        $wpeak *= 1e-9; # ns to s
        $peak = $peak * sqrt($wpeak);
        if (!defined($min_avg) || ($avg < $min_avg)) { $min_avg = $avg; }
        if (!defined($max_avg) || ($avg > $max_avg)) { $max_avg = $avg; }
        if (!defined($max_rms) || ($rms > $max_rms)) { $max_rms = $rms; }
        if (!defined($max_peak) || ($peak > $max_peak)) { $max_peak = $peak; }
    }
    print AD "measure_worst_avg_rms $file $node $min_avg, $max_avg, $max_rms, $max_peak\n"
        if $DEBUG;
    # avg here is actually the integral of the value
    # rms here is actually the integral of the square
    return ($min_avg, $max_avg, $max_rms, $max_peak);
}


# measure min/max delay from any fanin nodes to victim node
sub measure_delay {
    my ($run, $minmax, $trigdir, $targdir, $node, $trigV, $targV, $timeref, @fanin) = @_;
    local $"=" ";
    $run=aplot_file_cmd($run);
    write_aplot "file \"$run\"";
    my ($delay, $delay_t)=(undef,undef);
    
    my @starttime = @$timeref;

    for(my $i=0;$i<scalar(@starttime);$i++){
        my $start=$starttime[$i];
        my $end="";
        $end=":$starttime[$i+1]" if (defined $starttime[$i+1]);
        write_aplot "range $start$end";
        print "range $start$end\n" if($DEBUG);
        my $minmax_cmd=$minmax;
        write_aplot "$minmax_cmd @fanin $trigdir $trigV $node $targdir $targV";

        my $out = read_aplot();

        if (($out =~ /WARNING/i)) {
            flush_aplot();
            if ($minmax_cmd eq "maxdelay") {
                $minmax_cmd = "mindelay";
            }
            elsif ($minmax_cmd eq "mindelay") {
                $minmax_cmd = "maxdelay";
            }
            write_aplot "$minmax_cmd $node $targdir $targV @fanin $trigdir $trigV";
            $out = read_aplot();
            if (($out =~ /WARNING/i)) {
              flush_aplot();
              if ($minmax_cmd eq "maxdelay") {
                  $minmax_cmd = "mindelay";
              }
              elsif ($minmax_cmd eq "mindelay") {
                  $minmax_cmd = "maxdelay";
              }
              write_aplot "$minmax_cmd $node $targdir $targV @fanin $trigdir $trigV";
              $out = read_aplot();
            }
            my ($a,$b)=split(" ", $out);
            if (is_numeric ($a) and is_numeric($b)) {
                $b = -$b;
                flush_aplot();
                if (not defined $delay or ($b > $delay and $minmax eq "maxdelay") or ($b < $delay and $minmax eq "mindelay")){
                  $delay=$b; 
                  $delay_t=$a;
                }
            }
        }else{
            my ($a,$b)=split(" ", $out);
            if (is_numeric ($a) and is_numeric($b)) {
                flush_aplot();
                if (not defined $delay or ($b > $delay and $minmax eq "maxdelay") or ($b < $delay and $minmax eq "mindelay")){
                  $delay=$b;
                  $delay_t=$a;
                }
            }
        }
        flush_aplot();

    }
    return ("NA","NA") if(not defined $delay);
    return ($delay_t, $delay);
}

sub ncmp {
    $a-$b;
}

# use aplot to make a png picture of a set of nodes
sub make_png {
    local $" = " ";
    my $file = shift;
    my $title = shift;
    my $run = shift;
    my @nodes = @_;

    my $run_trace=aplot_file_cmd($run);
    my $run_names=aplot_file_cmd($run,"names");
    if($run eq $run_trace and $run eq $run_names){
      return if (! -e "$file.trace" && -e "$run.trace" );
    }else{
      $run=$run_trace;
    }
        write_aplot "file \"$run\"";
        write_aplot "range";
        write_aplot "\\set term png";
        write_aplot "\\set title \"$title\"";
        write_aplot "\\set out \"$file\"";
        write_aplot "trace @nodes";
        flush_aplot();
}

# consolidate coverage across multiple environments
sub coverage_merging {
    my %coverage_info = ();
    my $outfile = shift;
    my @tcounts_files = @_;
    foreach my $tcount_file (@tcounts_files){
	open my $tcount_filehandle, "<$tcount_file" or 
	    die "Cannot open \"$tcount_file\"\n";
	while(<$tcount_filehandle>){
	    if($_ =~ /^ @(\d+) \#(\d+) (\S+)\:(\d+)$/){
		my $tcount = $2;
		my $node   = $3;
		if(!defined($coverage_info{$node})){
		    $coverage_info{$node} = $tcount;
		}
		else {
		    $coverage_info{$node} = $coverage_info{$node} + $tcount;
		}
	    }
	}
    }
    my @node_keys = keys(%coverage_info);
    open OUTFILE, ">$outfile" or 
	die "Cannot open $outfile\n";
    my $i =0;
    foreach my $node_key(@node_keys){
	if($coverage_info{$node_key} > 2){ $i++; }
	print OUTFILE "$node_key: $coverage_info{$node_key}\n";
    }
    my $total = scalar(keys(%coverage_info));
    close OUTFILE;
    return ($i/scalar(keys(%coverage_info))*100);
}


our @DIR_STR = ( "-", "+" );

#
# Post-processes the alint delay scenarios for a particular cell and
# constructs a table of the following data:
#
#  halfop (characterized node + transition direction)
#      -> input_halfop (input transition scenario)
#      -> input_slew (input slew rate, NOTE: still in units of prsTau)
#      -> [ slow_delay, slow_slew, fast_delay, fast_slew ]
#
# Note: aplot pipe must be open.
#
sub characterize_delay {
    my ($alint_dir,$localprops,$slew_scaling,%skipnodes) = @_;
    my %data;
    %skipnodes=() if ! %skipnodes;
    my $alt_alint_dir = $alint_dir;
    $alt_alint_dir =~ s/\.tmp$//;

    $localprops = $alint_dir if ! defined $localprops;
    my @localnodes=();
    if ( -f "$localprops" or -l "$localprops") {
        @localnodes = read_localnodes($localprops);
    }
    else {
        @localnodes = ($localprops);
    }

    for my $node (@localnodes) {
        next if $skipnodes{$node};
        free_aplot();
        my @slow_str  = ( "slow_dn", "slow_up" );
        my @fast_str  = ( "fast_dn", "fast_up" );
        my $out_file="$alint_dir/$node/out";
        $out_file="$alint_dir/$node/out.gz" if(-e "$alint_dir/$node/out.gz");
        if(! -e $out_file){
           $out_file="$alt_alint_dir/$node/out" if(-e "$alt_alint_dir/$node/out");
           $out_file="$alt_alint_dir/$node/out.gz" if(-e "$alt_alint_dir/$node/out.gz");
        }
        if (! -e $out_file) {
            warn "No alint data found for local node $node. Skipping " .
                 "delay characterization.\n";
            next;
        }
        my %scenarios=();
        %scenarios = read_delay_scenarios("$out_file",0);

        (my $re_node = $node) =~ s/\[/\\\[/g; $re_node =~ s/\]/\\\]/g;

        my ($is_archive,@node_files)=is_node_archive("$alint_dir/$node",$trace_archive);  

        if($is_archive==0){
          # find all files for this node
          opendir DIR, "$alint_dir/$node" ||
              die "Couldn't read alint directory $alint_dir/$node.\n";
           @node_files = readdir DIR;
          closedir DIR;
        }
        for my $dir (0..1) {
            print "Processing half-op $node$LveAspice::DIR_STR[$dir]\n" 
                if ($DEBUG);
            my (@slews, @slow_ranges, @fast_ranges);
            foreach my $trace (@node_files) {
                if ($trace =~ /$slow_str[$dir]:0:([\d\.]+)\.trace$/) {
                    push @slews, $1;
                    push @slow_ranges, $scenarios{"$slow_str[$dir]:0:$1"};
                    push @fast_ranges, $scenarios{"$fast_str[$dir]:0:$1"};
                }
            }
# BUG 6548, remove this warning
#            warn "Warning: No slow delay data for half-operator " .
#                 "$node$LveAspice::DIR_STR[$dir].\n" if (@slews == 0);
            warn "Warning: Only one (slow) slew rate point for half-operator " .
                 "$node$LveAspice::DIR_STR[$dir].\n" if (@slews == 1);

            #@slews = sort { $a <=> $b } @slews;
            my %slow_data = measure_transitions($node, $dir,
                "$alint_dir/$node/$slow_str[$dir]:0", 
                \@slow_ranges, 1, $slew_scaling, @slews);
            my %fast_data = measure_transitions($node, $dir,
                "$alint_dir/$node/$fast_str[$dir]:0", 
                \@fast_ranges, 0, $slew_scaling, @slews);

            my %charge_data = measure_charge($node, $dir,
                "$alint_dir/$node/$fast_str[$dir]:0", 
                \@fast_ranges, $slew_scaling, @slews);

            # Store slow & fast delays
            my %all;
            foreach my $ho (keys %slow_data) {
                $all{$ho}=1;
            }
            foreach my $ho (keys %fast_data) {
                $all{$ho}=1;
            }
            foreach my $halfop_in (keys %all) {
                my %slow_slewdata;
                my %fast_slewdata;
                if ( ! defined $fast_data{$halfop_in}) {
# BUG 6548, remove this warning
#                    warn "Mismatch between slow/fast scenarios found for " .
#                         "$halfop_in -> $node$LveAspice::DIR_STR[$dir]\n";
                    %fast_slewdata = %{$slow_data{$halfop_in}};
                }
                else {
                    %fast_slewdata = %{$fast_data{$halfop_in}};
                }
                if ( ! defined $slow_data{$halfop_in}) {
# BUG 6548, remove this warning
#                    warn "Mismatch between slow/fast scenarios found for " .
#                         "$halfop_in -> $node$LveAspice::DIR_STR[$dir]\n";
                    %slow_slewdata = %{$fast_data{$halfop_in}};
                }
                else {
                    %slow_slewdata = %{$slow_data{$halfop_in}};
                }
                my %fast_charge_data;
                if(defined $charge_data{$halfop_in}){
                    %fast_charge_data=%{$charge_data{$halfop_in}};
                }else{
                    #Use slow one if there is mismatch between slow/fast
                    %charge_data = measure_charge($node, $dir,
                                "$alint_dir/$node/$slow_str[$dir]:0", 
                                  \@slow_ranges, $slew_scaling, @slews);
                    %fast_charge_data=%{$charge_data{$halfop_in}};
                }
                foreach my $slew (keys %slow_slewdata) {
                    my $slow_delay = ${$slow_slewdata{$slew}}[0];
                    my $fast_delay = ${$fast_slewdata{$slew}}[0];
                    my $slow_slew  = ${$slow_slewdata{$slew}}[1];
                    my $fast_slew  = ${$fast_slewdata{$slew}}[1];
                    my ($gcharge, $gwidth, $vcharge, $vwidth) =
                        @{$fast_charge_data{$slew}};

                    print "MCD $slew $halfop_in $gcharge, $gwidth $vcharge $vwidth\n" if $DEBUG;
                    # Keep track of max delay/slew, min delay/slew and charge parameters
                    ${${$data{"$node$LveAspice::DIR_STR[$dir]"}}{$halfop_in}}{$slew} = 
                        [ $slow_delay, $slow_slew, $fast_delay, $fast_slew, $gcharge, $gwidth, $vcharge, $vwidth ];
                }
            } # halfop
        } # dir
    } # nodes
    return %data;
}

#characterize_cap_delay is called before archive
sub characterize_cap_delay {
    my ($alint_dir,$node,$cap,$slew_scaling) = @_;
    my $alt_alint_dir=$alint_dir;
    $alt_alint_dir =~ s/\.tmp$//;
    my %data=();
    local (*DIR);
    $cap = sprintf "%g", $cap/1e-15;

    free_aplot();
    my @slow_str  = ( "slow_dn", "slow_up" );
    my $out_file="$alt_alint_dir/$node/out";
    $out_file="$alt_alint_dir/$node/out.gz" if(-e "$alt_alint_dir/$node/out.gz");
    if (! -e "$out_file") {
        warn "No alint data found for local node $node. Skipping " .
             "delay characterization.\n";
        return %data;
    }
    my %scenarios = read_delay_scenarios("$out_file",0);

    (my $re_node = $node) =~ s/\[/\\\[/g; $re_node =~ s/\]/\\\]/g;

    # find all files for this node
    opendir DIR, "$alint_dir/$node" ||
        die "Couldn't read alint directory $alint_dir/$node.\n";
    my @node_files = readdir DIR;
    closedir DIR;


    my $hastrace = 0;
    my $hasdirectives=0;
    foreach my $file (@node_files) {
        if ($file =~ /\.trace$/) {
            $hastrace=1;
        }
        if ($file =~ /^directives\./ and -s "$alint_dir/$node/$file") {
            $hasdirectives=1;
        }
    }
    if (! $hastrace) {
        warn "No alint data found for local node $node. Skipping " .
             "delay characterization.\n" if ! $hasdirectives;
        return %data;
    }

    for my $dir (0..1) {
        print "Processing half-op $node$LveAspice::DIR_STR[$dir]\n" 
            if ($DEBUG);
        my (@slews, @slow_ranges);
        foreach my $trace (@node_files) {
            if ($trace =~ /$slow_str[$dir]:0:([\d\.]+):$cap\.trace$/) {
                push @slews, $1;
                push @slow_ranges, $scenarios{"$slow_str[$dir]:0:$1:$cap"};
            }
        }
        warn "Warning: Only one (slow) slew rate point for half-operator " .
             "$node$LveAspice::DIR_STR[$dir].\n" if (@slews == 1);

        #@slews = sort { $a <=> $b } @slews;
        my %slow_data = measure_transitions_wcap($node, $dir,
            "$alint_dir/$node/$slow_str[$dir]:0", 
            \@slow_ranges, 1, $cap, $slew_scaling, @slews);

        my %fast_data = measure_transitions_wcap($node, $dir,
            "$alint_dir/$node/$slow_str[$dir]:0", 
            \@slow_ranges, 0, $cap, $slew_scaling, @slews);

        my %charge_data = measure_charge_wcap($node, $dir,
            "$alint_dir/$node/$slow_str[$dir]:0", 
            \@slow_ranges, $cap, $slew_scaling, @slews);

        # Store slow & fast delays
        my %all;
        foreach my $ho (keys %slow_data) {
            $all{$ho}=1;
        }
        foreach my $ho (keys %fast_data) {
            $all{$ho}=1;
        }
        foreach my $halfop_in (keys %all) {
            my %slow_slewdata;
            my %fast_slewdata;
            %fast_slewdata = %{$fast_data{$halfop_in}};
            %slow_slewdata = %{$slow_data{$halfop_in}};
            my %fast_charge_data=%{$charge_data{$halfop_in}};
            foreach my $slew (keys %slow_slewdata) {
                my $slow_delay = ${$slow_slewdata{$slew}}[0];
                my $fast_delay = ${$fast_slewdata{$slew}}[0];
                my $slow_slew  = ${$slow_slewdata{$slew}}[1];
                my $fast_slew  = ${$fast_slewdata{$slew}}[1];
                my ($gcharge, $gwidth, $vcharge, $vwidth) =
                    @{$fast_charge_data{$slew}};

                # Keep track of max delay/slew, min delay/slew
                ${${$data{"$node$LveAspice::DIR_STR[$dir]"}}{$halfop_in}}{$slew} = 
                    [ $slow_delay, $slow_slew, $fast_delay, $fast_slew, $gcharge, $gwidth, $vcharge, $vwidth ];
            }
        }
    }
    return %data;
}


#
# Measures the transitions of 'node' to 'dir' for each input of 'node'
# over the slew rates of 'slews' and ranges specified in 'ranges_ref' 
# (a map of slew index -> input -> list of ranges).  If 'min_or_max' is
# 0, the minimum transition delays and slew rates are measured; if it's
# 1, the max values are measured.
# 
sub measure_transitions {
    my ($node, $dir, $file_base, $ranges_ref, $min_or_max, $slew_scaling, @slews) = @_;
    my $vdd = get_nth_run_param($file_base,3); $vdd =~ s/V$//;
    my @aplot_cmd    = ( "mindelay", "maxdelay" );
    my @aplot_dir    = ( "<", ">" );
    my @vth          = ( 2*$vdd/3, $vdd/3 );
    my %data;
    my $max_or_min;

    $max_or_min = $min_or_max == 1 ? 0 : 1;

    for my $i (0..$#slews) {
        my $file = "$file_base:$slews[$i]";
        my $filecmd=aplot_file_cmd($file);
        if($file eq $filecmd){
          if (! -e "$file.trace") {
# BUG 6548: assumes it is ok to miss this file.
#           warn "Warning: No data for $file_base, slew rate $slews[$i].\n";
            next;
          }
        }else{
          $file=$filecmd;
        }
        print "File = $file\n" if ($DEBUG);
        write_aplot("file \"$file\"");
        my $hashref = $$ranges_ref[$i];
        for my $input (keys %$hashref) {
            my ($minmax_delay, $minmax_slew);
            $minmax_delay = $minmax_slew = 1e6;
            if ($min_or_max != 0) {
                $minmax_delay = -$minmax_delay;
                $minmax_slew = -$minmax_slew;
            }
            #$NON_CONTIGUOUS += $#{$hashref->{$input}};
            for my $r (0..$#{$hashref->{$input}}) {
                my $range = "[" . $hashref->{$input}->[$r]->[0] . ":" .
                                  $hashref->{$input}->[$r]->[1] . "]";
                print " Input = $input, Range = $range\n" if ($DEBUG);
                write_aplot("range $range");
                my $aplot_cmd = "$aplot_cmd[$min_or_max] " .
                                "$input $aplot_dir[1-$dir] $vth[1-$dir] " .
                                "$node $aplot_dir[$dir] $vth[$dir]";
                print "$aplot_cmd\n" if ($DEBUG);
                write_aplot($aplot_cmd);
                my $out = read_aplot();
                print "$out\n" if $DEBUG;
                if ($out =~ /^WARNING/) {
                    flush_aplot();
                    $aplot_cmd = "$aplot_cmd[$max_or_min] " .
                            "$node $aplot_dir[$dir] $vth[$dir] " .
                            "$input $aplot_dir[1-$dir] $vth[1-$dir]";
                    print "RETRY $aplot_cmd\n" if $DEBUG;
                    write_aplot ($aplot_cmd);
                    $out = read_aplot();
                    print "$out\n" if $DEBUG;
                    if ($out =~ /^WARNING/i) {
                        $out = "0 0";
                    }
                    else {
                        my ($t, $d) = split /\s+/, $out;
                        $d = -$d;
                        $out = "$t $d";
                    }
                }
                flush_aplot();
                my ($time0, $delay) = split /\s+/, $out;
                if ($time0 eq "WARNING:") {
                    warn "Unexpected no trigger.\n";
                    next;
                }
                if (!is_numeric($delay)) {
                    warn "Encountered non-numeric $delay for node $node in " .
                         "$file, input $input.\nUsing delay of 0.";
                    warn "cmd = $aplot_cmd\n";
                    warn "response = $out\n";
                    $delay = 0;
                }

                # Measure slew rate
                $aplot_cmd = "$aplot_cmd[$min_or_max] " .
                             "$input $aplot_dir[1-$dir] $vth[1-$dir] " .
                             "$node $aplot_dir[$dir] $vth[1-$dir]";
                print "$aplot_cmd\n" if $DEBUG;
                write_aplot($aplot_cmd);
                $out = read_aplot();
                print "$out\n" if $DEBUG;
                if ($out =~ /^WARNING/) {
                    flush_aplot();
                    $aplot_cmd = "$aplot_cmd[$max_or_min] " .
                             "$node $aplot_dir[$dir] $vth[1-$dir] ".
                             "$input $aplot_dir[1-$dir] $vth[1-$dir]";
                    print "RETRY $aplot_cmd\n" if $DEBUG;
                    write_aplot ($aplot_cmd);
                    $out = read_aplot();
                    print "$out\n" if $DEBUG;
                    if ($out =~ /^WARNING/i) {
                        $out = "0 0";
                    }
                    else {
                        my ($t, $d) = split /\s+/, $out;
                        $d = -$d;
                        $out = "$t $d";
                    }
                }
                flush_aplot();
                my ($time1, $slew_delay) = split /\s+/, $out;
                if ($time1 eq "WARNING:") {
                    warn "Unexpected no trigger (slew).\n";
                    next;
                }
                if (!is_numeric($slew_delay)) {
                    warn "Encountered non-numeric $slew_delay for node $node in " .
                         "$file, input $input.\n" .
                         "** Using slew rate of 0 **";
                    warn "cmd = $aplot_cmd\n";
                    warn "response = $out\n";
                    $slew_delay = $delay;
                }
                $slew_delay -= $delay;
                $slew_delay *= 1e3;
                $delay      *= 1e3;
                if ($min_or_max == 0) {
                    $minmax_delay = $delay if ($delay < $minmax_delay);
                    $minmax_slew  = $slew_delay if ($slew_delay < $minmax_slew);
                }
                else {
                    $minmax_delay = $delay if ($delay > $minmax_delay);
                    $minmax_slew  = $slew_delay if ($slew_delay > $minmax_slew);
                }
            }
            print "MX $minmax_delay $minmax_slew\n" if $DEBUG;
            ${$data{"$input$LveAspice::DIR_STR[$dir-1]"}}{$slews[$i]*$slew_scaling} = 
                [ $minmax_delay, $minmax_slew ];
        }
    }
    #die "No data found for $file_base.\n" if (! %data);
    return %data;
}

sub measure_charge_wcap {
    my ($node, $dir, $file_base, $ranges_ref, $cap, $slew_scaling, @slews) = @_;
    my %data;

    for my $i (0..$#slews) {
        my $file = "$file_base:$slews[$i]";
        $file .= ":$cap" if defined ($cap) and $cap ne "0";
        my $filecmd=aplot_file_cmd($file);
        if($file eq $filecmd){
          if (! -e "$file.trace") {
# BUG 6548: assumes it is ok to miss this file.
#           warn "Warning: No data for $file_base, slew rate $slews[$i].\n";
            next;
          }
        }else{
          $file=$filecmd;
        }
        print "File = $file\n" if ($DEBUG);
        write_aplot("file \"$file\"");
        my $hashref = $$ranges_ref[$i];
        for my $input (keys %$hashref) {
            #$NON_CONTIGUOUS += $#{$hashref->{$input}};
            my $gcharge=0;
            my $gwidth=0;
            my $vcharge=0;
            my $vwidth=0;
            for my $r (0..$#{$hashref->{$input}}) {
                my $range = "[" . $hashref->{$input}->[$r]->[0] . ":" .
                                  $hashref->{$input}->[$r]->[1] . "]";
                # stupid case of range being incorrect
                if (abs($hashref->{$input}->[$r]->[1] - $hashref->{$input}->[$r]->[0] - 10) < 0.001 ) {
                    $range = "[" . $hashref->{$input}->[$r]->[0] . "]";
                }
                write_aplot("range $range");
                # GND
                write_aplot "avg IGND|";
                my $out = read_aplot();
                my ($a,$b) = split (/ /, $out);
                flush_aplot();
                # default is full width
                $gwidth=$a;
                if (is_numeric ($a) and is_numeric($b)) {
                    $gcharge=$b*$a;
                }
                else {
                    $gcharge=0;
                    $a=0;
                }
                write_aplot "minv IGND|";
                $out = read_aplot();
                my ($dt,$minv,$maxv);
                ($dt,$minv) = split (/ /, $out);
                flush_aplot();
                write_aplot "maxv IGND|";
                $out = read_aplot();
                ($dt,$maxv) = split (/ /, $out);
                flush_aplot();
                my ($t1,$t2);
                if (is_numeric($minv) and is_numeric($maxv)) {
                    write_aplot "trigger IGND| > $b 1";
                    $t1 = read_aplot();
                    flush_aplot();
                    write_aplot "trigger IGND| < $b 1";
                    $t2 = read_aplot();
                    flush_aplot();
                    if (is_numeric($t1) and is_numeric($t2)) {
                        $gwidth=abs($t1-$t2);
                    }
                }
                # Vdd
                write_aplot "avg IVdd|";
                $out = read_aplot();
                ($a,$b) = split (/ /, $out);
                flush_aplot();
                # default is full width
                $vwidth=$a;
                if (is_numeric ($a) and is_numeric($b)) {
                    $vcharge=$b*$a;
                }
                else {
                    $vcharge=0;
                    $a=0;
                }
                write_aplot "minv IVdd|";
                $out = read_aplot();
                ($dt,$minv) = split (/ /, $out);
                flush_aplot();
                write_aplot "maxv IVdd|";
                $out = read_aplot();
                ($dt,$maxv) = split (/ /, $out);
                flush_aplot();
                if (is_numeric($minv) and is_numeric($maxv)) {
                    write_aplot "trigger IVdd| > $b 1";
                    $t1 = read_aplot();
                    flush_aplot();
                    write_aplot "trigger IVdd| < $b 1";
                    $t2 = read_aplot();
                    flush_aplot();
                    if (is_numeric($t1) and is_numeric($t2)) {
                        $vwidth=abs($t1-$t2);
                    }
                }
            }
            print "MCW ". "$input$LveAspice::DIR_STR[$dir-1]" . " " . $slews[$i]*$slew_scaling . " = [ $gcharge, $gwidth , $vcharge, $vwidth ]'\n" if $DEBUG;
            ${$data{"$input$LveAspice::DIR_STR[$dir-1]"}}{$slews[$i]*$slew_scaling} = 
                [ $gcharge, $gwidth, $vcharge, $vwidth ];
        }
    }
    return %data;
}

sub measure_charge {
    my ($node, $dir, $file_base, $ranges_ref, $slew_scaling, @slews) = @_;
    my %data;

 
    for my $i (0..$#slews) {
        my $file = "$file_base:$slews[$i]";
        #explicit use fas_ data if slow is not avaliable
        #next if ! ($file =~ /fast_/);
        my $filecmd=aplot_file_cmd($file);
        if($file eq $filecmd){
          if (! -e "$file.trace") {
# BUG 6548: assumes it is ok to miss this file.
#           warn "Warning: No data for $file_base, slew rate $slews[$i].\n";
            next;
          }
        }else{
          $file=$filecmd;
        }
        print "File = $file\n" if ($DEBUG);
        write_aplot("file \"$file\"");
        my $hashref = $$ranges_ref[$i];
        for my $input (keys %$hashref) {
            #$NON_CONTIGUOUS += $#{$hashref->{$input}};
            my $gcharge=0;
            my $gwidth=0;
            my $vcharge=0;
            my $vwidth=0;
            for my $r (0..$#{$hashref->{$input}}) {
                my $range = "[" . $hashref->{$input}->[$r]->[0] . ":" .
                                  $hashref->{$input}->[$r]->[1] . "]";
                # stupid case of range being incorrect
                if (abs($hashref->{$input}->[$r]->[1] - $hashref->{$input}->[$r]->[0] - 10) < 0.001 ) {
                    $range = "[" . $hashref->{$input}->[$r]->[0] . "]";
                }
                write_aplot("range $range");
                # GND
                write_aplot "avg IGND|";
                my $out = read_aplot();
                my ($a,$b) = split (/ /, $out);
                flush_aplot();
                # default is full width
                $gwidth=$a;
                if (is_numeric ($a) and is_numeric($b)) {
                    $gcharge=$b*$a;
                }
                else {
                    $gcharge=0;
                    $a=0;
                }
                write_aplot "minv IGND|";
                $out = read_aplot();
                my ($dt,$minv,$maxv);
                ($dt,$minv) = split (/ /, $out);
                flush_aplot();
                write_aplot "maxv IGND|";
                $out = read_aplot();
                ($dt,$maxv) = split (/ /, $out);
                flush_aplot();
                my ($t1,$t2);
                if (is_numeric($minv) and is_numeric($maxv)) {
                    write_aplot "trigger IGND| > $b 1";
                    $t1 = read_aplot();
                    flush_aplot();
                    write_aplot "trigger IGND| < $b 1";
                    $t2 = read_aplot();
                    flush_aplot();
                    if (is_numeric($t1) and is_numeric($t2)) {
                        $gwidth=abs($t1-$t2);
                    }
                }
                # Vdd
                write_aplot "avg IVdd|";
                $out = read_aplot();
                ($a,$b) = split (/ /, $out);
                flush_aplot();
                # default is full width
                $vwidth=$a;
                if (is_numeric ($a) and is_numeric($b)) {
                    $vcharge=$b*$a;
                }
                else {
                    $vcharge=0;
                    $a=0;
                }
                write_aplot "minv IVdd|";
                $out = read_aplot();
                ($dt,$minv) = split (/ /, $out);
                flush_aplot();
                write_aplot "maxv IVdd|";
                $out = read_aplot();
                ($dt,$maxv) = split (/ /, $out);
                flush_aplot();
                if (is_numeric($minv) and is_numeric($maxv)) {
                    write_aplot "trigger IVdd| > $b 1";
                    $t1 = read_aplot();
                    flush_aplot();
                    write_aplot "trigger IVdd| < $b 1";
                    $t2 = read_aplot();
                    flush_aplot();
                    if (is_numeric($t1) and is_numeric($t2)) {
                        $vwidth=abs($t1-$t2);
                    }
                }
            }
            print "MC ". "$input$LveAspice::DIR_STR[$dir-1]" . " " . $slews[$i]*$slew_scaling . " = [ $gcharge, $gwidth , $vcharge, $vwidth ]'\n" if $DEBUG;
            ${$data{"$input$LveAspice::DIR_STR[$dir-1]"}}{$slews[$i]*$slew_scaling} = 
                [ $gcharge, $gwidth, $vcharge, $vwidth ];
        }
    }
    return %data;
}

sub measure_transitions_wcap {
    my ($node, $dir, $file_base, $ranges_ref, $min_or_max, $cap, $slew_scaling, @slews) = @_;
    my $vdd          = get_nth_run_param($file_base,3); $vdd =~ s/V$//;
    my @aplot_cmd    = ( "mindelay", "maxdelay" );
    my @aplot_dir    = ( "<", ">" );
    my @vth          = ( 2*$vdd/3, $vdd/3 );
    my %data;
    my $max_or_min;

    $max_or_min = $min_or_max == 1 ? 0 : 1;

    for my $i (0..$#slews) {
        my $file = "$file_base:$slews[$i]";
        $file .= ":$cap" if defined($cap) and $cap ne "0";
#        print "Error: measure_transition $slews[$i] $cap $file $node $dir\n";
        my $filecmd=aplot_file_cmd($file);
        if($file eq $filecmd){
          if (! -e "$file.trace") {
# BUG 6548: assumes it is ok to miss this file.
#           warn "Warning: No data for $file_base, slew rate $slews[$i].\n";
            next;
          }
        }else{
          $file=$filecmd;
        }
        print "File = $file\n" if ($DEBUG);
        write_aplot("file \"$file\"");
        my $hashref = $$ranges_ref[$i];
        for my $input (keys %$hashref) {
            print " Input $input\n" if $DEBUG;
            my ($minmax_delay, $minmax_slew);
            $minmax_delay = $minmax_slew = 1e6;
            if ($min_or_max != 0) {
                $minmax_delay = -$minmax_delay;
                $minmax_slew = -$minmax_slew;
            }
            #$NON_CONTIGUOUS += $#{$hashref->{$input}};
            for my $r (0..$#{$hashref->{$input}}) {
                my $range = "[" . $hashref->{$input}->[$r]->[0] . ":" .
                                  $hashref->{$input}->[$r]->[1] . "]";
                print " Input = $input, Range = $range\n" if ($DEBUG);
                write_aplot("range $range");
                my $aplot_cmd = "$aplot_cmd[$min_or_max] " .
                                "$input $aplot_dir[1-$dir] $vth[1-$dir] " .
                                "$node $aplot_dir[$dir] $vth[$dir]";
                print "$aplot_cmd\n" if ($DEBUG);
                write_aplot($aplot_cmd);
                my $out = read_aplot();
                print "$out\n" if $DEBUG;
                if ($out =~ /^WARNING/) {
                    flush_aplot();
                    $aplot_cmd = "$aplot_cmd[$max_or_min] " .
                            "$node $aplot_dir[$dir] $vth[$dir] " .
                            "$input $aplot_dir[1-$dir] $vth[1-$dir]";
                    print "RETRY $aplot_cmd\n" if $DEBUG;
                    write_aplot ($aplot_cmd);
                    $out = read_aplot();
                    print "$out\n" if $DEBUG;
                    if ($out =~ /^WARNING/i) {
                        $out = "0 0";
                    }
                    else {
                        my ($t, $d) = split /\s+/, $out;
                        $d = -$d;
                        $out = "$t $d";
                    }
                }
                flush_aplot();
                my ($time0, $delay) = split /\s+/, $out;
                if ($time0 eq "WARNING:") {
                    warn "Unexpected no trigger.\n";
                    next;
                }
                if (!is_numeric($delay)) {
                    warn "Encountered non-numeric $delay for node $node in " .
                         "$file, input $input.\nUsing delay of 0.";
                    warn "cmd = $aplot_cmd\n";
                    warn "response = $out\n";
                    $delay = 0;
                }

                # Measure slew rate
                $aplot_cmd = "$aplot_cmd[$min_or_max] " .
                             "$input $aplot_dir[1-$dir] $vth[1-$dir] " .
                             "$node $aplot_dir[$dir] $vth[1-$dir]";
                print "$aplot_cmd\n" if $DEBUG;
                write_aplot($aplot_cmd);
                $out = read_aplot();
                print "$out\n" if $DEBUG;
                if ($out =~ /^WARNING/) {
                    flush_aplot();
                    $aplot_cmd = "$aplot_cmd[$max_or_min] " .
                             "$node $aplot_dir[$dir] $vth[1-$dir] ".
                             "$input $aplot_dir[1-$dir] $vth[1-$dir]";
                    print "RETRY $aplot_cmd\n" if $DEBUG;
                    write_aplot ($aplot_cmd);
                    $out = read_aplot();
                    print "$out\n" if $DEBUG;
                    if ($out =~ /^WARNING/i) {
                        $out = "0 0";
                    }
                    else {
                        my ($t, $d) = split /\s+/, $out;
                        $d = -$d;
                        $out = "$t $d";
                    }
                }
                flush_aplot();
                my ($time1, $slew_delay) = split /\s+/, $out;
                if ($time1 eq "WARNING:") {
                    warn "Unexpected no trigger (slew).\n";
                    next;
                }
                if (!is_numeric($slew_delay)) {
                    warn "Encountered non-numeric $slew_delay for node $node in " .
                         "$file, input $input.\n" .
                         "** Using slew rate of 0 **";
                    warn "cmd = $aplot_cmd\n";
                    warn "response = $out\n";
                    $slew_delay = $delay;
                }
                $slew_delay -= $delay;
                $slew_delay *= 1e3;
                $delay      *= 1e3;
                if ($min_or_max == 0) {
                    $minmax_delay = $delay if ($delay < $minmax_delay);
                    $minmax_slew  = $slew_delay if ($slew_delay < $minmax_slew);
                }
                else {
                    $minmax_delay = $delay if ($delay > $minmax_delay);
                    $minmax_slew  = $slew_delay if ($slew_delay > $minmax_slew);
                }
            }
            print "MX $minmax_delay $minmax_slew\n" if $DEBUG;
            ${$data{"$input$LveAspice::DIR_STR[$dir-1]"}}{$slews[$i]*$slew_scaling} = 
                [ $minmax_delay, $minmax_slew ];
        }
    }
    #die "No data found for $file_base.\n" if (! %data);
    return %data;
}


# Returns test->aggressor->ranges map of delay scenarios, where test
# parameter is test:cc:tau (e.g. "slow_up:0:15"), aggressor is a node name,
# and the ranges value is a list of two-element array references 
# [ start, stop].  
#
# When optimize is set to 1, the ranges of continguous scenarios for the 
# same aggressor are combined.  When optimize==1, a warning is printed if 
# all scenarios for a particular aggressor are not contiguous.
sub read_delay_scenarios {
    my ($file,$optimize,$type,$timearray) = @_;
    my %scenarios;
    my $test = "";
    my $start = 0;
    my @aggressor = ();
    my $d2a_shape = "(not found)";
    if($file=~/\.gz$/){
      open (IN, "gzip -cd \'$file\'|") or die "Cannot read $file.\n";
    }else{
      open (IN, "<$file") or die "Cannot read $file.\n";
    }
    $type="" if(not defined $type);
    while (<IN>) {
        chomp;
        s/^\s+//;
        if (/^d2a_shape = (\S+)/) {
            $d2a_shape = $1;
        }
        elsif ($test eq "" and $type ne "thresh" and /^Alint (\S+) simulation \(CC=(\d) Tau=([\d\.e-]+) Cap=([\d\.e-]+)\):$/) {
            next if ($1 eq "bump_up" or $1 eq "bump_dn");
            $test = "$1:$2:" . int($3/1e-12 + 0.5);
            $test .= ":".sprintf("%g", $4*1e15) if $4 > 0;
            $scenarios{$test} = {};
            @aggressor = ();
        }
        elsif ($test eq "" and $type ne "thresh" and /^Alint (\S+) simulation \(CC=(\d) Tau=([\d\.e-]+)\):$/) {
            # needed for re-raw
            next if ($1 eq "bump_up" or $1 eq "bump_dn");
            $test = "$1:$2:" . int($3/1e-12 + 0.5);
            $scenarios{$test} = {};
            @aggressor = ();
        }
        elsif($test eq "" and $type eq "thresh" and /^Alint thresh_(\S+) simulation \(CC=(\d) Tau=([\d\.e-]+) Percent=([\d\.]+)\):$/) {
            $test="thresh_$1:$2:".int($3/1e-12 + 0.5).":$4";
            $scenarios{$test} = {};
            $$timearray{$test} = ();
            @aggressor = ();        
            next;
        }
        elsif($test eq "" and $type eq "thresh" and /^Alint thresh_(\S+) simulation \(Percent=([\d\.]+)\):$/) {
            #old thresh style cc=1, tau=8
            $test="thresh_$1:$2";
            $scenarios{$test} = {};
            $$timearray{$test} = ();
            @aggressor = ();        
            next;
        }
        elsif ($test ne "" and /^\w+\s+([\d\.]+)\s*/) {
            my $this = $1;
            my $nodes=$';
            if (scalar(@aggressor) > 0 ) {
                add_scenario($scenarios{$test},\@aggressor,$start,$this,$optimize); 
                @aggressor=();
                if($type eq "thresh"){
                  push @{$$timearray{$test}},[$start,$this];
                }

            }
            $start = $this;                       
            my @nodes= split(/\s+/,$nodes); 
            foreach my $item (@nodes){
                if($item =~ /([\w\[,\]\.\#\:]+):(\+|\-)/){
                    push @aggressor, $1;
                }
            }
        }
        elsif ($test ne "") {
            if (scalar(@aggressor) >0 ) {
                add_scenario($scenarios{$test},\@aggressor,$start,$start+10,$optimize);
                foreach my $aggressor (@aggressor){
                    if (@{$scenarios{$test}{$aggressor}} > 1 and $optimize and $DEBUG) {
                        warn "Non-contiguous scenarios for test $test, " .
                             "aggressor $aggressor.\n"
                    }
                }
                if($type eq "thresh"){
                  push @{$$timearray{$test}},[$start,$start+10];
                }
            }
            else {
                delete $scenarios{$test} unless keys %{$scenarios{$test}};
                warn "No scenarios for test $test in $file.\n" 
            }
            @aggressor=();
            $test = "";
        }
    }
    close IN;

    return %scenarios;
}

sub read_bump_scenarios {
    my ($file) = @_;
    my $scenarios={};
    my $test = "";
    my $bump = "";
    if($file=~/\.gz$/){
      open (IN, "gzip -cd \'$file\'|") or die "Cannot read $file.\n";
    }else{
      open (IN, "<$file") or die "Cannot read $file.\n";
    }
    print "filename: $file\n" if $DEBUG;
    while (<IN>) {
        s/^\s+//;
        if ($test eq "" and /^Alint (\S+) simulation \(CC=(\d) Tau=([\d\.e-]+) Cap=([\d\.e-]+)\):$/) {            
            if ($1 eq "bump_up" or $1 eq "bump_dn"){
                $bump=$1;
                $test = "$2:" . int($3/1e-12 + 0.5);
                $test .= ":".sprintf("%g", $4*1e15) if $4 > 0;
                @{$scenarios->{$test}->{$1}} = ();
            }
        }elsif ($test eq "" and /^Alint (\S+) simulation \(CC=(\d) Tau=([\d\.e-]+)\):$/) {
            # needed for re-raw
            if ($1 eq "bump_up" or $1 eq "bump_dn"){
                $bump=$1;
                $test = "$2:" . int($3/1e-12 + 0.5);
                @{$scenarios->{$test}->{$1}} = ();
            }
        }elsif ($test ne "" and /^\w+\s+([\d\.]+)/) {
            push @{$scenarios->{$test}->{$bump}}, $1;
        }elsif ($test ne "") {
          $test="";
        }
    }
    close(IN);
    return $scenarios;
}

sub add_scenario {
    my ($scenarios,$aggressorref,$start,$stop,$optimize) = @_;
    foreach my $aggressor (@$aggressorref){
        if (defined @{$$scenarios{$aggressor}} && $optimize &&
            ${${$$scenarios{$aggressor}}[0]}[1] == $start) {
            ${${$$scenarios{$aggressor}}[0]}[1] = $stop;
            my $x = ${${$$scenarios{$aggressor}}[0]}[0];
            print "ADD1 $aggressor $x $stop\n" if $DEBUG;
        }
        else {
            push @{$$scenarios{$aggressor}}, [ $start, $stop ];
            print "ADD2 $aggressor $start $stop\n" if $DEBUG;
        }
    }
}

sub generate_pwl {
    my ($bindir, $aplot, $type)=@_;
    my $pwd=`pwd`;
    chomp $pwd;


    my $fd;
    opendir ($fd, $bindir);
    my @nodes=readdir($fd);
    closedir ($fd);
    my $workdir  = mktemp_workdir("pwl");    
    foreach my $node (@nodes) {
        next if ( ! -d "$bindir/$node" or ($node =~ /^\./));
        my ($is_archive,@files)=is_node_archive("$bindir/$node",$trace_archive);  
        if(! $is_archive){
          opendir ($fd, "$bindir/$node");
          @files=grep(/${type}_.*\.trace/, readdir($fd));
          closedir ($fd);
        }else{
          archive_extrace_files("$bindir/$node/$trace_archive",$workdir,@files);
          @files=map {"$workdir/$_"} grep(/${type}_.*\.trace/, @files);
        }
        chdir("$bindir/$node");
        open_aplot($aplot);
        my @pwl;
        foreach my $file (@files) {
            $file =~ s/\.trace//;
            write_aplot("file \"$file\"\n");
            my $pwl="$file:IGND.pwl";
            (my $vol, my $dir_t, $pwl) = File::Spec->splitpath($pwl);
            push @pwl, $pwl;
            unlink $pwl;
            write_aplot("write IGND|\n");
            $pwl="$file:IVdd.pwl";
            ($vol, $dir_t, $pwl) = File::Spec->splitpath($pwl);
            push @pwl, $pwl;
            unlink $pwl;
            write_aplot("write IVdd|\n");
            flush_aplot();
        }
        close_aplot();
        if (@pwl) {
            my $fz;
            unlink "$type.pwl.gz";
            open ($fz, "| gzip > $type.pwl.gz");
            foreach my $pwl (sort @pwl) {
                my $fp;
                if (open ($fp, "<$pwl")) {
                    print $fz "$pwl\n";
                    while (<$fp>) {
                        print $fz $_;
                    }
                    print $fz "";
                    close $fp;
                    unlink $pwl;
                }
                else {
                    print STDERR "Cannot open $pwl $!\n";
                }
            }
            close $fz;
        }
    }
    chdir ($pwd);
    cleanUp_workdir($workdir);

}

sub aplot_file_cmd{
    my ($filebase,$postfix)=@_;
    $postfix="trace" if(not defined $postfix);
    my ($vol, $dir, $file_name_t) = File::Spec->splitpath($filebase);
    my $archive_file=catfile($dir,$trace_archive); 
    if (-e $archive_file){
        my $ismember=archive_memberNamed($archive_file,"$file_name_t.$postfix");
        return $filebase if(not defined $ismember);
        return "$archive_file/$file_name_t";
    }
    return $filebase;
}

1;


