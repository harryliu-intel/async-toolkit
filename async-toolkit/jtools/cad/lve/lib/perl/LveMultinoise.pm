
package LveMultinoise;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        &multi_noise_analysis &save_bump_resp &save_fanout_info
    );
}

use strict;
use FileHandle;
use IPC::Open2;
use File::Spec;
use File::Spec::Functions;
use LveUtil;

#store bump response result for multiple noise analysis
our $isMultiNoise=1;
our $bump_resp_result={};
our $fanout_info={};
our $raw_fh;
our $raw_prefix;
our @resp_bound=(10,30); #cc=0, cc=1

#save bump response result into a hash table
sub save_bump_resp{
  my ($fanout_cell, $in, $out, $bumpinfo, $resp_ref, 
      $bump_scen,$thresh_scen,$power_sag,$source_pvt,$fanout_type)=@_;
  return if(!$isMultiNoise);
  my $mis = "$power_sag$source_pvt$fanout_type";
  $bump_resp_result->{$bump_scen}->{$thresh_scen}->{$mis}->{"$fanout_cell/$out"}->{$in}->{bump}
   =$bumpinfo;
  @{$bump_resp_result->{$bump_scen}->{$thresh_scen}->{$mis}->{"$fanout_cell/$out"}->{$in}->{resp}}
   =@$resp_ref;
}

#save fanout pair information into a hash table and get parallel switching inputs
sub save_fanout_info{
  my ($fanout_cell, $out, $path)=@_;
  return if(!$isMultiNoise);
  $fanout_info->{$fanout_cell}->{path}=$path;
  return if (defined  $fanout_info->{$fanout_cell}->{out}->{$out}); 
  get_parallel_inputs($path,$fanout_cell, $out);
}

#main function of multiple noise analysis
sub multi_noise_analysis{
    ($raw_fh, $raw_prefix)=@_;
    return if(!$isMultiNoise);
    foreach my $fanout_cell (keys %$fanout_info){
       foreach my $out (keys %{$fanout_info->{$fanout_cell}->{out}}){
           additive_resp($fanout_cell,$out);
       }
    }    
}



##############################
# Package internal functions
##############################
#get parallel switching inputs
sub get_parallel_inputs{
  my ($path,$fanout_cell,$out)=@_;
  my $node_path=get_node_path($out, $path);
  my $out_file="";
  if(-e "$node_path/out"){
    $out_file="$node_path/out";
  }elsif(-e "$node_path/out.gz"){
    $out_file="$node_path/out.gz";
  }else{
    print STDERR "ERROR: cannot find $node_path/out file";
  }
  my($up_scen,$dn_scen)=read_thresh_scenarios($out_file);
  my %merge;
  @{$merge{up}}=find_merged_scenarios(@$up_scen);
  @{$merge{dn}}=find_merged_scenarios(@$dn_scen);
  foreach my $dir ("up","dn"){
    foreach my $scen (@{$merge{$dir}}){
      my @in=split(/\s+/,$scen);    
      my @pairs=();
      foreach my $in (@in){
        my ($n,$d) = split(":",$in);
        if($d eq "+" or $d eq "-"){
          push @pairs, $n;    
        }
      }
      push @{$fanout_info->{$fanout_cell}->{out}->{$out}->{$dir}}, [@pairs];
    }
  }
}

#get node bin path
sub get_node_path {
  my ($node, $alint_path)=@_;
  my $node_fix = $node;
  my $isPO=0;
  $node_fix =~ s/\[/\\\[/g;
  $node_fix =~ s/\]/\\\]/g;
  $node_fix =~ s/\./\\\./g;
  my @loc=`grep '^alint ${node_fix}\\s*\$' \"$alint_path/alint_parallel/alint.in.\"*`;
  if(scalar(@loc)==0){
      if(-e "$alint_path/alint_PO_parallel"){
        @loc=`grep '^alint ${node_fix}\\s*\$' \"$alint_path/alint_PO_parallel/alint_PO.in.\"*`;
        $isPO=1;
      }
      return "" if (scalar(@loc)==0);
  }
  my ($bin_path,$rest)=split(":",$loc[0]);
  if($bin_path =~ /\.(\d+)$/){
    my $node_path = "$alint_path/alint.bin.$1/$node";
    $node_path = "$alint_path/alint_PO.bin.$1/$node" if ($isPO);
    return $node_path; 
  }
  return "";
}

#Based on parallel switching inputs, add up response results.
sub additive_resp{
    my ($fanout_cell, $out)=@_;
    foreach my $bump_scen (keys %{$bump_resp_result}){
        foreach my $thresh_scen (keys %{$bump_resp_result->{$bump_scen}}){
            my ($threshCC, $threshTau)=split(":",$thresh_scen);
            foreach my $mis (keys %{$bump_resp_result->{$bump_scen}->{$thresh_scen}}){
                if (not defined $bump_resp_result->{$bump_scen}->{$thresh_scen}->{$mis}->{"$fanout_cell/$out"}){
                    next;
                }
                my @multi_noise_resp=();
                #for each parallel pair, generate the multiple noise response
                my $dir="up";
                $dir="dn" if($bump_scen=~/bump_up/);
                my ($worst_v, $worst_t)=(0,0);
                my @worst_pair=();
                my @worst_bump=();
                my $status="PASS";
                foreach my $ref (@{$fanout_info->{$fanout_cell}->{out}->{$out}->{$dir}}){
                    #filter out those input nodes which do not have response function. Such as PI.
                    my @in= map {(defined $bump_resp_result->{$bump_scen}->{$thresh_scen}->{$mis}->{"$fanout_cell/$out"}->{$_})? $_:() } @$ref;
                    next if(scalar(@in)<=1);
                    my @bump_info=();
                    foreach my $in (@in){
                      @multi_noise_resp = merge_resp(\@multi_noise_resp,
                                          $bump_resp_result->{$bump_scen}->{$thresh_scen}->{$mis}->{"$fanout_cell/$out"}->{$in}->{resp});
                      push @bump_info, $bump_resp_result->{$bump_scen}->{$thresh_scen}->{$mis}->{"$fanout_cell/$out"}->{$in}->{bump};
                    }
                    # multi-noise response result
                    my ($peak_t, $peak_v)=get_resp_peak(@multi_noise_resp);
                    if ($peak_v >= $worst_v){
                      #save current worst result
                      @worst_pair = @in;
                      $worst_v = $peak_v;
                      $worst_t = $peak_t;
                      @worst_bump = @bump_info;
                    }   
                    #report the worst result
                    if($peak_v > $resp_bound[$threshCC]){
                      $status="FAIL";
                      report_formatter($status, $peak_v, $peak_t, $bump_scen, $thresh_scen, $resp_bound[$threshCC], 
                                       $mis, $fanout_cell, $out, \@bump_info, \@in);
                    }
                }
                #report the worst result
                #only reports when the fanout node has parallel inputs. 
                if ($status eq "PASS" and scalar(@worst_pair)>1){
                  report_formatter($status, $worst_v, $worst_t, $bump_scen, $thresh_scen, $resp_bound[$threshCC],
                                   $mis, $fanout_cell, $out, \@worst_bump, \@worst_pair);
                }
            }
        }
    }
}

sub merge_resp{
    my ($resp1,$resp2)=@_;
    my $r1_index=0;
    my @result=();
    my ($v2, $t2, $t2_p, $v2_p)=(0,0,0,0);
    my ($t1, $v1, $t1_p, $v1_p)=(0,0,0,0);
    foreach my $tv_ref (@$resp2){
      ($t2,$v2) = @$tv_ref;
      for (; $r1_index<scalar(@$resp1);$r1_index++){
        ($t1, $v1) = @{$resp1->[$r1_index]};
        last if($t1>=$t2);
        my $v_ = $v1 + $v2_p + ($v2 - $v2_p)*(($t1-$t2_p)/($t2-$t2_p));
        push @result, [$t1, $v_]; 
        $t1_p = $t1;
        $v1_p = $v1;
      }
      
      my $v_;
      if($t1>=$t2){
         $v_ = $v2 + $v1_p + ($v1 - $v1_p)*(($t2-$t1_p)/($t1-$t1_p));
      } else {
         $v_ = $v2;
      }
      push @result, [$t2, $v_]; 
      if($t1==$t2){
        $t1_p = $t1;
        $v1_p = $v1;
        $r1_index++;
      }
      $t2_p = $t2;
      $v2_p = $v2;
    }
    for (; $r1_index<scalar(@$resp1);$r1_index++){
        my ($t1, $v1) = @{$resp1->[$r1_index]};
        push @result, [$t1, $v1];
    }      
  
    return @result;
}

sub report_formatter{
    my ($status, $resp_v, $resp_t, $bump_scen, $thresh_scen, $resp_bound, $mis, $fanout_cell, $out, $bump_ref, $in_ref)=@_;
    my ($bump_dir,$bump_cc,$bump_tau)=split(":",$bump_scen);
    my ($thresh_cc,$thresh_tau)=split(":",$thresh_scen);
    my $resp_str = "multi_noise_resp_up";
    $resp_str = "multi_noise_resp_dn" if ($bump_dir =~ /up/);
    my $fanin = join("/", @$in_ref);
    my $bump = join("/", @$bump_ref);
    my ($key, $cell, $path)=split(/\s+/,$raw_prefix);
    my $fanout = $out;
    $fanout = "$fanout_cell/$fanout" if ($cell ne $fanout_cell);
    my $resp_v_int = int($resp_v+0.5);
    print $raw_fh "$status $raw_prefix".
                  " fanout=$fanout".
                  " fanin=$fanin".
                  " cc=$bump_cc".
                  " tau=$bump_tau".
                  " threshCC=$thresh_cc".
                  " threshTau=$thresh_tau".
                  "$mis".
                  " $bump_dir=$bump".
                  " $resp_str=$resp_v_int".
                  " resp_bound=$resp_bound\n";

}

sub get_resp_peak{
  my ($max_t,$max_v)=(0,0);
  foreach my $ref (@_){
      my ($t,$v) = @$ref;
      if ($v>=$max_v){
          $max_v = $v;
          $max_t = $t;
      }
  }
  return ($max_t, $max_v);
}

sub read_thresh_scenarios{
  my($file)=@_;
  my @up_scen=();
  my @dn_scen=();
  if($file=~/\.gz$/){
    open (IN, "gzip -cd \'$file\'|") or die "Cannot read $file.\n";
  }else{
    open (IN, "<$file") or die "Cannot read $file.\n";
  }
  my $gotUp=0; my $gotDn=0;
  while (<IN>) {
      chomp;
      last if ($gotUp and $gotDn);
      if (/^Alint (thresh_up|thresh_dn) simulation/){
         my $dir=$1;
         while(<IN>){
            last if (/^Total/);
            push @up_scen, $1 if(/^\s+thresh_up\s+\S+ (.*)$/ and !$gotUp);
            push @dn_scen, $1 if(/^\s+thresh_dn\s+\S+ (.*)$/ and !$gotDn);
         }
         $gotUp=1 if ($dir=~/_up/);
         $gotDn=1 if ($dir=~/_dn/);
      }
  }
  return(\@up_scen, \@dn_scen);
}
###################################
# evaluate thresh pairs from Andrew
###################################
sub find_merged_scenarios {
    my @scenarios = @_;
    my %found;
    foreach my $s1 (@scenarios) {
        my $s = $s1;
        my $do = 0;
        foreach my $s2 (@scenarios) {
            my $t = merge_scenarios($s,$s2);
            unless ($t eq "") {
                $do=1;
                $s=$t;
            }
        }
        if ($do) { $found{$s} = 1; }
    }
    return sort keys %found;
}

sub merge_scenarios {
    my ($s1, $s2) = @_;
    my @f1=split(" ",$s1);
    my @f2=split(" ",$s2);
    my $s = "";
    my $d;
    my $n=0;
    for (my $i=0; $i<@f1; $i++) {
        my ($n1,$d1) = split(":",$f1[$i]);
        my ($n2,$d2) = split(":",$f2[$i]);
        if (($d1 eq "0") && ($d2 eq "1") ||
            ($d1 eq "1") && ($d2 eq "0") ||
            ($d1 eq "+") && ($d2 eq "1") ||
            ($d1 eq "-") && ($d2 eq "0") ||
            ($d1 eq "1") && ($d2 eq "+") ||
            ($d1 eq "0") && ($d2 eq "-")) { return ""; }
        $d=$d1;
        if    ($d1 eq "+" || $d2 eq "+") { $d = "+"; $n++; }
        elsif ($d1 eq "-" || $d2 eq "-") { $d = "-"; $n++; }
        $s .= "$n1:$d ";
    }
    if ($n<2) { return ""; }
    return $s;
}

1;


