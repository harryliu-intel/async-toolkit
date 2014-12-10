#!/usr/intel/bin/perl

# Gets statistics out of TestChannel's output

while (<>) {
    if (/discrepancy/) {
        if (/No time discrepancy/) {
            $num = 0;
        }
        else {
            ($temp = $_) =~ s/.*discrepancy//;
            $temp =~ s/vs.//;
            @nums = split(' ',$temp);
            $num = $nums[0]-$nums[1]; # Tsim - dsim
        }
        if (/Split/) {
            $splitDevice{(int($num/100))} ++;
        }
        if (/Merge/) {
            $mergeDevice{(int($num/100))} ++;
        }
    }
}

open SPLIT, "| sort > split" or die "can't open split file";
foreach $key (keys %splitDevice) {
    print SPLIT "$key $splitDevice{$key}\n";
}

open MERGE, "| sort > merge" or die "can't open merge file";
foreach $key (keys %mergeDevice) {
    print MERGE "$key $mergeDevice{$key}\n";
}

    
        
