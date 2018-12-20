#!/usr/bin/env perl 
open(SF, "$ENV{'MODEL_ROOT'}/target/mby/mgm_run/gpm/src/gpm_shells_wrapper.v") || die "can't open input file
";
open(TF, ">./gpm_shells_wrapper_inc.v") || die "can't open output file
";

print TF "`include        \"gpm_mem.def\"
";
while (<SF>) {
   printf TF ("%s",$_);
}
close (TF);
close (SF);
