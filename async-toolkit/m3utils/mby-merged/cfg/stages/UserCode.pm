package UserCode;
use strict;

sub UserCode::ndocs() {
   my $status = 0;
   my $self = (caller(0))[3] . "()";

   print "-I- $self: Running the Natural Docs doc_me PRE_FLOW...\n";

   my $modelRoot = $ENV{'MODEL_ROOT'};
   defined($modelRoot) or chomp($modelRoot = `/usr/intel/bin/git rev-parse --show-toplevel`);
   my $cmd = "csh -f $modelRoot/cfg/bin/doc_me";

   print "-I- $self: Executing: $cmd\n";
   system("$cmd");
   $status = $?;

   if($status != 0) {
      print "-E- $self: Execution of doc_me returned with status=$status\n";
   } else {
      print "-I- $self: Execution of doc_me returned with status=$status\n";
   }

   return $status;
}

1;
