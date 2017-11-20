#---*-perl-*--------------------------------------------------------------------
package tlm::RunModes;



use Ace::TestRunModes;

sub init_library {
	register_library "tlm::RunModes";
	if ($main::ACE_RUN_MODE =~ /^seed_/) { 
	    $SEED = $main::ACE_RUN_MODE;
	    $SEED =~ s/seed_//;
	}
	%RUNMODES = (
	    
	    "seed_$SEED" => {
		-seed => "$SEED",
	    },
	    
	    );
	%RUNMODE_GROUPS = (
	    all => [ keys %RUNMODES ],
	    );
}
1;
