#
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id: CastLib.pm,v 1.07 2002/01/24 12:00:00 auto-migrate Exp $
#

package		CastParserLib;
require		Exporter;

use CastLib;
use Parse::RecDescent;
use strict;

sub format_arraydecl
{
	my $arraydecl = shift;

	return if( not defined $arraydecl );

	if( @{$arraydecl} == 1 )
	{
		return format_decl(@{$arraydecl}[0]);
	}
	else
	{
		my $str = "{";
		my $first = 1;
		foreach my $decl(@{$arraydecl})
		{
			$str .= "," if( not $first );
			$first = 0;
			$str .= format_decl($decl);
		}
		$str .= "}";
		return $str;
	}
}

sub format_metaparams
{
	my $metaparams = shift;

	return if( not defined $metaparams );

	my $str;

	my $first = 1;
	$str .= "(";
	foreach my $param (@{$metaparams})
	{
		$str .= "," if( not $first );
		$first = 0;
		$str .= $param;
	}
	$str .= ")";

	return $str;
}

sub format_decl
{
	my $decl = shift;
	return if( not defined $decl );
	
	my $str = "";
	$str .= $decl->{name};
	$str .= format_declarrayparam($decl->{arrayparam});
	return $str;
}

sub format_whole_decl
{
	my $decl = shift;
	return if( not defined $decl );

	my $str = "";

	$str .= $decl->{type};
	$str .= format_metaparams($decl->{metaparams});
	$str .= format_declarrayparam($decl->{arrayparam});
	$str .= " ";
	$str .= format_decl($decl->{name});

	return $str;
}

sub format_instantiation
{
	my $inst = shift;

	return if( not defined $inst );

	my $str = "";

	$str .= $inst->{type};
	$str .= format_metaparams($inst->{metaparams});
	$str .= " ";
	$str .= format_decl($inst->{instance});
	$str .= format_paramlist($inst->{params});

	return $str;
}

sub format_paramlist
{
	my $params = shift;

	return if( not defined $params );
	my $str = "";

	$str .= "(";
	my $first = 1;
	foreach my $param(@{$params})
	{
		$str .= "," if( not $first );
		$first = 0;
		$str .= format_decl($param);
	}
	$str .= ")";

	return $str;
}

sub _unroll_inst_array
{
	my $index = shift;
	my $range = shift;
	my $inst = shift;

#	print STDERR "UNROLL inst: " . format_instantiation($inst) . "\n";
#	print STDERR "index: $index   range: $range->{low}..$range->{high}\n";

	my @instance_list;					# List of instance data structures

	# Extract instance name
	my $instance = $inst->{instance};					# Type decl 
	my $inst_name = $instance->{name};					# Type alphanumeric
	my $expr = @{$instance->{arrayparam}}[0]
		if( defined $instance->{arrayparam} );			# Type declrange

	for( my $i = $range->{low}; $i <= $range->{high}; $i++ )
	{
		my %newinst = ();
		# celltype,keywords will not have "index" in it, so just copy
		$newinst{type} = $inst->{type};
		$newinst{keywords} = $inst->{keywords};

		# Process any occurance of "index" in instance name
		my @processed_expr = ();
		$newinst{instance}{name} = $inst_name;
		if( defined $expr )
		{
			my %range;
	
			$range{low} = 
				CastParser::_eval_expr($expr->{low},$index,$i);
			$range{high} = 
				CastParser::_eval_expr($expr->{high},$index,$i);
			push @{$newinst{instance}{arrayparam}}, \%range;
		}

		# Process metaparameters
		if( defined $inst->{metaparams} )
		{
			foreach my $param (@{$inst->{metaparams}})	# Type expression 
			{
				push @{$newinst{metaparams}}, CastParser::_eval_expr($param,$index,$i);
			}
		}

		# Process parameters
		if( defined $inst->{params} )
		{
			foreach my $param (@{$inst->{params}})	# Type decl
			{
				my $param_name = $param->{name};
				my $param_list = $param->{arrayparam};
				my @processed_params = ();
				foreach my $foo (@{$param_list})	# Type declrange
				{
					my %declrange;
					$declrange{low} = 
						CastParser::_eval_expr($foo->{low},$index,$i);
					$declrange{high} = 
						CastParser::_eval_expr($foo->{high},$index,$i);
					push @processed_params, \%declrange;
				}
				my %decl;
				$decl{name} = $param_name;
				$decl{arrayparam} = \@processed_params;
				push @{$newinst{params}}, \%decl;
			}
		}
#		print STDERR  "  " . format_instantiation(\%newinst) . "\n";
		push @instance_list, \%newinst;
	}
	return @instance_list;
}

sub _unroll_array
{
	my $index = shift;
	my $range = shift;
	my $contents = shift;

	my %newcontents;

	foreach my $inst (@{$contents->{instances}})
	{
		push @{$newcontents{instances}}, ::unroll_inst_array($index,$range,$inst);
	}
	foreach my $decl (keys %{$contents->{declarations}})
	{
		push @{$newcontents{declarations}}, ::unroll_decl_array($index,$range,$decl);
	}
	return \%newcontents;
}

sub _subcell_merge_contents
{
	my @contents = @_;

	my %merge = ();

	my @types = ("e1of[0-9]+","node","e1of","_1of[0-9]+","1of[0-9]+","shs");

	foreach my $cur_contents (@contents)
	{
		if( defined $cur_contents->{instances} )
		{
			foreach my $inst (@{$cur_contents->{instances}})
			{
				my $type = $inst->{type};
				my $match = 0;
				foreach my $typecheck (@types)
				{
					$match = 1 if( $type =~ /^$typecheck$/ );
				}
				if( $match )
				{
					# Need to cram this into a whole_decl data structure
					my %decl_info;
					$decl_info{name} = $inst->{instance};
					$decl_info{type} = $inst->{type};
					$decl_info{metaparams} = $inst->{metaparams};
					$decl_info{arrayparam} = $inst->{params};
					push @{$merge{declarations}}, \%decl_info;
				}
				else
				{
					push @{$merge{instances}}, $inst;
				}
			}
		}
		if( defined $cur_contents->{declarations} )
		{
			push @{$merge{declarations}}, @{$cur_contents->{declarations}};
		}
	}
	return \%merge;
}

sub _is_subcell_flattened
{
	my $inst = shift;

	return 0 if( not defined $inst->{keywords} );
	foreach my $keyword (@{$inst->{keywords}})
	{
		return 1 if( $keyword eq "flatten" );
	}
	return 0;
}

sub format_declrange
{
	my $range = shift;

	return if( not defined $range );
	my $low = $range->{low};
	my $high = $range->{high};
	my $str = "$low";
	$str .= "..$high" if( $low ne $high );
	return $str;
}

sub format_declarrayparam
{
	my $arrayparam = shift;

	return if( not defined $arrayparam );

	my $str = "";

	$str .= "[";
	my $first = 1;

	foreach my $range (@{$arrayparam})
	{
		$str .= "," if( not $first );
		$first = 0;
		$str .= format_declrange($range);
	}
	$str .= "]";

	return $str;
}

sub format_chanrange
{
	my $range = shift;

	return if( not defined $range );
	my $low = $range->{low};
	my $high = $range->{high};
#	return "$range->{low}..$range->{high}";
	return "$low..$high";
}

sub format_chanarrayparam
{
	my $arrayparam = shift;

	return if( not defined $arrayparam );

	my $str = "";

	$str .= "[";
	my $first = 1;

	foreach my $range (@{$arrayparam})
	{
		$str .= "," if( not $first );
		$first = 0;
		$str .= format_chanrange($range);
	}
	$str .= "]";

	return $str;
}

sub format_portname
{
	my $name = shift;
	my $nodir = shift;

	return if( not defined $name or not defined $nodir );

	my $str = "";

	$str .= $name->{direction} if( not $nodir );
	$str .= $name->{name};
	$str .= format_declarrayparam($name->{arrayparam});

	return $str;
}

sub format_port
{
	my $port = shift;
	my $nodir = shift;

	return if( not defined $port );

	$nodir = 1 if( not defined $nodir );

	my $str = "";

	$str .= $port->{type};
	if( defined $port->{metaparams} )
	{
		$str .= "(";
		my $first = 1;
		foreach my $metaparam (@{$port->{metaparams}})
		{
			$str .= "," if( not $first );
			$first = 0;
			$str .= $metaparam;
		}		
		$str .= ")";
	}

	$str .= format_chanarrayparam($port->{arrayparam});

	$str .= " ";
	$str .= format_portname($port->{name},$nodir);

	return $str;
}

1;
