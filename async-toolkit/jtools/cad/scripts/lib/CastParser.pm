#
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id: CastLib.pm,v 1.07 2002/01/24 12:00:00 auto-migrate Exp $
#

package		CastParser;
require		Exporter;

use CastLib;
use CastParserLib;
use Parse::RecDescent;
use strict;

# $::RD_TRACE = 1;
# $::RD_ERRORS = 1; # Make sure the parser dies when it encounters an error
# $::RD_WARN   = 1; # Enable warnings. This will warn on unused rules &c.
# $::RD_HINT   = 1; # Give out hints to help fix problems.
$::RD_AUTOSTUB = 1;

my %block_parsers;

my $common_grammar = <<'END_GRAMMAR';
	whole_decl		:	alphanumeric LPAREN params RPAREN LBRACKET chanarrayparam RBRACKET decls SEMI
						{ # print STDERR  "FOUND WHOLE PORT 1: \"@item\"\n";
							my @a;
							foreach my $decl (@{$item[8]})
							{
								my %decl_info;
								$decl_info{name} = $decl;
								$decl_info{type} = $item[1];
								$decl_info{metaparams} = $item[3];
								$decl_info{arrayparam} = $item[6];
								push @a,\%decl_info;
							}
							$return = \@a;
						}
					|	alphanumeric LPAREN params RPAREN decls SEMI
						{ # print STDERR  "FOUND WHOLE PORT 2: \"@item\"\n";
							my @a;
							foreach my $decl (@{$item[5]})
							{
								my %decl_info;
								$decl_info{name} = $decl;
								$decl_info{type} = $item[1];
								$decl_info{metaparams} = $item[3];
								push @a,\%decl_info;
							}
							$return = \@a;
						}
					|	alphanumeric LBRACKET chanarrayparam RBRACKET decls SEMI
						{ # print STDERR  "FOUND WHOLE PORT 3: \"$item[1]\[$item[3]\] @{$item[5]}\"\n";
							my @a;
							foreach my $decl (@{$item[5]})
							{
								my %decl_info;
								$decl_info{name} = $decl;
								$decl_info{type} = $item[1];
								$decl_info{arrayparam} = $item[3];
								push @a,\%decl_info;
							}
							$return = \@a;
						}
					|	alphanumeric decls SEMI
						{ # print STDERR "FOUND WHOLE PORT 4: \"$item[1] $item[2]\"\n";
							my @a;
							foreach my $decl (@{$item[2]})
							{
								my %decl_info;
								$decl_info{name} = $decl;
								$decl_info{type} = $item[1];
								push @a,\%decl_info;
							}
							$return = \@a;
						}

	decls			:	arraydecl(s /,/)
						{ my @decls;
							foreach my $decl (@{$item[1]})
							{
								push @decls, @{$decl};
							}
							$return = \@decls;
						}

	arraydecl		:	'{' decl(s /,/) '}'
						{ # print STDERR "FOUND ARRAYDECL: @{$item[2]}\n";
							$return = $item[2]
						}
					|	decl
						{ $return = [$item[1]] }

	decl			:	alphanumeric LBRACKET declarrayparam RBRACKET DOT decl 
						{ # print STDERR  "FOUND DECL.DOT: \"$item[1]\[$item[3]\].$item[6]\"\n";
							my %decl_name;
							$decl_name{name} = $item[1];
							@{$decl_name{arrayparam}} = @{$item[3]};
							$decl_name{subdecl} = $item[6];
							$return = \%decl_name;
						}
					|	alphanumeric DOT decl
						{ # print STDERR  "FOUND DECL.DOT: \"$item[1].$item[3]\"\n";
							my %decl_name;
							$decl_name{name} = $item[1];
							$decl_name{subdecl} = $item[3];
							$return = \%decl_name;
						}
					|	alphanumeric LBRACKET declarrayparam RBRACKET
						{ # print STDERR  "FOUND DECL: \"$item[1] $item[3]\"\n";
							my %decl_name;
							$decl_name{name} = $item[1];
							@{$decl_name{arrayparam}} = @{$item[3]};
							$return = \%decl_name;
						}
					|	alphanumeric
						{ # print STDERR  "FOUND DECL: \"$item[1]\"\n";
							my %decl_name;
							$decl_name{name} = $item[1];
							$return = \%decl_name;
						}

	params			:	<leftop: expression ',' expression>
					|	''
						{ # print STDERR "FOUND EMPTY PARAMS\n";
							$return = [];
						}

	declarrayparam	:	declrange(s /,/)

	chanarrayparam	:	chanrange(s /,/)

	declrange	:	expression DOT DOT expression
						{ # print STDERR "FOUND DECLRANGE: $item[1]..$item[4]\n";
							my %range;
							$range{low} = CastParser::_eval_expr($item[1]);
							$range{high} = CastParser::_eval_expr($item[4]);
							$return = \%range;
						}
					|	expression
						{ # print STDERR "FOUND DECLRANGE: $item[1]\n";
							my %range;
							$range{low} = CastParser::_eval_expr($item[1]);
							$range{high} = $range{low};
							$return = \%range;
						}

	chanrange	:	expression DOT DOT expression
						{ # print STDERR "FOUND CHANRANGE: $item[1]..$item[4]\n";
							my %range;
							$range{low} = CastParser::_eval_expr($item[1]);
							$range{high} = CastParser::_eval_expr($item[4]);
							$return = \%range;
						}
					|	expression
						{ # print STDERR "FOUND CHANRANGE: 0..$item[1]-1\n";
							my %range;
							$range{low} = "0";
							$range{high} = CastParser::_eval_expr("$item[1]-1");
							$return = \%range;
						}

	expression		:	<leftop: term expr_op expression>
						{ $return = "@{$item[1]}"
						}

	expr_op			:	'+'
					|	'-'

	term			:	<leftop: factor term_op term> 
						{ $return = "@{$item[1]}"
						}

	term_op			:	'*'
					|	'/'
					|	'%'

	factor			:	'(' expression ')'
						{ $return = "($item[2])" }
					|	INTEGER
					|	arraydecl
						{ # print STDERR "Found ARRAYDECL as FACTOR: " . 
								CastParserLib::format_arraydecl($item[1]) . "\n";
							$return = CastParserLib::format_arraydecl($item[1]) }

	conditional		:	LBRACKET cond_expr cond_sep contents RBRACKET
						{ # print STDERR "FOUND CONDITION: \[$item[2] -> $item[4]\]\n";
							$return = $item[4];
						}

	cond_expr		:	<leftop: cond_factor cond_expr_op cond_expr>
						{ # print STDERR "FOUND CONDITION: @{$item[1]}\n";
							$return = "@{$item[1]}";
						}

	cond_expr_op	:	'=' '='
						{ $return = "==" }
					|	'<' '='
						{ $return = "<=" }
					|	'>' '='
						{ $return = ">=" }
					|	'<'
						{ $return = "<" }
					|	'>'
						{ $return = ">" }
					|	'!' '='
						{ $return = "!=" }
					|	'|'
						{ $return = "|" }
					|	'&'
						{ $return = "&" }
					|	'+'
						{ $return = "+" }
					|	'-'
						{ $return = "-" }
					|	'*'
						{ $return = "*" }
					|	'/'
						{ $return = "/" }
					|	'%'
						{ $return = "%" }

	cond_factor		:	'(' cond_expr ')'
						{ # print STDERR "Found (COND_EXPR) as COND_FACTOR: $item[2]\n"; 
							$return = $item[2] 
						}
					|	INTEGER
					|	arraydecl
						{ # print STDERR "Found ARRAYDECL as COND_FACTOR: " . 
								CastParserLib::format_arraydecl($item[1]) . "\n";
							$return = CastParserLib::format_arraydecl($item[1]) }

	cond_sep		:	'-' '>'
						{ # print STDERR "FOUND COND_SEP: ->\n";
							1;
						}

	var				:	('int' | 'bool') alias
						{ # print STDERR "FOUND VAR ASSIGNMENT: $item[1] $item[2]\n";
							1;
						}
					|	('int' | 'bool') decls SEMI
						{ # print STDERR "FOUND VAR: $item[1] $item[2]\n";
							1;
						}
					|	alphanumeric '=' expression SEMI
						{ 1; }

	alias			:	decl ('=' expression)(s) SEMI
						{ # print STDERR "FOUND ALIAS: $item[1] = @{$item[2]}\n";
							1;
						}

	alphanumeric	:	/[a-zA-Z0-9_]+/
						{ # print STDERR  "FOUND ALPHANUMERIC $item[1]\n";
							$return = $item[1] 
						}

	number			:	/[0-9]+/
						{  # print STDERR  "FOUND DIGIT $item[1]\n"; 
							$return = $item[1] 
						}

	alpha			:	/[a-zA-Z]+/
						{  # print STDERR  "FOUND ALPHA $item[1]\n"; 
							$return = $item[1] 
						}

	SEMI			:	';'
	COLON			:	':'
	LT				:	'<'
	GT				:	'>'
	LBRACKET		:	'['
	RBRACKET		:	']'
	DOT				:	'.'
	LPAREN			:	'('
	RPAREN			:	')'
	COMMA			:	','
	INTEGER			:	/[-+]?\d+/      			# Signed integers
	VARIABLE		:	/\w[a-z0-9_]*/i 			# Variable

END_GRAMMAR

$block_parsers{subcells} = new Parse::RecDescent q{

	top				:	contents	
						{ # print STDERR  "FOUND EVERYTHING!!!\n"; 
							$return = $item[1];
						}

	contents		:	var contents
						{ # print STDERR  "FOUND VAR followed by CONTENTS\n";
							$return = CastParserLib::_subcell_merge_contents($item[2] )
						}
					|	instantiation contents
						{ # print STDERR  "FOUND INST followed by CONTENTS\n"; 
							my %contents;
							push @{$contents{instances}}, @{$item[1]};
							$return = CastParserLib::_subcell_merge_contents(\%contents,$item[2]);
						}
					|	array contents
						{ # print STDERR  "FOUND ARRAY followed by CONTENTS\n";
							$return = CastParserLib::_subcell_merge_contents($item[1],$item[2]);
						}
					|	alias contents
						{ # print STDERR  "FOUND ALIAS followed by CONTENTS\n";
							$return = CastParserLib::_subcell_merge_contents($item[2] )
						}
					|	conditional contents
						{ # print STDERR  "FOUND CONTENTS FOLLOWED BY CONDITIONAL\n";
							$return = CastParserLib::_subcell_merge_contents($item[1],$item[2]);
						}
					|	var	
						{ # print STDERR  "FOUND CONTENTS: VAR $item[1]\n";
							$return = CastParserLib::_subcell_merge_contents();	
						}
					|	instantiation
						{ # print STDERR  "FOUND CONTENTS: INST\n";
							my %contents;
							push @{$contents{instances}}, @{$item[1]};
							$return = CastParserLib::_subcell_merge_contents(\%contents)
						}
					|	array
						{ # print STDERR  "FOUND CONTENTS: ARRAY\n";
							$return = CastParserLib::_subcell_merge_contents($item[1])
						}
					|	conditional
						{ # print STDERR  "FOUND CONDITIONAL CONTENTS\n";
							$return = CastParserLib::_subcell_merge_contents($item[1] )
						}
					|	alias	
						{ # print STDERR  "FOUND CONTENTS: ALIAS $item[1]\n";
							$return = CastParserLib::_subcell_merge_contents();	
						}

	array			:	LT alpha COLON chanrange COLON contents GT
						{ # print STDERR  "FOUND CONTENTS ARRAY: <$item[2]:";
							# print STDERR CastParserLib::format_chanrange($item[4]);
							# print STDERR ": $item[6]>\n";
							my $index = $item[2];
							my $range = $item[4];
							my %contents;
							foreach my $inst (@{$item[6]->{instances}})
							{
								push @{$contents{instances}}, CastParserLib::_unroll_inst_array($index,$range,$inst);
							}
							$return = \%contents;
						}

	instantiation	:	celltype '(' params ')' decl '(' decls ')' SEMI
						{ # print STDERR  "FOUND INST1: $item[1]\n";
							my %inst;
							$inst{type} = $item[1];
							@{$inst{metaparams}} = @{$item[3]};
							$inst{instance} = $item[5];
							$inst{params} = $item[7];
							$return = [\%inst];
						}
					|	celltype decl '(' decls ')' SEMI
						{ # print STDERR  "FOUND INST2: $item[1]\n"; 
							my %inst;
							$inst{type} = $item[1];
							$inst{instance} = $item[2];
							$inst{params} = $item[4];
							$return = [\%inst];
						}
					|	celltype '(' params ')' decl SEMI
						{ # print STDERR  "FOUND INST3: $item[1]\n"; 
							my %inst;
							$inst{type} = $item[1];
							@{$inst{metaparams}} = @{$item[3]};
							$inst{instance} = $item[5];
							$return = [\%inst];
						}
					|	celltype decls SEMI
						{ # print STDERR  "FOUND INST4: $item[1] $item[2]\n"; 
							my @instances;
							foreach my $decl (@{$item[2]})
							{
								my %inst;
								$inst{type} = $item[1];
								$inst{instance} = $decl;
								push @instances, \%inst;
							}
							$return = \@instances;
						}
					|	celltype decl SEMI
						{ # print STDERR  "FOUND INST5: $item[1] $item[2]\n"; 
							my %inst;
							$inst{type} = $item[1];
							$inst{instance} = $item[2];
							$return = [\%inst];
						}
					|	celltype instantiation	
						{ # print STDERR  "FOUND INST6: $item[1] $item[2]\n";
							my @results;
							foreach my $inst (@{$item[2]})
							{
								my $newinst = $inst;
								push @{$newinst->{keywords}}, $item[1];
								push @results, $newinst;
							}
							$return = \@results;
						}

	celltype		:	<leftop: alphanumeric DOT alphanumeric>
						{ # print STDERR "FOUND CELLTYPE: @{$item[1]}\n";
							$return = "";
							foreach my $piece(@{$item[1]})
							{
								$return .= $piece;
							}
						}
};

$block_parsers{def} = new Parse::RecDescent q{

	top				:	metaparams LPAREN portlist RPAREN
						{ # print STDERR  "FOUND EVERYTHING!!!\n"; 
							my %def;
							$def{metaparams} = $item[1];
							$def{portlist} = $item[3];
							$return = \%def;
						}

	portlist		:	whole_port (';' whole_port)(s?)
						{ # print STDERR  "FOUND PORTLIST: \"@{$item[1]}\"";
							my @a;
							foreach my $port (@{$item[1]})
							{
								push @a,$port;
							}
							foreach my $portlist (@{$item[2]})
							{
								foreach my $port (@{$portlist})
								{
									push @a,$port;
								}
							}
							$return = \@a;
						}

	whole_port		:	alphanumeric LPAREN params RPAREN LBRACKET chanarrayparam RBRACKET ports
						{ # print STDERR  "FOUND WHOLE PORT 1: \"@item\"\n";
							my @a;
							foreach my $port (@{$item[8]})
							{
								my %port_info;
								$port_info{name} = $port;
								$port_info{type} = $item[1];
								$port_info{metaparams} = $item[3];
								$port_info{arrayparam} = $item[6];
								push @a,\%port_info;
							}
							$return = \@a;
						}
					|	alphanumeric LPAREN params RPAREN ports
						{ # print STDERR  "FOUND WHOLE PORT 2: \"@item\"\n";
							my @a;
							foreach my $port (@{$item[5]})
							{
								my %port_info;
								$port_info{name} = $port;
								$port_info{type} = $item[1];
								$port_info{metaparams} = $item[3];
								push @a,\%port_info;
							}
							$return = \@a;
						}
					|	alphanumeric LBRACKET chanarrayparam RBRACKET ports
						{ # print STDERR  "FOUND WHOLE PORT 3: $item[1]\[$item[3]\] @{$item[5]}\n";
							my @a;
							foreach my $port (@{$item[5]})
							{
								my %port_info;
								$port_info{name} = $port;
								$port_info{type} = $item[1];
								$port_info{arrayparam} = $item[3];
								push @a,\%port_info;
							}
							$return = \@a;
						}
					|	alphanumeric ports
						{ # print STDERR  "FOUND WHOLE PORT 4: \"$item[1] $item[2]\"\n";
							my @a;
							foreach my $port (@{$item[2]})
							{
								my %port_info;
								$port_info{name} = $port;
								$port_info{type} = $item[1];
								push @a,\%port_info;
							}
							$return = \@a;
						}

	ports			:	<leftop: port ',' port>

	port			:	direction decl
						{ # print STDERR "FOUND DIR-PORT: $item[1]$item[2]\n";
							my $port_name;
							$port_name = $item[2];
							$port_name->{direction} = $item[1];
							$return = $port_name;
						}

	metaparams		:	LPAREN string params RPAREN
						{ # print STDERR  "FOUND METAPARAMS: \"($item[2] $item[3])\"\n";
							$return = $item[1]
						}
					|	LPAREN RPAREN
						{ # print STDERR  "FOUND EMPTY METAPARAMS\n";
							$return = ""
						}

	direction		:	'+'
						{ # print STDERR  "DIRECTION: \"+\"\n";
							$return = $item[1]
						}
					|	'-'
						{ # print STDERR  "DIRECTION: \"-\"\n";
							$return = $item[1]
						}
};

$block_parsers{prs} = new Parse::RecDescent q{
	top				:	contents
						{ # print STDERR  "FOUND EVERYTHING!!!\n"; 
							$return = $item[1];
						}

	contents		:	prs_array contents
						{ my $contents = $item[2];
							push @{$contents->{prs}}, $item[1];
							$return = $contents;
						}
					|	prs contents
						{ my $contents = $item[2];
							push @{$contents->{prs}}, $item[1];
							$return = $contents;
						}
					|	whole_decl contents
						{ my $contents = $item[2];
							push @{$contents->{decl}}, @{$item[1]};
							$return = $contents;
						}
					|	prs_array(s)
						{ my %contents;
							push @{$contents{prs}}, @{$item[1]};
							$return = \%contents;
						}
					|	prs(s)
						{ my %contents;
							push @{$contents{prs}}, @{$item[1]};
							$return = \%contents;
						}
					|	whole_decl(s)
						{ my %contents;
							foreach my $decl (@{$item[1]})
							{
								push @{$contents{decl}}, @{$decl};
							}
							$return = \%contents;
						}
					|	conditional	
						{ # print STDERR  "FOUND CONTENTS: CONDITIONAL\n";
							$return = $item[1] 
						}

	prs_array		:	LT alpha COLON number COLON prs_array GT
						{ # print STDERR "FOUND PRS_ARRAY: <$item[2]:$item[4]: $item[6]>\n";
							 $return = "<$item[2]:$item[4]: $item[6]>";
						}
					|	LT alpha COLON number COLON prs GT
						{ # print STDERR "FOUND PRS_ARRAY: <$item[2]:$item[4]: $item[6]>\n";
							 $return = "<$item[2]:$item[4]: $item[6]>";
						}

	prs				:	prs_expression prs_op prs_result
						{ # print STDERR "FOUND PRS: $item[1] $item[2] $item[3]\n";
							 $return = "$item[1] $item[2] $item[3]";
						}

	prs_result		:	node '-'
						{ # print STDERR "FOUND PRS_RESULT: $item[1]-\n";
							 $return = "$item[1]-";
						}
					|	node '+'
						{ # print STDERR "FOUND PRS_RESULT: $item[1]+\n";
							 $return = "$item[1]+";
						}

	prs_op			:	'->'
						{ # print STDERR "FOUND PRS_OP: $item[1]\n";
							 $return = "$item[1]";
						}
					|	'=>'
						{ # print STDERR "FOUND PRS_OP: $item[1]\n";
							 $return = "$item[1]";
						}
					|	'#>'
						{ # print STDERR "FOUND PRS_OP: $item[1]\n";
							 $return = "$item[1]";
						}

	prs_expression	:	<leftop: prs_term '|' prs_term >

	prs_term		:	<leftop: prs_factor '&' prs_factor >

	prs_factor		:	LPAREN prs_expression RPAREN
						{ # print STDERR "FOUND PRS_FACTOR: ($item[2])\n";
							$return = "($item[2])";
						}
					|	'~' prs_expression
						{ # print STDERR "FOUND PRS_FACTOR: ~$item[2]\n";
							$return = "~$item[2]";
						}
					|	node
						{ # print STDERR "FOUND PRS_FACTOR: $item[1]\n"; 
							$return = $item[1];
						}

	node			:	/[a-zA-Z0-9_\.]+/ LBRACKET expression RBRACKET
						{ # print STDERR "FOUND NODE: $item[1]\[$item[3]\]\n";
							 $return = "$item[1]\[$item[3]\]";
						}
					|	/[a-zA-Z0-9_\.]+/
						{ # print STDERR "FOUND NODE: $item[1]\n";
							 $return = $item[1];
						}
};

use vars qw(%VARIABLE);

my $expression_parser = new Parse::RecDescent q{

	INTEGER		:	/[-+]?\d+/      			# Signed integers
	VARIABLE	:	/\w[a-z0-9_]*/i 			# Variable

	expression	:	term '+' expression
					{ $return = "$item[1] $item[2] $item[3]";
						$return = $item[1]+$item[3] if( not $item[1] =~ /[a-zA-Z]/ and not $item[3] =~ /[a-zA-Z]/ )
					}
				|	term '-' expression
					{ $return = "$item[1] $item[2] $item[3]";
						$return = $item[1]-$item[3] if( not $item[1] =~ /[a-zA-Z]/ and not $item[3] =~ /[a-zA-Z]/ )
					}
				|	term

	term		:	factor '*' term 
					{ $return = "$item[1] $item[2] $item[3]";
						$return = $item[1]*$item[3] if( not $item[1] =~ /[a-zA-Z]/ and not $item[3] =~ /[a-zA-Z]/ )
					}
				|	factor '/' term
					{ $return = "$item[1] $item[2] $item[3]";
						$return = $item[1]/$item[3] if( not $item[1] =~ /[a-zA-Z]/ and not $item[3] =~ /[a-zA-Z]/ )
					}
				|	factor '%' term
					{ $return = "$item[1] $item[2] $item[3]";
						$return = $item[1]%$item[3] if( not $item[1] =~ /[a-zA-Z]/ and not $item[3] =~ /[a-zA-Z]/ )
					}
				|	factor

	factor		:	'(' expression ')'
						{ $return = "($item[2])" }
				|	INTEGER
				|	VARIABLE
					{ $return = $item[1];
						$return=$main::VARIABLE->{$item[1]} if( defined $main::VARIABLE->{$item[1]} );
						$return=$main::VARIABLE->{__ALL__} if( defined $main::VARIABLE->{__ALL__} );
						# print STDERR "Found VARIABLE $item[1] = \"$return\"\n";
					}
};

my %eval_cache;
my %eval_num_cache;

sub _eval_expr
{
	my $expr = shift;
	my $var = shift;
	my $i = shift;

	if( defined $var and defined $i )
	{
		return $eval_cache{$expr}{$var}{$i} if( defined $eval_cache{$expr}{$var}{$i} );
	}
	else
	{
		return $eval_num_cache{$expr} if( defined $eval_num_cache{$expr} );
	}

	$main::VARIABLE = ();
	if( ref($var) eq "HASH" )
	{
		$main::VARIABLE = $var;
	}
	else
	{
		return eval "$expr" if( not $expr =~ /$var/ );
		$main::VARIABLE->{$var} = $i if( $var );
	}

	my $result = $expression_parser->expression($expr);
	if( defined $var and defined $i )
	{
		$eval_cache{$expr}{$var}{$i} = $result;
	}
	else
	{
		$eval_num_cache{$expr} = $result;
	}
	return $result;
}

my %parse_cache;
my $first = 1;

sub init_parsers
{
	foreach my $block (keys %block_parsers)
	{
		$block_parsers{$block}->Extend($common_grammar);
	}
	$first = 0;
}

sub parse_block
{
	my $cellname = shift;
	my $block = shift;

	return $parse_cache{$cellname}{$block} if( defined $parse_cache{$cellname}{$block} );
	init_parsers if( $first );

	my $data = CastLib::get_block_contents($cellname,$block);
	return if( $data eq "" );
	$data = `echo \"$data\" | cpp -P`;
	$data =~ s/^\s*//;
	$data =~ s/\s*$//;
	return if( $data eq "" );

	$parse_cache{$cellname}{$block} = $block_parsers{$block}->top( $data );
	return $parse_cache{$cellname}{$block};
}

sub parse_def
{
	my $cellname = shift;
	return parse_block($cellname,"def");
}

sub new_parse_subcells
{
	my $cellname = shift;
	return parse_block($cellname,"subcells");
}

sub parse_prs
{
	my $cellname = shift;
	return parse_block($cellname,"prs");
}

# Returns list of subcells 
sub parse_subcells 
{
	my $subcells_block = shift;
	my $ignore_inline = shift;

    open TMPFILE, ">tmp_$$" or die "Could not write to tmp_$$";
    print TMPFILE "$subcells_block\n";
    close TMPFILE;
	my $cell_contents = `cat tmp_$$ | cpp -P`;
    unlink "tmp_$$";

#	$cell_contents =~ s/\n//g;
	
	my @lines = split ";",$cell_contents;

	my @identifiers = ("node ","e1of","_e1of","a1of","_a1of","_1of","1of","int ","bool ");

	my %subcells = ();
	foreach my $line (@lines)
	{
		# Look for [ cond -> foo ] statements on one line, and keep foo
		$line = $1 if( $line =~ /\s*\[.*\s+\-\>\s+(.*)/ );

		# Remove everything from beginning of line up to the first character
		# that is alpha or an _ (valid chars with which to start cell name)
		# Have to include digits so that we can filter out things like 1of2
		# And include < to filter out <i:18: type stuff
		$line =~ s/^[^a-zA-Z_0-9\<]*//g;

		# Remove stuff having to do with loops
		# <i:##: and closing >
		$line =~ s/\<[a-zA-Z]+\:[^:]+:\s+//g;
		$line =~ s/\s*[>]+\s*//g;

		next if( not $ignore_inline and $line =~ /^flatten/ );
		next if( not $ignore_inline and $line =~ /^inline\s/ );

		$line =~ s/^inline1\s+//g;
		$line =~ s/^inline\s+//g;
		$line =~ s/^flatten\s+//g;
		my $id = 0;
		foreach my $ident (@identifiers)
		{
			$id = 1 if( $line =~ /^${ident}/ );
		}
		if( not $id and not $line =~ /\=/ )
		{
			# Remove everything between ()  Must be in while loop to
			# get nested, such as: ((15-N)%4)
			while( $line =~ s/\([^)(]*\)//g ) {}
			# Should be left with a string containing
			# CELLNAME instance_name, so match CELLNAME
			if( $line =~ /([A-Z0-9_a-z]+)\s+[a-zA-Z0-9_]+/ )
			{
				$subcells{$1} = "";
			}
		}
	}
	return keys %subcells;
}

1;

