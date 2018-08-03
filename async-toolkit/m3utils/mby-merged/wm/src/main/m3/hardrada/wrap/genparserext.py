#!/usr/bin/env python3

######################
### genparserext.py ##
### by Roman Parise ##
######################

###########
# Imports #
###########
import os.path
import argparse
import re
import os
import constants

#############
# Constants #
#############
# Program arguments
TOKEN_ARG = "--token"
LEX_ARG = "--lexer"
PARSE_ARG = "--parser"
EXTTOK_ARG = "--exttok"
EXTLEX_ARG = "--extlex"
EXTPARSE_ARG = "--extparse"
NAMES_ARG = "--names"
# Names file
ID = "MYID"
# Regex
TOKEN_NAME_REGEX = r"[a-zA-Z_][a-zA-Z0-9_]*"
CONTROLLED_NONWS = r"[^ \t\n\r\f]"
CONTROLLED_WS = r"[ \t]"

##############
# Exceptions #
##############
class NotAbsPathError( Exception ) :
	pass

class InvalidFilePathError( Exception ) :
	pass

class NoTokensError( Exception ) :
	pass

class NoStartSymbolError( Exception ) :
	pass

class NoNonTermsError( Exception ) :
	pass

class MultipleStartSymbolError( Exception ) :
	pass

class InvalidIDError( Exception ) :
	def __init__( self , invalid_id ) :
		self.invalid_id = invalid_id

class MultipleLexDefError( Exception ) :
	pass

class MiscError( Exception ) :
	pass

###############
## Functions ##
###############

## Program arguments ##

def usage( ) :
	print( os.path.basename( __file__ ) )
	print( "==========" )
	print( "Generates parser extension files for hardrada." )
	print( "Mandatory Arguments: " )
	print( TOKEN_ARG + " :: absolute path to input *.t token spec file" )
	print( LEX_ARG + " :: absolute path to input *.l lexer spec file" )
	print( PARSE_ARG + " :: absolute path to input *.y parser spec file" )
	print( NAMES_ARG + " :: absolute path to input *.names parser spec file" )
	print( EXTTOK_ARG + " :: absolute path to output *.e token spec file" )
	print( EXTLEX_ARG + " :: absolute path to output *.e lexer spec file" )
	print( EXTPARSE_ARG + " :: absolute path to output *.e parser spec file" )

def check_input_arg( input_arg : str ) :
	# Must be an absolute path
	if not os.path.isabs( input_arg ) :
		raise NotAbsPathError
	# Must be a valid file
	if not os.path.isfile( input_arg ) :
		raise InvalidFilePathError

def check_output_arg( input_arg : str ) :
	# Must be an absolute path
	if not os.path.isabs( input_arg ) :
		raise NotAbsPathError
	# Must not be a valid file
	if os.path.isfile( input_arg ) :
		raise InvalidFilePathError
	# Check if file would have valid M3 interface name

## Auxiliary functions ##

# parse_list
# Parse a list of the form "start element1 element2 ... elementN"
# within the text text_to_parse. End of line is assumed after elementN.
# Return an array [element1,element2,...,elementN] 
# element1,element2,...,elementN are assumed to follow the regex [a-zA-Z_][a-zA-Z0-9_]*.
# If start_opt is True, then the start token "start" is optional.
# So, "element1 element2 ... elementN" and "start element1 ..."
# would return the same list.
# Note: If there are multiple lines with the start symbol, the result of each is
# concatenated and returned.
# If no tokens are found in the text, NoTokensError exception is thrown.
def parse_list( start : str , text_to_parse : str , start_opt : bool = False , regex_token : str = TOKEN_NAME_REGEX ) :

	# Regexes - use just to grab the line with the tokens
	regex = ""
	# Beginning of line with optional whitespace
	regex += r"^" + CONTROLLED_WS + r"*"
	# Potentially optional token keyword with at least one subsequent
	# whitespace character
	if start_opt == True :
		regex += r"(?:" + start + CONTROLLED_WS + r"+)*"
	else :
		regex += start + CONTROLLED_WS + r"+"
	# Optional tokens preceding the one mandatory token
	regex += r"(?:" + regex_token + CONTROLLED_WS + "+)*"
	# The one mandatory token and end of line
	regex += regex_token + CONTROLLED_WS + "*$"
	# Capture tokens from text
	list_of_tokens = [ ]

	capture = re.findall( regex , text_to_parse , re.M )
	if capture :
		# TODO Are these splits going to work well if the spacing is a bit odd?
		for cap in capture :
			current_toklist = cap.split( )
			if current_toklist[ 0 ] == start :
				list_of_tokens += current_toklist[ 1: ]
			elif start_opt == True :
				list_of_tokens += current_toklist
			else : # TODO Just as a sanity check for your regex
				raise MiscError( )
	else :
		raise NoTokensError

	return list_of_tokens

## parse_names_file
#
# Format:
#
# ID id1 id2 ... idN
#
# ids and keys are METHOD names defined in the *.l lexer
# spec file.
# Return a hashtable: { ID : [ "id1" , ... , "idN" ] }
# TODO Error check. Make sure you don't have multiple occurrences of
# ID or anything other than that.
def parse_names_file( names_path : str ) :
	ids_list = [ ]
	with open( names_path , "r" ) as names_fhandle :
		ftext = names_fhandle.read( )
		try :
			ids_list = parse_list( ID , ftext )
		except NoTokensError :
			ids_list = [ ]
	return { ID : ids_list }

# Get tokens from token specfile
# Returns...
# { "nonconst" : [ "tok1" , ... , "tokN" ] ,
#   "const" : [ "const_tok1" , ... , "const_tokM" ] }
# Either one array or both arrays can be empty.
# Assumes arguments were checked by the program's main function.
# As a result, do not call this function separately.
# TODO Check that these can cover the full spectrum of *.tok formats.
def get_tokens( token_spec_path : str ) :
	list_of_tokens = [ ]
	list_of_consts = [ ]
	with open( token_spec_path , "r" ) as token_spec_fhandle :
		ftext = token_spec_fhandle.read( )
		try :
			list_of_tokens = parse_list( r"%token" , ftext , True )
		except NoTokensError :
			list_of_tokens = [ ]
		try :
			list_of_consts = parse_list( r"%const" , ftext )
		except NoTokensError :
			list_of_consts = [ ]
	return { "nonconst" : list_of_tokens , "const" : list_of_consts }

# Returns a hashtable...
# [ const_token_name => value ]
# Constant tokens that are not defined in the lexer spec are excluded
def get_const_token_values( token_spec_path : str , lexer_spec_path : str ) :
	list_of_consts = get_tokens( token_spec_path )[ 'const' ]
	return_result = { }
	if list_of_consts == [ ] :
		return return_result
	with open( lexer_spec_path , "r" ) as lexer_spec_fhandle :
		lspec_text = lexer_spec_fhandle.read( )
		for const_token in list_of_consts :
			try :
				values = parse_list( const_token , lspec_text , False , CONTROLLED_NONWS + r"*" )
			except NoTokensError :
				continue
			if len( values ) == 1 :
				return_result[ const_token ] = values[ 0 ]
			else :
				raise MultipleLexDefError( )
	return return_result

# module = True -> module imports
# module = False -> interface imports
# Returns a string of the code for importing in that particular parser extension file
# TODO Doesn't seem to be much of a use case for this function. Consider eliminating.
def gen_imports_list( imports_list : list , module : bool = True ) :
	imports_src = ""
	if imports_list != [ ] :
		if module == True :
			imports_src += "%module{\n"
		else :
			imports_src += "%interface{\n"
		for _import in imports_list :
			imports_src += "IMPORT " + _import + " ;\n"
		imports_src += "}\n\n"
	return imports_src

## Code Generators ##

# Token ext generation
# Assumes arguments were checked by the program's main function.
# As a result, do not call this function separately.
def gen_token_ext( token_spec_path : str , tok_ext_path : str ) :
	with open( tok_ext_path , "w" ) as tok_ext_fhandle :

		# Write file header
		token_spec_path_base = os.path.basename( token_spec_path )
		# TODO Any way to make this.... correct by design? What I somehow rename the lexer interface?
		interface_name = os.path.splitext( os.path.basename( token_spec_path ) )[ 0 ] + "Tok"
		tok_ext_fhandle.write( "%source " + token_spec_path_base + "\n" )
		tok_ext_fhandle.write( "%import " + interface_name + "\n\n" )
		tok_ext_fhandle.write( gen_imports_list( [ "Node" ] , False ) )

		# Parse token_spec_path for all tokens
		mytoks = get_tokens( token_spec_path )

		# Generate list of tokens
		for current_token in mytoks[ "nonconst" ] :
			tok_ext_fhandle.write( current_token + ": {val: REF Node.T}\n" )

# Lexer ext generation
# Assumes arguments were checked by the program's main function.
# As a result, do not call this function separately.
def gen_lexer_ext( token_spec_path : str , lexer_spec_path : str , tok_ext_path : str , names_path : str , lexer_ext_path : str ) :

	with open( lexer_ext_path , "w" ) as lexer_ext_fhandle :
		
		# Generate file header
		token_spec_path_base = os.path.basename( token_spec_path )
		lexer_spec_path_base = os.path.basename( lexer_spec_path )
		tokext_interface_name = os.path.splitext( os.path.basename( tok_ext_path ) )[ 0 ]
		# TODO Any way to make this.... correct by design? What I somehow rename the lexer interface?
		lexer_interface_name = os.path.splitext( os.path.basename( lexer_spec_path ) )[ 0 ] + "Lex"

		# Top portion
		lexer_ext_fhandle.write( "%source " + token_spec_path_base + " " + lexer_spec_path_base + "\n" )
		lexer_ext_fhandle.write( "%import " + tokext_interface_name + " " + lexer_interface_name + "\n" )
		lexer_ext_fhandle.write( "\n" )
		
		# Imports
		imports = [ "Node" ]
		lexer_ext_fhandle.write( gen_imports_list( imports , True ) )

		# Generate expression method definitions
		mytoks = get_tokens( token_spec_path )
		names = parse_names_file( names_path )
		for current_token in mytoks[ "nonconst" ] :
			lexer_ext_fhandle.write( current_token + " {$R " + current_token + "{ $$ := NEW( REF Node.T , val := $ , children := NIL ," )
			if current_token in names[ ID ] :
				lexer_ext_fhandle.write( " cat := Node.Category.Identifier ) ; } }\n" )
			else :
				lexer_ext_fhandle.write( " cat := Node.Category.Constant ) ; } }\n" )

# Generate parser ext
# Assumes arguments were checked by the program's main function.
# As a result, do not call this function separately.
def gen_parser_ext( token_spec_path : str , lexer_spec_path : str , parser_spec_path : str , tok_ext_path : str , names_path : str , parser_ext_path : str ) :
	token_spec_path_base = os.path.basename( token_spec_path )
	parser_spec_path_base = os.path.basename( parser_spec_path )
	tokext_interface_name = os.path.splitext( os.path.basename( tok_ext_path ) )[ 0 ]
	parser_interface_name = os.path.splitext( os.path.basename( parser_spec_path ) )[ 0 ] + "Parse"
	try :
		const_token_vals = get_const_token_values( token_spec_path , lexer_spec_path )
	except MultipleLexDefError :
		raise MultipleLexDefError( )

	# Parse parser specification for useful Intel... pun intended. :)
	start_symbol = ""
	nonterminal_rules = {}
	grule_toks = {}
	with open( parser_spec_path , "r" ) as parser_spec_fhandle :
		program_text = parser_spec_fhandle.read( )
		# Get start symbol for grammar
		start_symbol_capture = lambda text : re.findall( r"^%start" + CONTROLLED_WS + "+(" + TOKEN_NAME_REGEX + r")" + CONTROLLED_WS + "*$" , text , re.M )
		start_symbol_capture_list = start_symbol_capture( program_text )
		if start_symbol_capture_list == [ ] :
			# No start symbol for language grammar
			raise NoStartSymbolError( )
		elif len( start_symbol_capture_list ) > 1 :
			# Multiple start symbols
			raise MultipleStartSymbolError( )
		else :
			start_symbol = start_symbol_capture_list[ 0 ]
		# Remove line from string
		program_text = re.sub( r"^%start\s+" + start_symbol + r"\s*$" , "" , program_text )
		# Get each nonterminal
		# First, construct the regex
		nonterminal_block_regex = r""
		# Beginning of line
		nonterminal_block_regex += r"^"
		# Nonterm name
		nonterminal_block_regex += TOKEN_NAME_REGEX + r":\s*\n"
		# Grammar rule name
		nonterminal_block_regex += r"^(?:" + CONTROLLED_WS + "+" + TOKEN_NAME_REGEX 
		# Each token of grammar rule
		nonterminal_block_regex += r"(?:" + CONTROLLED_WS + "+" + CONTROLLED_NONWS + r"+)*\s*)+$"
		nonterminal_block_match = re.findall( nonterminal_block_regex , program_text , re.M )
		if nonterminal_block_match == [ ] :
			raise NoNonTermsError( )
		while nonterminal_block_match != [ ] :
			# Delete block from text
			block_text = nonterminal_block_match[ 0 ]
			program_text = re.sub( block_text , "" , program_text )
			nonterminal_block_match = nonterminal_block_match[ 1: ]
			# Get nonterm name
			nonterm_match = re.findall( "^(" + TOKEN_NAME_REGEX + "):\s*\n" , block_text , re.M )
			# Just for debugging purposes, in case I missed something
			# in the regex
			if len( nonterm_match ) != 1 :
				raise MiscError( )
			nonterm = nonterm_match[ 0 ]
			# Remove nonterm from block text
			block_text = re.sub( "^" + nonterm + ":\s*\n" , "" , block_text )
			# Get each grammar rule for the nonterminal
			grules_for_nonterm = [ ]
			grammar_rule_match = re.findall( "^" + CONTROLLED_WS + r"+" + TOKEN_NAME_REGEX + r"(?:" + CONTROLLED_WS + r"+" + CONTROLLED_NONWS + r"+)*" , block_text , re.M )
			while grammar_rule_match != [ ] :
				# TODO Are these splits going to work well if the spacing is a bit odd?
				grammar_rule_block_text = grammar_rule_match[ 0 ]
				grammar_rule_match = grammar_rule_match[ 1: ]
				toklist = grammar_rule_block_text.split( )
				grule_name = toklist[ 0 ]
				grules_for_nonterm.append( grule_name )
				grule_toks[ nonterm + "." + grule_name ] = toklist[ 1: ]
				# Remove grammar rule from block text
				block_text = re.sub( grammar_rule_block_text , "" , block_text )
			nonterminal_rules[ nonterm ] = grules_for_nonterm

	with open( parser_ext_path , "w" ) as parser_ext_fhandle :

		# Generate header
		parser_ext_fhandle.write( "%source " + token_spec_path_base + " " + parser_spec_path_base + "\n" )
		parser_ext_fhandle.write( "%import " + tokext_interface_name + " " + parser_interface_name + "\n\n" )

		parser_ext_fhandle.write( "%interface{\n" )
		parser_ext_fhandle.write( "IMPORT Node ;\n" )
		parser_ext_fhandle.write( "VAR root : REF Node.T := NIL ;\n" )
		parser_ext_fhandle.write( "PROCEDURE GetParseTree( ) : REF Node.T ;\n" )
		parser_ext_fhandle.write( "}\n\n" )

		parser_ext_fhandle.write( "%module{\n" )
		parser_ext_fhandle.write( "IMPORT Node ;\n" )
		parser_ext_fhandle.write( "PROCEDURE GetParseTree( ) : REF Node.T =\n" )
		parser_ext_fhandle.write( "BEGIN\n" )
		parser_ext_fhandle.write( "\tRETURN root ;\n" )
		parser_ext_fhandle.write( "END GetParseTree ;\n" )
		parser_ext_fhandle.write( "}\n\n" )

		# Generate body

		# For each nonterminal
		for nonterm in nonterminal_rules.keys( ) :
			parser_ext_fhandle.write( nonterm + ": {val: REF Node.T}\n" )
			# For each grammar rule corresponding to the nonterminal
			for grule in nonterminal_rules[ nonterm ] :
				# Create a node...
				parser_ext_fhandle.write( "\t" + grule + "\n" )
				parser_ext_fhandle.write( "\t\t{ VAR newnode : REF Node.T := NEW( REF Node.T ) ;\n" )
				parser_ext_fhandle.write( "\t\tBEGIN\n" )
				# Whose value is the name of the nonterminal . the name of the grammar rule
				parser_ext_fhandle.write( "\t\tnewnode^.val := \"" + nonterm + "." + grule + "\" ;\n" )
				token_list = grule_toks[ nonterm + "." + grule ]
				if grule_toks[ nonterm + "." + grule ] != [ ] :
					# If the token list corresponding to this grammar rule is nonempty...
					# The node will have children, namely its tokens and other grammar rules invoked
					nonconst_ctr = 0
					# For each token/grammar rule in the token list
					parser_ext_fhandle.write( "\t\tVAR children := NEW( REF ARRAY OF REF Node.T , " + str( len( grule_toks[ nonterm + "." + grule ] ) ) + " ) ;\n" )
					parser_ext_fhandle.write( "\t\tBEGIN\n" )
					for token_index in range( 0 , len( grule_toks[ nonterm + "." + grule ] ) ) :
						if token_list[ token_index ].startswith( "\'" ) == True and token_list[ token_index ].endswith( "\'" ) == True :
							# If in single-quotes, then it is a constant and therefore a terminal node
							# TODO Check for if this is a valid constant from Calc.t
							parser_ext_fhandle.write( "\t\tchildren[ FIRST( children^ ) + " + str( token_index ) + " ] := " )
							parser_ext_fhandle.write( "NEW( REF Node.T , val := \"" + token_list[ token_index ] + "\" , cat := Node.Category.Constant , " )
							parser_ext_fhandle.write( "children := NIL ) ;\n" )
						else :
							try :
								# The token could also be constant token.
								tokval = const_token_vals[ token_list[ token_index ] ]
								parser_ext_fhandle.write( "\t\tchildren[ FIRST( children^ ) + " + str( token_index ) + " ] := " )
								parser_ext_fhandle.write( "NEW( REF Node.T , val := " + tokval + " , cat := Node.Category.Constant , " )
								parser_ext_fhandle.write( "children := NIL ) ;\n" )
							except KeyError :
								# Otherwise, it is a nonterminal
								nonconst_ctr += 1
								parser_ext_fhandle.write( "\t\tchildren[ FIRST( children^ ) + " + str( token_index ) + " ] := $" + str( nonconst_ctr ) + " ;\n" )
					parser_ext_fhandle.write( "\t\tnewnode^.children := children ;\n" )
					parser_ext_fhandle.write( "\t\tEND;\n" )
					parser_ext_fhandle.write( "\t\tnewnode^.cat := Node.Category.NonTerminal ;\n" )
				else :
					# If the token list is empty, we say this is a constant by convention
					parser_ext_fhandle.write( "\t\tnewnode^.children := NIL ;\n" )
					parser_ext_fhandle.write( "\t\tnewnode^.cat := Node.Category.Constant ;\n" )
				if nonterm == start_symbol :
					# Set the root node to this node if the start symbol is the corresponding nonterm
					parser_ext_fhandle.write( "\t\troot := newnode ;\n" )
				parser_ext_fhandle.write( "\t\t$$ := newnode ;\n" )
				parser_ext_fhandle.write( "\t\tEND ;\n" )
				parser_ext_fhandle.write( "\t\t}\n" )
			parser_ext_fhandle.write( "\n" )

########
# Main #
########
if __name__ == "__main__" :

	my_arg_parser = argparse.ArgumentParser( usage = usage( ) )
	my_arg_parser.add_argument( TOKEN_ARG , required = True , type = str )
	my_arg_parser.add_argument( LEX_ARG , required = True , type = str )
	my_arg_parser.add_argument( PARSE_ARG , required = True , type = str )
	my_arg_parser.add_argument( NAMES_ARG , required = True , type = str )
	my_arg_parser.add_argument( EXTTOK_ARG , required = True , type = str )
	my_arg_parser.add_argument( EXTLEX_ARG , required = True , type = str )
	my_arg_parser.add_argument( EXTPARSE_ARG , required = True , type = str )
	my_args = my_arg_parser.parse_args( )

	# Get args
	token_arg_result = my_args.token
	lexer_arg_result = my_args.lexer
	parser_arg_result = my_args.parser
	names_arg_result = my_args.names
	exttok_arg_result = my_args.exttok
	extparse_arg_result = my_args.extparse
	extlex_arg_result = my_args.extlex

	# Check args
	try :
		check_input_arg( token_arg_result )
	except NotAbsPathError :
		print( TOKEN_ARG + " arg value must be absolute path." )
		exit( constants.FAILURE )
	except InvalidFilePathError :
		print( TOKEN_ARG + " arg is a file that does not exist." )
		print( "Please provide one that does." )
		exit( constants.FAILURE )

	try :
		check_input_arg( lexer_arg_result )
	except NotAbsPathError :
		print( LEX_ARG + " arg value must be absolute path." )
		exit( constants.FAILURE )
	except InvalidFilePathError :
		print( LEX_ARG + " arg is a file that does not exist." )
		print( "Please provide one that does." )
		exit( constants.FAILURE )

	try :
		check_input_arg( parser_arg_result )
	except NotAbsPathError :
		print( PARSE_ARG + " arg value must be absolute path." )
		exit( constants.FAILURE )
	except InvalidFilePathError :
		print( PARSE_ARG + " arg is a file that does not exist." )
		print( "Please provide one that does." )
		exit( constants.FAILURE )

	try :
		check_input_arg( names_arg_result )
	except NotAbsPathError :
		print( NAMES_ARG + " arg value must be absolute path." )
		exit( constants.FAILURE )
	except InvalidFilePathError :
		print( NAMES_ARG + " arg is a file that does not exist." )
		print( "Please provide one that does." )
		exit( constants.FAILURE )

	try :
		check_output_arg( exttok_arg_result )
	except NotAbsPathError :
		print( EXTTOK_ARG + " arg value must be absolute path." )
		exit( constants.FAILURE )
	except InvalidFilePathError :
		print( EXTTOK_ARG + " arg is a file that exists." )
		print( "Please provide one that does not." )
		print( "We refuse to overwrite files." )
		exit( constants.FAILURE )

	try :
		check_output_arg( extlex_arg_result )
	except NotAbsPathError :
		print( EXTLEX_ARG + " arg value must be absolute path." )
		exit( constants.FAILURE )
	except InvalidFilePathError :
		print( EXTLEX_ARG + " arg is a file that exists." )
		print( "Please provide one that does not." )
		print( "We refuse to overwrite files." )
		exit( constants.FAILURE )

	try :
		check_output_arg( extparse_arg_result )
	except NotAbsPathError :
		print( EXTPARSE_ARG + " arg value must be absolute path." )
		exit( constants.FAILURE )
	except InvalidFilePathError :
		print( EXTPARSE_ARG + " arg is a file that exists." )
		print( "Please provide one that does not." )
		print( "We refuse to overwrite files." )
		exit( constants.FAILURE )

	generated_files = [ ]

	# Generate token extension
	try :
		generated_files.append( exttok_arg_result )
		gen_token_ext( token_arg_result , exttok_arg_result )
	except NoTokensError :
		print( EXTTOK_ARG + " arg contains no tokens." )
		for f_to_del in generated_files :
			os.unlink( f_to_del )
		exit( constants.FAILURE )

	# Generate lexer extension
	try :
		generated_files.append( extlex_arg_result )
		gen_lexer_ext( token_arg_result , lexer_arg_result , exttok_arg_result , names_arg_result , extlex_arg_result )
	except InvalidIDError as e :
		print( e.invalid_id + " is defined as an identifier and a keyword in " + names_arg_result + "!" )
		for f_to_del in generated_files :
			os.unlink( f_to_del )
		exit( constants.FAILURE )
	
	# Generate parser extension
	try :
		generated_files.append( extparse_arg_result )
		gen_parser_ext( token_arg_result , lexer_arg_result , parser_arg_result , exttok_arg_result , names_arg_result , extparse_arg_result )
	except NoStartSymbolError :
		print( "No start symbol specified in " + parser_arg_result + "!" )
		for f_to_del in generated_files :
			try :
				os.unlink( f_to_del )
			except FileNotFoundError :
				pass
		exit( constants.FAILURE )
	except MultipleStartSymbolError :
		print( "Multiple start symbols specified in " + parser_arg_result + "!" )
		for f_to_del in generated_files :
			try :
				os.unlink( f_to_del )
			except FileNotFoundError :
				pass
		exit( constants.FAILURE )
	except NoNonTermsError :
		print( "No nonterminals specified in " + parser_arg_result + "!" )
		for f_to_del in generated_files :
			try :
				os.unlink( f_to_del )
			except FileNotFoundError :
				pass
		exit( constants.FAILURE )
	except MultipleLexDefError :
		print( "Token defined multiple times in lexer spec " + lexer_arg_result + "!" )
		for f_to_del in generated_files :
			try :
				os.unlink( f_to_del )
			except FileNotFoundError :
				pass
		exit( constants.FAILURE )
	except MiscError :
		print( "You made a big mistake, Roman..." )
		for f_to_del in generated_files :
			try :
				os.unlink( f_to_del )
			except FileNotFoundError :
				pass
		exit( constants.FAILURE )

	exit( constants.SUCCESS )

########
# TODO #
########
# Make the directory as well for the full path
# Write a wrapper for argparse for both this and the other Python scripts
# Do M3 interface name check in check_output_arg
# Document regex exactly and what the program will take
# Do we need to be able to support multiline tokens? "%token\nDIGIT" or even "DIGIT\nLOLCATZ"?
# See comment in parse_names_file. Gotta make sure nothing else is in .t, .l, .names, etc. files.
# parse_list is a bit dangerous because both arguments are strings and are technically acceptable. Took me a lot of time to fix a bug where I permuted the args
# What if ID token name is the same as the underlying value of ID?
# Nothing can come before ID in the *.names file. Please check for this as well.
