#!/usr/bin/env python

###################
### genm3make.py ##
# by Roman Parise #
###################

###########
# Imports #
###########
import os
import os.path
import argparse
import shutil
import constants

#############
# Constants #
#############
# Program arguments
fmt_fn_arg = lambda x : "--" + x
INDIR = "indir"
OUTDIR = "outdir"
PROGNAME = "progname"

##############
# Exceptions #
##############
class NotAbsPathError( Exception ) :
	pass

class InvalidDirPathError( Exception ) :
	pass

class InvalidProgNameError( Exception ) :
	pass

class MiscDirError( Exception ) :
	pass

class OverwriteFileError( Exception ) :
	pass

#############
# Functions #
#############

### Argument parsing functions ###

# usage( ) - basic program usage; no arguments
def usage( ) :
	print( os.path.basename( __file__ ) )
	print( "=============" )
	print( "Note all file names MUST be absolute paths or an error will be thrown." )
	print( "Errors thrown for explicitly named input files that do not exist!" )
	print( "Errors thrown for explicitly named output files that already exist!" )
	print( "Mandatory Arguments: " )
	print( fmt_fn_arg( INDIR ) + " X :: take directory named X and generate output" )
	print( "Input directory can (but is not required to) contain any of the following..." )
	print( "- *.i3 files (Modula-3 interfaces)" )
	print( "- *.m3 files (Modula-3 modules/implementations of interfaces)" )
	print( "- *.t files (token specification for parser generator)" )
	print( "- *.l files (lexer specification for parser generator)" )
	print( "- *.y files (parser specification for parser generator)" )
	print( "- *.e files (extensions for parser generator)" )
	print( fmt_fn_arg( OUTDIR ) + " X :: output the resulting files in directory named X" )
	print( "The output directory contains all of the files in the input directory together with the following files..." )
	print( "- m3makefile - not guaranteed to build properly if your files have errors" )
	print( "Optional Arguments: " )
	print( fmt_fn_arg( PROGNAME ) + " X :: name of the program to be generated. Error thrown if Main.m3 not in directory" )
	print( "" )

# indir_check( indir : str ) - check the indir argument
# indir - input directory path (must be absolute directory path that exists)
# Raises NotAbsPathError and InvalidDirPathError
# Raises MiscDirError if directory contains other directories. This can cause
# ambiguity if those directories contain *.i3 files and such.
def indir_check( indir ) :
	assert isinstance( indir , str )
	# Must be an absolute path
	if not os.path.isabs( indir ) :
		raise NotAbsPathError
	# Must be a valid directory and not a file
	if not os.path.isdir( indir ) :
		raise InvalidDirPathError
	# Must not contain subdirectories
	if [ d for d in os.listdir( indir ) if os.path.isdir( os.path.join( indir , d ) ) ] != [ ] :
		raise MiscDirError

# outdir_check( outdir : str ) - check the outdir argument
# outdir - must be an absolute path that does not exist
# Raises NotAbsPathError and InvalidDirPathError
def outdir_check( outdir ) :
	assert isinstance( outdir , str )
	# Must be an absolute path
	if not os.path.isabs( outdir ) :
		raise NotAbsPathError
	# Must not currently exist
	if os.path.isdir( outdir ) :
		raise InvalidDirPathError

# progname_check( progname : str ) - check the program name provided
# progname - must be an executable name that does not exist in the path
#	     specified by outdir; this is the program name and will be
#	     located in the directory specified by outdir; do NOT provide
#	     a full path
# Raises InvalidProgNameError.
def progname_check( progname , outdir ) :
	assert isinstance( outdir , str )
	assert isinstance( progname , str )
	# Must not exist and must not be a file path
	if os.path.isfile( os.path.join( outdir , progname ) ) or progname.find( "/" ) != -1 :
		raise InvalidProgNameError

### Actual functions for code generation ###
# gen_out_dir - generate the output directory with m3makefile and other files
# - indir :: absolute path of input directory
# - outdir :: absolute path of output directory
# - progname :: optional ; name of the final program
def gen_out_dir( indir , outdir , progname = None ) :

	assert isinstance( indir , str )
	assert isinstance( outdir , str )
	assert isinstance( progname , str )

	# Divide up the directory based on file extensions
	all_files_with_ext = lambda ext : [ os.path.splitext( f )[ 0 ] for f in os.listdir( indir ) if os.path.splitext( f )[ 1 ] == ext ]
	interface_names = all_files_with_ext( ".i3" )
	implementation_names = all_files_with_ext( ".m3" )
	token_spec_names = all_files_with_ext( ".t" )
	lexer_spec_names = all_files_with_ext( ".l" )
	parser_spec_names = all_files_with_ext( ".y" )
	ext_spec_names = all_files_with_ext( ".e" )

	# Determine necessary libraries to import
	imports = [ "libm3" ]
	if token_spec_names != [ ] or lexer_spec_names != [ ] or parser_spec_names != [ ] or ext_spec_names != [ ] :
		imports.append( "parserlib" )

	# Generate strings
	import_strings = [ "import( \"" + lib + "\" )\n" for lib in imports ]
	interface_strings = [ "interface( \"" + intfc + "\" )\n" for intfc in interface_names ]
	implementation_strings = [ "implementation( \"" + impl + "\" )\n" for impl in implementation_names ]
	token_spec_strings = [ "token( \"" + tok + "\" )\n" for tok in token_spec_names ]
	lexer_spec_strings = [ "lexer( \"" + lex + "\" , \"" + lex + "\" )\n" for lex in lexer_spec_names ]
	parser_spec_strings = [ "parser( \"" + parser + "\" , \"" + parser + "\" )\n" for parser in parser_spec_names ]
	ext_spec_strings = [ "extended( \"" + ext + "\" )\n" for ext in ext_spec_names ]
	program_strings = [ ]
	if progname != None :
		program_strings.append( "program( \"" + progname + "\" )\n" )

	# Generate outdir
	os.makedirs( outdir )
	# Generate m3makefile in outdir
	with open( os.path.join( outdir , "m3makefile" ) , "w" ) as m3makefile_handle :

		m3makefile_handle.write( "% Generated by " + __file__ + "\n\n" )

		m3makefile_handle.write( "% Imports\n" )
		for impstr in import_strings :
			m3makefile_handle.write( impstr )

		m3makefile_handle.write( "\n% Interfaces\n" )
		for intfcstr in interface_strings :
			m3makefile_handle.write( intfcstr )

		m3makefile_handle.write( "\n% Implementations\n" )
		for implstr in implementation_strings :
			m3makefile_handle.write( implstr )

		m3makefile_handle.write( "\n% Token Specifications\n" )
		for tokstr in token_spec_strings :
			m3makefile_handle.write( tokstr )

		m3makefile_handle.write( "\n% Lexer Specifications\n" )
		for lexstr in lexer_spec_strings :
			m3makefile_handle.write( lexstr )

		m3makefile_handle.write( "\n% Parser Specifications\n" )
		for parsestr in parser_spec_strings :
			m3makefile_handle.write( parsestr )

		m3makefile_handle.write( "\n% Parser Extension Specifications\n" )
		for extstr in ext_spec_strings :
			m3makefile_handle.write( extstr )
	
		if program_strings != [ ] :
			m3makefile_handle.write( "\n% Program\n" )
			for progstr in program_strings :
				m3makefile_handle.write( progstr )

	# Copy over all the source files. Stop and delete all generated
	# files and directories if there's a name conflict.
	for f in os.listdir( indir ) :
		if os.path.isfile( os.path.join( outdir , f ) ) :
			shutil.rmtree( outdir , ignore_errors = True )
			raise OverwriteFileError( f )
		shutil.copy2( os.path.join( indir , f ) , outdir )

########
# Main #
########
if __name__ == "__main__" :
	my_arg_parser = argparse.ArgumentParser( usage = usage( ) )
	my_arg_parser.add_argument( fmt_fn_arg( INDIR ) , required = True , type = str )
	my_arg_parser.add_argument( fmt_fn_arg( OUTDIR ) , required = True , type = str )
	my_arg_parser.add_argument( fmt_fn_arg( PROGNAME ) , required = False , type = str )

	my_args = my_arg_parser.parse_args( )

	# INDIR
	indir_result = my_args.indir
	try :
		indir_check( indir_result )
	except NotAbsPathError :
		print( fmt_fn_arg( INDIR ) + " argument must be an absolute path. Given " + indir_result + "." )
		print( "Please provide an absolute path instead of the relative path provided." )
		print( "Relative paths change the behavior of the script depending on where the script is called." )
		exit( constants.FAILURE )
	except InvalidDirPathError :
		print( fmt_fn_arg( INDIR ) + " argument must be a valid directory path. Given " + indir_result + "." )
		print( "The directory may not exist. Please provide a path that exists." )
		exit( constants.FAILURE )
	except MiscDirError :
		print( fmt_fn_arg( INDIR ) + " argument must not contain subdirectories. Given " + indir_result + "." )
		exit( constants.FAILURE )

	# OUTDIR
	outdir_result = my_args.outdir
	try :
		outdir_check( outdir_result )
	except NotAbsPathError :
		print( fmt_fn_arg( OUTDIR ) + " argument must be an absolute path. Given " + outdir_result + "." )
		print( "Please provide an absolute path instead of the relative path provided." )
		print( "Relative paths change the behavior of the script depending on where the script is called." )
		exit( constants.FAILURE )
	except InvalidDirPathError :
		print( fmt_fn_arg( OUTDIR ) + " argument must be a directory that does not exist. Given " + outdir_result + "." )
		print( "The directory exists. Please provide a path that does NOT exist." )
		print( "We refuse to overwrite directories." )
		exit( constants.FAILURE )
	
	# PROGNAME
	progname_result = my_args.progname
	if progname_result :
		try :
			progname_check( progname_result , outdir_result )
		except InvalidProgNameError :
			print( fmt_fn_arg( PROGNAME ) + " argument is an invalid filename. Given " + progname_result + "." )
			print("Either a file named " + progname_result + " already exists in the directory specified by " + fmt_fn_arg( OUTDIR ))
			print( "or you provided a file path and not simply an executable file name." )
			exit( constants.FAILURE )
	
	# Generate the outdir
	try :
		gen_out_dir( indir_result , outdir_result , progname_result )
	except OverwriteFileError as e :
		print( fmt_fn_arg( INDIR ) + " argument is not valid. Given " + indir_result + "." )
		print( indir_result + " contains " + e.args[ 0 ] + ", which conflicts with a file of the same" )
		print( "name in " + outdir_result + ". This could be a file generated by the script. Please remove " )
		print( os.path.join( indir_result , e.args[ 0 ] ) + " from the directory and try again." )
	exit( constants.SUCCESS )
	

########
# TODO #
########
# Make it so that you can do my_args.variablename in case you want to change arg name
# What to do with files in the directory that do not have one of the valid file extensions?
# What if the user inputs a name of a program or a file with invalid unicode characters?
# What to do if program depends on other imports besides libm3 - extra arg? libm3 and parserlib included by default
# What if I want a directory within a directory to be generated?
# Make it so usage doesn't print every time I run
# Need a better soln. Right now we specify the same ParserExt and LexerExt in both Spec.m3 and the generated parser exts. They
# should both be generated from the same source.
# Can we allow subdirectories? Why not just copy all the directories as well?
# Subclass ArgumentParser and override error() and usage() functions with proper exit codes and not to print usage each time
