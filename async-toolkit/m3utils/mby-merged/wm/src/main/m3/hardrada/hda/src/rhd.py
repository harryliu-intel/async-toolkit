#!/usr/bin/env python

import common
import os.path
import pe
import imp
import argparse
import sys

# pe_and_compile
# Generate the remaining Python code and run the file to perform partial evaluation.
# Does not return a value.
def pe_and_compile( final_rhd_src_fname ) :
	assert isinstance( final_rhd_src_fname , str )
	if not os.path.isabs( final_rhd_src_fname ) :
		print ( "Please provide absolute path for final rhd source filename." )
		sys.exit( common.FAILURE )
	if not os.path.isfile( final_rhd_src_fname ) :
		print ( "Final rhd source file does not exist." )
		sys.exit( common.FAILURE )
	# Append loop for all pe
	final_rhd_src_suffix = "pe.pe_collection( \"" + hda_paths.hw_lib + "\" , all_src , \"" + os.path.join( hda_paths.parts , hda_paths.pe_top_fname ) + "\" , \"" + hda_paths.parts + "\" )\n"
	# Append loop for all compiling
	final_rhd_src = "all_src = { }\n"
	final_rhd_src += "pe_outfnames = [ ]\n"
	# Run final_rhd_src
	with open( final_rhd_src_fname , "r" ) as final_rhd_src_handle :
		final_rhd_src += final_rhd_src_handle.read( ) + final_rhd_src_suffix
		try :
			exec final_rhd_src
		except :
			print ( "Something went wrong with hda_directives file." )
			sys.exit( common.FAILURE )

# gen_final_scm_src
# Generate some definitions for the Scheme code in hda_directives.
# Return the final Scheme source file name.
def gen_final_scm_src( hda_directives , final_rhd_src_fname , dirname ) :

	assert isinstance( hda_directives , str )
	assert isinstance( final_rhd_src_fname , str )
	assert isinstance( dirname , str )

	if not os.path.isabs( hda_directives ) :
		print ( "Please provide absolute path for hda_directives file name." )
		sys.exit( common.FAILURE )
	if not os.path.isfile( hda_directives ) :
		print ( "hda_directives file does not exist." )
		sys.exit( common.FAILURE )

	if not os.path.isabs( final_rhd_src_fname ) :
		print ( "Please provide absolute path for final scm source filename." )
		sys.exit( common.FAILURE )

	if not os.path.isabs( dirname ) :
		print ( "Please provide absolute path for directory with final scm source." )
		sys.exit( common.FAILURE )
	if not os.path.isdir( dirname ) :
		print ( "Final scm source file's directory does not exist." )
		sys.exit( common.FAILURE )

	final_scm_src = ""
	with open( common.hda_root + "/src/rhd.scm" , "r" ) as rhd_scm_handle :
		# First, wrap hda_directives in a call that appends all the strings and
		# prints to a file.
		final_scm_src += rhd_scm_handle.read( )
		final_scm_src += "( define ( toplevel x ) ( let ( ( outfile ( open-output-file \"" + final_rhd_src_fname + "\" ) ) ) "
		final_scm_src += "( display ( string-append\n "
		# Secondly, prepend hda_directives with file contents of rhd.scm
		with open( hda_directives , "r" ) as hda_directives_handle :
			final_scm_src += hda_directives_handle.read( )
		final_scm_src += "\n) outfile )\n"
		final_scm_src += "( close-output-port outfile )\n"
		final_scm_src += ") )\n"
		final_scm_src += "( toplevel 1 )\n"
		final_scm_src += "( exit )\n"
	# Write to a file. Be sure to get a unique name for it.
	final_scm_src_fname = common.uniquify_name( os.path.join( dirname , "temp" ) , ".scm" )
	with open( final_scm_src_fname , "w" ) as final_scm_src_handle :
		final_scm_src_handle.write( final_scm_src )
	return final_scm_src_fname

# perform_hda_directives_actions
# Generate Python code from hda_directives and execute.
# Place temporary collateral in the dirname directory.
def perform_hda_directives_actions( hda_directives , dirname ) :

	assert isinstance( hda_directives , str )
	assert isinstance( dirname , str )

	if not os.path.isabs( hda_directives ) :
		print ( "Please provide absolute path for hda_directives file name." )
		sys.exit( common.FAILURE )
	if not os.path.isfile( hda_directives ) :
		print ( "hda_directives file does not exist." )
		sys.exit( common.FAILURE )

	if not os.path.isabs( dirname ) :
		print ( "Please provide absolute path for temp directory for hda_directives collateral." )
		sys.exit( common.FAILURE )
	if not os.path.isfile( hda_directives ) :
		print ( "temp directory for hda_directives collateral does not exist." )
		sys.exit( common.FAILURE )

	# Name final Python output file and run
	final_rhd_src_fname = common.uniquify_name( os.path.join( dirname , "temp" ) )
	final_scm_src_fname = gen_final_scm_src( hda_directives , final_rhd_src_fname , dirname )
	os.system( common.scheme + " " + final_scm_src_fname )
	pe_and_compile( final_rhd_src_fname )

# rhd - run hda_directives
# Run hda_directives and perform partial evaluation. Generates collateral in a temporary
# directory. This directory is deleted afterward.
def rhd( hda_paths_path ) :

	assert isinstance( hda_paths_path , str )
	if not os.path.isabs( hda_paths_path ) :
		print ( "Please provide absolute path for hda_paths file." )
		sys.exit( common.FAILURE )
	if not os.path.isfile( hda_paths_path ) :
		print ( "hda_paths file does not exist." )
		sys.exit( common.FAILURE )

	global hda_paths
	hda_paths = imp.load_source( "hda_paths" , hda_paths_path )
	# Generate a uniquely-named directory
	dirname = os.path.join( os.path.dirname( hda_paths_path ) , common.uniquify_name( "temp" ) )
	os.system( "mkdir -p " + dirname )
	perform_hda_directives_actions( hda_paths.hda_directives , dirname )
	os.system( "rm -rf " + dirname )

HDA_PATHS_ARG = "--hda_paths"

def usage( ) :
	print ( " === rhd === " )
	print ( "rhd stands for \"run hda_directives\"." )
	print ( "rhd partially evaluates and compiles based on" )
	print ( "information specified in hda_directives." )
	print ( "Only one mandatory argument: " )
	print ( HDA_PATHS_ARG + " :: absolute path to .hda_paths file" )

if __name__ == "__main__" :
	parser = argparse.ArgumentParser( usage = usage( ) )
	parser.add_argument( HDA_PATHS_ARG , required = True , type = str )
	args = parser.parse_args( )
	rhd( args.hda_paths )
