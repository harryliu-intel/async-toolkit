#!/usr/bin/env python

import common
import os.path
import sc
import pe
import imp
import argparse

def pe_and_compile( final_rhd_src_fname ) :
	# TODO Typechecking
	# Append loop for all pe
	# TODO Need some error checking to make sure you don't overwrite anything
	final_rhd_src_suffix = "pe.pe_collection( \"" + hda_paths.hw_lib + "\" , all_src , \"" + os.path.join( hda_paths.parts , hda_paths.pe_top_fname ) + "\" , \"" + hda_paths.parts + "\" )\n"
	# Append loop for all compiling
	# final_rhd_src_suffix += "sc.sc_collection( all_files_to_compile )\n"
	# final_rhd_src = "all_files_to_compile = [ ]\n"
	final_rhd_src = "all_src = { }\n"
	final_rhd_src += "pe_outfnames = [ ]\n"
	# Run final_rhd_src
	with open( final_rhd_src_fname , "r" ) as final_rhd_src_handle :
		final_rhd_src += final_rhd_src_handle.read( ) + final_rhd_src_suffix
		print ( "=== Final Code ===" )
		print ( final_rhd_src )
		print ( "=== Final Code ===" )
		exec final_rhd_src

def gen_final_scm_src( hda_directives , final_rhd_src_fname , dirname ) :
	# TODO Typechecking
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

def perform_hda_directives_actions( hda_directives , dirname ) :
	# TODO Typechecking
	# Name final Python output file
	final_rhd_src_fname = common.uniquify_name( os.path.join( dirname , "temp" ) )
	final_scm_src_fname = gen_final_scm_src( hda_directives , final_rhd_src_fname , dirname )
	# Compile and run. Be sure to get a unique name for the executable.
	# final_scm_src_exec_fname = common.uniquify_name( os.path.join( dirname , "temp" ) , ".image" )
	# sc.sc( final_scm_src_fname , "toplevel" , final_scm_src_exec_fname )
	# TODO Ensure that you've sourced the hda.env file
	os.system( common.scheme + " " + final_scm_src_fname )
	pe_and_compile( final_rhd_src_fname )

def rhd( hda_paths_path ) :
	# TODO Typechecking and error checking
	# TODO Check for absolute path
	global hda_paths
	hda_paths = imp.load_source( "hda_paths" , hda_paths_path )
	# Generate a uniquely-named directory
	dirname = common.uniquify_name( "temp" )
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
