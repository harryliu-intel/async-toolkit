#!/usr/bin/env python

import sys
import argparse
import os
import common

SRC_FILE_ARG = "--src"
TOP_LVL_PROC_NAME_ARG = "--topproc"
OUTPUT_FNAME_ARG = "--outfname"

def usage( ) :
	print ( "***************************" )
	print ( "*** sc - Scheme Compiler **" )
	print ( "***************************" )
	print ( "***************************" )
	print ( "** " + SRC_FILE_ARG + " - src file name **" )
	print ( "** (including *.scm      **" )
	print ( "** extension)            **" )
	print ( "***************************" )
	print ( "** " + TOP_LVL_PROC_NAME_ARG + " - top-level **" )
	print ( "** interface procedure   **" )
	print ( "** name                  **" )
	print ( "***************************" )
	print ( "** " + OUTPUT_FNAME_ARG + " - output   **" )
	print ( "** file name             **" )
	print ( "** (including *.image    **" )
	print ( "**  extension )          **" )
	print ( "***************************" )

# src - absolute path to source file
# topproc - top-level procedure in source file
# outfname - absolute path to output file
def sc( src , topproc , outfname ) :
	# TODO Typechecking
	# TODO Abs path checking and ensure you dont overwrite files checking
	with open( src , "r" ) as prefix_src :
		output_file_src = prefix_src.read( ) + "\n,build " + topproc + " " + outfname + "\n,exit"
		temp_fname = common.uniquify_name( "temp" )
		with open( temp_fname , "w" ) as tempfile_handle :
			tempfile_handle.write( output_file_src )
		os.system( "cat " + temp_fname + " | " + common.scheme48 )
		os.system( "rm -rf " + temp_fname )

def sc_collection( all_files_to_compile ) :
	# TODO Typechecking
	# TODO Abs path checking and ensure you dont overwrite files checking
	for file_to_compile in all_files_to_compile :
		sc( file_to_compile[ "src" ] , file_to_compile[ "topproc" ] , file_to_compile[ "outfname" ] )

if __name__ == "__main__" :
	parser = argparse.ArgumentParser( usage = usage( ) )
	parser.add_argument( SRC_FILE_ARG , required = True , type = str )
	parser.add_argument( TOP_LVL_PROC_NAME_ARG , required = True , type = str )
	parser.add_argument( OUTPUT_FNAME_ARG , required = True , type = str )
	args = parser.parse_args( )
	sc( args.src , args.topproc , args.outfname )
