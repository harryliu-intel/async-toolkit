#!/usr/bin/env python

import sys
import argparse
import os
import os.path
import common

SRC_FILE_ARG = "--src"
FN_NAME_TO_PE_ARG = "--fn2pe"
RESIDUAL_ARG = "--residual_name"
OUTFNAME_ARG = "--outfile"
NUMBER_OF_ARGS_ARG = "--numofargs"
STATIC_ARG = "--static"

def usage( ) :
	print ( "*****************************" )
	print ( "*** pe - Partial Evaluator **" )
	print ( "*****************************" )
	print ( "*****************************" )
	print ( "*** " + SRC_FILE_ARG + " - src file name ***" )
	print ( "*****************************" )
	print ( "**** " + FN_NAME_TO_PE_ARG + " - top-level ****" )
	print ( "*** procedure to partially **" )
	print ( "*** evaluate.              **" )
	print ( "*****************************" )
	print ( "** " + RESIDUAL_ARG + " - residual **" )
	print ( "** procedure name          **" )
	print ( "*****************************" )
	print ( "***** " + OUTFNAME_ARG + " - output   ****" )
	print ( "** file name (including    **" )
	print ( "**        *.scm extension) **" )
	print ( "*****************************" )
	print ( "** " + NUMBER_OF_ARGS_ARG + " - number of **" )
	print ( "** arguments to function   **" )
	print ( "** to be partially         **" )
	print ( "** evaluated               **" )
	print ( "*****************************" )
	print ( "** " + STATIC_ARG + " (optional) -   **" )
	print ( "** a list of args ( first  **" )
	print ( "** arg is index of arg     **" )
	print ( "** to PE and the second is **" )
	print ( "** a string with the       **" )
	print ( "** Scheme code to replace  **" )
	print ( "** that argument ; you can **" )
	print ( "** keep repeating these    **" )
	print ( "*****************************" )
	print ( "** Do not partially       ***" )
	print ( "** evaluate the top-level ***" )
	print ( "** procedure              ***" )
	print ( "*****************************" )
	print ( "*** Note: Uses the scm    ***" )
	print ( "*** partial evaluator     ***" )
	print ( "*** from Aubrey Jaffer    ***" )
	print ( "*****************************" )

# src - str ; absolute path to src file
# fn2pe - str ; name of function to partially evaluate
# outfile - str ; absolute path to output file with partially evaluated function
# static - dictionary ( int -> str ) ; maps static argument index starting from 0
#	   to Scheme source code to replace that argument
# num_of_args - int ; total number of arguments to function to be PE'd
def pe( src , fn2pe , residual_name , outfile , static , num_of_args ) :
	# TODO Typechecking
	# TODO Abs path checking and ensure you dont overwrite files checking

	# Load Similix files via abs path
	command_to_exec = "( load \"" + os.path.join( common.similix_root , "system/sim-scm.scm" ) + "\" ) ; ( load \"" + os.path.join( common.similix_root , "system/sim-chez.so" ) + "\" )\n"
	print ( "Start string: " + command_to_exec )
	# Construct Similix PE string
	args_str = ""
	for arg_index in range( 0 , num_of_args ) :
		try :
			static[ arg_index ]
			args_str += " " + static[ arg_index ]
		except KeyError :
			args_str += " '***"
	command_to_exec += "( similix '" + fn2pe + " ( list " + args_str + " ) \"" + src + "\" )\n"
	# Write the outfile file
	command_to_exec += "(define outfile (open-output-file \"" + outfile + "\"))\n"
	args_list = ""
	for arg_index in range( 0 , num_of_args - len( static.keys( ) ) ) :
		args_list += "x" + str( arg_index ) + " "
	command_to_exec += "( display \"( define ( " + residual_name + " " + args_list + " ) ( let ( )\n\" outfile )\n"
	command_to_exec += "( display ( car ( residual-program ) ) outfile )\n"
	command_to_exec += "( display \"\n( " + fn2pe + "-0 " + args_list + " )\n\" outfile )\n"
	command_to_exec += "( display \") )\" outfile )\n"
	command_to_exec += "(close-output-port outfile)\n"
	command_to_exec += "(exit)\n"
	# Write command to a temp file
	# TODO Check to make sure you aren't overwriting anything
	temp_fname = common.uniquify_name( "temp" )
	with open( temp_fname , "w" ) as tempfile_handle :
		print ( "=== Scheme Code to be Executed ===" )
		print ( command_to_exec )
		print ( "=== Scheme Code to be Executed ===" )
		tempfile_handle.write( command_to_exec )
	os.system( common.scheme + " " + temp_fname )
	os.system( "rm -rf " + temp_fname )
	os.system( "rm -rf _simtmp0.scm" )

# src_dir - str ; absolute path to dir with all the src files
# fn2pe_struct - dictionary ;
# 	{ str of fname -> { str of function name to pe -> [ { "num_of_args" -> int ; number of arguments ,
#		 					    "static" -> static dictionary type used in pe ,
#							    "outfname" -> output file name
#							    "residual_name" -> residual procedure name } ] } }
# file_w_all_defs - str ; file name in out_dir with all the residual functions
# out_dir - str ; absolute path to dir with file_w_all_defs and each of the individual residual
#	    function files (must currently exist)
def pe_collection( src_dir , fn2pe_struct , file_w_all_defs , out_dir ) :
	# TODO Typechecking
	# TODO Abs path checking and ensure you dont overwrite files checking
	# TODO Ensure unique outfile names
	# Got loop from https://stackoverflow.com/questions/2186525/use-a-glob-to-find-files-recursively-in-python
	for root, dirnames, filenames in os.walk( src_dir ) :
		for filename in filenames :
			src = os.path.join( root , filename )
			try :
				fns_to_be_PEd = fn2pe_struct[ src ]
				for fn2pe in fns_to_be_PEd.keys( ) :
					for pe_instance in fns_to_be_PEd[ fn2pe ] :
						static = pe_instance[ "static" ]
						outfile = os.path.join( out_dir , pe_instance[ "outfname" ] )
						num_of_args = pe_instance[ "num_of_args" ]
						residual_name = pe_instance[ "residual_name" ]
						pe( src , fn2pe , residual_name , outfile , static , num_of_args )
			except KeyError :
				pass
	file_w_all_defs_src = ""
	for root , dirnames , filenames in os.walk( out_dir ) :
		for filename in filenames :
			dest = os.path.join( root , filename )
			# TODO Don't you get this from hda_directives?
			if dest != os.path.join( root , "pe_top.scm" ) :
#				file_w_all_defs_src += os.popen( "cat " + dest ).read( ) + "\n"
				with open( dest , "r" ) as dest_handle :
					file_w_all_defs_src += dest_handle.read( ) + "\n"
	with open( file_w_all_defs , "w" ) as file_w_all_defs_handle :
		file_w_all_defs_handle.write( file_w_all_defs_src )

if __name__ == "__main__" :
	parser = argparse.ArgumentParser( usage = usage( ) )
	parser.add_argument( SRC_FILE_ARG , required = True , type = str )
	parser.add_argument( FN_NAME_TO_PE_ARG , required = True , type = str )
	parser.add_argument( RESIDUAL_ARG , required = True , type = str )
	parser.add_argument( OUTFNAME_ARG , required = True , type = str )
	parser.add_argument( STATIC_ARG , nargs = '*' , required = False , type = str )
	parser.add_argument( NUMBER_OF_ARGS_ARG , required = True , type = int )
	args = parser.parse_args( )
	# TODO Check for abs paths
	src = args.src
	fn2pe = args.fn2pe
	outfile = args.outfile
	static = args.static
	residual_name = args.residual_name
	num_of_args = args.numofargs
	if static != None and len( static ) % 2 != 0 :
		print ( "Length of list of args after " + STATIC_ARG + " must be even." )
		sys.exit( common.FAILURE )
	static_arg_indices = [ ]
	strings_to_pe = [ ]
	if static != None :
		for static_index in range( 0 , len( static ) ) :
			# Get index of each static arg
			if static_index % 2 == 0 :
				try :
					static_arg_indices.append( int( static[ static_index ] ) )
				except ValueError :
					print ( "Every other element of the list after " + STATIC_ARG + " must be an integer starting with the 0th element (0th, 2nd, 4th, etc.)" )
					sys.exit( common.FAILURE )
			# Get string to PE
			else :
				strings_to_pe.append( static[ static_index ] )
		pe( src , fn2pe , residual_name , outfile , dict( zip( static_arg_indices , strings_to_pe ) ) , num_of_args )
	else :
		pe( src , fn2pe , residual_name , outfile , { } , num_of_args )
	sys.exit( common.SUCCESS )
