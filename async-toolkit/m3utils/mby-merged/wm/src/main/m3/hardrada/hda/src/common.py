import os
import os.path
import sys

SUCCESS = 0
FAILURE = -1

try :
	hda_root = os.environ[ "HDA_ROOT" ]
	scheme = os.path.join( os.environ[ "SCM_ROOT" ] , "bin/scm" )
	similix_root = os.environ[ "SIMILIX_ROOT" ]
except KeyError :
	print ( "Please source hda.env." )
	sys.exit( FAILURE )

def uniquify_name( name , ext = "" ) :
	# TODO Typechecking
	temp_cnt = 0
	while os.path.exists( name + str( temp_cnt ) + ext ) :
		temp_cnt += 1
	temp_fname = name + str( temp_cnt ) + ext
	return temp_fname
