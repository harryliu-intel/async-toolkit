( define ( concat_list args_list )
	( if ( null? args_list ) 
		""
		( string-append ( car args_list ) "\n" ( concat_list ( cdr args_list ) ) )
	)
)

; src - path to source file relative to parts
; topproc - top-level procedure in source file
; outfname - path to output file relative to build
; ( compile src topproc outfname )
; ( define ( compile src topproc outfname )
; 	( string-append "all_files_to_compile.append( { \"src\" : os.path.join ( hda_paths.parts , \"" src "\" ) , \"topproc\" : \"" topproc "\" , \"outfname\" : os.path.join ( hda_paths.build , \"" outfname "\" ) } )\n"
; 	)
; )

; src - path to source file relative to hw_lib directory
; ( pe src outfname ( list of fn2pe's ) )
( define ( pe src list_of_fn2pes )
	( string-append "all_fn2pe_dict = { }\n"
	( concat_list list_of_fn2pes )
	"all_src[ os.path.join ( hda_paths.hw_lib , \"" src "\" ) ] = all_fn2pe_dict\n"
	)
)

; fn2pe_name_str - name of the function to be partially evaluated
; residual_name_str - name of the residual function
; num_of_args - total number of arguments to the function
; outfname - output residual file name relative to parts/ directory; do NOT
; include the extension; it is assumed to be .spec.scm. That way, clean knows which
; targets to delete.
; ( fn2pe fn2pe_name_str residual_name_str num_of_args ( list of static_arg's ) )
( define ( fn2pe fn2pe_name_str residual_name_str num_of_args outfname list_of_static_args )
	( string-append "fn2pe_dict = { }\n"
	"fn2pe_dict[ \"residual_name\" ] = \"" residual_name_str "\"\n"
	"fn2pe_dict[ \"num_of_args\" ] = " ( number->string num_of_args ) "\nstatic = { }\n"
	( concat_list list_of_static_args )
	"fn2pe_dict[ \"static\" ] = static\n"
	"fn2pe_dict[ \"outfname\" ] = \"" outfname ".spec.scm\"\n"
	"try :\n"
	"    all_fn2pe_dict[ \"" fn2pe_name_str "\" ].append( fn2pe_dict )\n"
	"except KeyError :\n"
	"    all_fn2pe_dict[ \"" fn2pe_name_str "\" ] = [ fn2pe_dict ]\n" )
)

; argnum - index of the argument
; code - value of the argument (str); Scheme code snippet
; ( static_arg argnum code )
( define ( static_arg argnum code )
	( string-append "static[ " ( number->string argnum ) " ] = \"" code "\"" )
)
