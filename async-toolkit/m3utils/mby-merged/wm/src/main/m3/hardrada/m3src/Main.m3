MODULE Main ;
IMPORT IO ;
IMPORT Node ;
IMPORT Spec ;
VAR
	root : REF Node.T := NIL ;
BEGIN
	TRY
		root := Spec.Parse( "add_test" ) ;
		Spec.DebugTree( root , "add_test_debug" ) ;
	EXCEPT
		| Spec.InvalidFname => IO.Put( "File named add_test doesn't exist.\n" ) ;
		| Spec.OutError => IO.Put( "Could not write to output file.\n" ) ;
	END ;
	(* TODO Learn how to properly exit the program *)
END Main.
