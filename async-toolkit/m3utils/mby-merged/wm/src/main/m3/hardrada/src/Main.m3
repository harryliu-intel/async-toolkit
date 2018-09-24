MODULE Main ;
IMPORT RDP ;
IMPORT IO ;
IMPORT ParseQueue ;
IMPORT FileRd ;
IMPORT Rd ;
VAR
	q := NEW( REF ParseQueue.T ) ;
	read_handle : Rd.T ;
BEGIN
	IO.Put( "Hello world!\n" ) ;
	(* Queue test *)
	ParseQueue.Push( q , 1 ) ;
	<* ASSERT ParseQueue.Peek( q ) = 1 *>
	ParseQueue.Push( q ) ;
	<* ASSERT ParseQueue.Peek( q ) = 0 *>
	ParseQueue.Inc( q , 2 ) ;
	<* ASSERT ParseQueue.Peek( q ) = 2 *>
	ParseQueue.Pop( q ) ;
	<* ASSERT ParseQueue.Peek( q ) = 1 *>
	IO.Put( "Passed the queue test...\n" ) ;
	(* Parser test *)
	read_handle := FileRd.Open( "./test" ) ;
	IF RDP.Parse( read_handle ) THEN
		IO.Put( "Parsed properly!\n" ) ;
	ELSE
		IO.Put( "Parsed improperly!\n" ) ;
	END ;
END Main .
