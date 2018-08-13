MODULE Main ;
IMPORT TcamTest ;
IMPORT WCMTest ;
IMPORT IO ;
BEGIN
	IO.Put( "*****************\n" ) ;
	IO.Put( "Test Suite - TCAM\n" ) ;
	IO.Put( "*****************\n" ) ;
	TcamTest.FullTest( ) ;

	IO.Put( "****************\n" ) ;
	IO.Put( "Test Suite - WCM\n" ) ;
	IO.Put( "****************\n" ) ;
	WCMTest.FullTest( ) ;

END Main.
