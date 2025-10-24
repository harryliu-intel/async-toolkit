(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

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
