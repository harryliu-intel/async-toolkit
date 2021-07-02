INTERFACE UnsafeReader;
IMPORT Rd;
IMPORT Thread;

PROCEDURE ReadI(rd : Rd.T) : INTEGER
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } ;

PROCEDURE ReadLRA(rd : Rd.T; VAR q : ARRAY OF LONGREAL)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };
  (* if we get Thread.Alerted here, the file needs to be rewound, as we will
     have lost our place *)

END UnsafeReader.
