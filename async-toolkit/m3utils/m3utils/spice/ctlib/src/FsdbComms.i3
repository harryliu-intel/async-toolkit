INTERFACE FsdbComms;

(* 
   these are the routines that communicate with an external nanosimrd program 

   Since nanosimrd is called out to by the trace converter, it is perfectly
   OK to run nanosim through netbatch, using a simple shell script.
*)

IMPORT Rd;
IMPORT Wr;
IMPORT TextReader;
IMPORT SpiceCompress; (* just for the Norm *)
IMPORT Word;

CONST
  TwoToThe32 = FLOAT(Word.Shift(1, 32), LONGREAL);

(* the following two are the basic I/O commands *)
PROCEDURE PutCommandG(wr : Wr.T; cmd : TEXT);

PROCEDURE GetResponseG(rd : Rd.T; matchKw : TEXT) : TextReader.T;

PROCEDURE ReadCompressedNodeDataG(rd         : Rd.T;
                                  VAR nodeid : CARDINAL;
                                  VAR norm   : SpiceCompress.Norm) : TEXT;

PROCEDURE ReadBinaryNodeDataG(rd         : Rd.T;
                              VAR nodeid : CARDINAL;
                              VAR buff   : ARRAY OF LONGREAL);

PROCEDURE ReadInterpolatedBinaryNodeDataG(rd          : Rd.T;
                                          VAR nodeid  : CARDINAL;
                                          VAR buff    : ARRAY OF LONGREAL;
                                          interpolate : LONGREAL;
                                          unit        : LONGREAL);

  (* given a byte stream from an instance of nansimrd.cpp in rd,
     read the results of an 'x' command (EXTENDED MODE) and interpolate said
     data into the buffer buff 
     
     here interpolate is the interpolation interval desired and unit is the
     timestep of the FSDB file (found by other methods).
  *)
  

PROCEDURE GetLineUntilG(rd : Rd.T; term : TEXT; VAR line : TEXT) : BOOLEAN;

END FsdbComms.
