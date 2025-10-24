(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

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
IMPORT Thread;

CONST
  TwoToThe32 = FLOAT(Word.Shift(1, 32), LONGREAL);

EXCEPTION
  Error (TEXT);

(* the following two are the basic I/O commands *)
PROCEDURE PutCommandG(wr : Wr.T; cmd : TEXT)
  RAISES { Thread.Alerted, Wr.Failure } ;

PROCEDURE GetResponseG(rd : Rd.T; matchKw : TEXT) : TextReader.T  RAISES { Thread.Alerted, Rd.Failure } ;

PROCEDURE ReadCompressedNodeDataG(rd         : Rd.T;
                                  VAR nodeid : CARDINAL;
                                  VAR norm   : SpiceCompress.Norm) : TEXT  RAISES { Thread.Alerted, Rd.Failure, Error } ;
  (* this is the counterpart to DistZTrace.WriteOut

     The result is the compressed bytes with the code byte prepended.

     This differs from the tempfile format in that the tempfile has the 
     norm prepended.
  *)
  
PROCEDURE ReadBinaryNodeDataG(rd         : Rd.T;
                              VAR nodeid : CARDINAL;
                              VAR buff   : ARRAY OF LONGREAL)  RAISES { Thread.Alerted, Rd.Failure, Error } ;

PROCEDURE ReadInterpolatedBinaryNodeDataG(rd          : Rd.T;
                                          VAR nodeid  : CARDINAL;
                                          VAR buff    : ARRAY OF LONGREAL;
                                          interpolate : LONGREAL;
                                          unit        : LONGREAL)  RAISES { Thread.Alerted, Rd.Failure, Error } ;

  (* given a byte stream from an instance of nansimrd.cpp in rd,
     read the results of an 'x' command (EXTENDED MODE) and interpolate said
     data into the buffer buff 
     
     here interpolate is the interpolation interval desired and unit is the
     timestep of the FSDB file (found by other methods).
  *)
  

PROCEDURE GetLineUntilG(rd : Rd.T; term : TEXT; VAR line : TEXT) : BOOLEAN  RAISES { Thread.Alerted, Rd.Failure } ;

END FsdbComms.
