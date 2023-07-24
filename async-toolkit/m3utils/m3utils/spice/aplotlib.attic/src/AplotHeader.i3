INTERFACE AplotHeader;
IMPORT Word;
IMPORT Wr, Rd;

(* 
   definition of header format for aplot file

   this definition must be backward-compatible with Andrew's old format
   definition

   Author: Mika Nystrom <mika.nystroem@intel.com>
   August 2021
*)

TYPE
  T = RECORD
    format    : Format;
    timestamp : Timestamp;
    numnodes  : Nodecount;
    parameter : Parameter; (* this is used for a subset of formats *)
    multiFile : CARDINAL;   (* nodes are striped across multiFile files in 
                               directory; if 0, all in a single file *)
  END;

    (* if multiFile # 0, then
       
       <path>/header 

       contains header

       and there are files named 00000000 .. multiFile-1 containing
       data

       nodes are sequential in files and arranged so that the first
       files 0 .. multiFile-2 have the same number of nodes, and
       multiFile-1 contains the maximum possible nodes not exceeding
       multiFile-1

       the number of nodes in the first files is

       numnodes DIV multiFile

       the number of nodes in the last file is

       numnodes - multiFile * (numnodes DIV multiFile)

    *)

  Uint32 = BITS 32 FOR [ 0 .. TwoToThe32Minus1 ];
  Uint64 = Word.T;;
  
  Timestamp = Uint32;
  Nodecount = Uint64;
  Parameter = Uint32;

  (* check the names here so I don't have them backwards *)

  (* floating formats:

     LE32 : little-endian IEEE754 32-bit format a.k.a. REAL (float)
     LE64 : little-endian IEEE754 64-bit format a.k.a. LONGREAL (double)
     LE80 : little-endian IEEE754 80-bit format a.k.a. EXTENDED (long double)
     LEVar: little-endian IEEE754 fix-bit format a.k.a. <the French thing>
  *)

  (* note that a particular implementation of the library may not support
     all formats.

     fully backward-compatible libraries MUST support the following:
     NodeMajor32, StepMajor32, Reordering
  *)
  
  Format = {
  NodeMajor32, (* packed format, LE32, sequential write, node-major *)
  StepMajor32, (* packed format, LE32, block node read, step-major *)
  Reordering,  (* file is being modified *)
  Reserved,    (* unused *)
  NodeMajor64, (* packed format, LE64, sequential write, node-major *)
  StepMajor64, (* packed format, LE64, block node read, step-major *)
  NodeMajor80, (* packed format, LE80, sequential write, node-major *)
  StepMajor80, (* packed format, LE80, block node read, step-major *)
  NodeMajorFix,(* packed format, LEFix, sequential write, node-major *)
  StepMajorFix,(* packed format, LEFix, block node read, step-major *)
  Fixed,       (* all nodes in consistent-width fixed-point format *)
  FixedVar     (* all nodes in individual-width fixed-point format *)
  };

  (* the following formats take an extra parameter:

     Fixed   <length of samples in bytes>
     NodeMajorFix, StepMajorFix <width of floating-point numbers in bits>
  *)

PROCEDURE TakesParameter(format : Format) : BOOLEAN;
  (* implementation of above comment *)
  
PROCEDURE UpdateTimestamp(VAR header : T);
  (* set the timestamp to NOW *)

CONST Brand = "AplotHeader";

CONST TwoToThe32Minus1 = Word.Shift(1, 32) - 1;
  (* this declaration won't work on a 32-bit machine *)

PROCEDURE Write(wr : Wr.T; header : T) RAISES { Wr.Failure };
  (* serialize header onto a Writer *)

PROCEDURE Read(rd : Rd.T) : T RAISES { Rd.Failure };
  (* deserialize from Reader *)
  
END AplotHeader.
 
