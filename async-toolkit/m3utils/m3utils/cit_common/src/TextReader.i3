(* $Id$ *)
(* "strtok"
   a TextReader.T is initialized with

   txtRd := NEW(TextReader.T).init(string);

   Tokens may be parsed out by passing in a delimiter,
   as follows:

   VAR txt : TEXT; BEGIN
     WHILE txtRd.next(" ,.", txt) DO
       (* parse txt *)
     END
   END

   To get the rest of the line, pass "" as the delims.
   It is a checked run-time error to pass NIL as the delims or as line.
 *)

INTERFACE TextReader;
IMPORT TextList, Rd;
IMPORT Thread;

EXCEPTION
  NoMore;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    (* all the methods of a TextReader.T leave the reader in a state
       to parse further untouched tokens *)
    (* get next word before delims from reader.  If skipNulls is true, 
       zero-length strings are never returned.  Return value is TRUE 
       if call succeeded.  If nothing was left, call fails, and return
       is FALSE *)
    next(delims : TEXT; VAR chunk : TEXT; skipNulls := FALSE) : BOOLEAN;

    (* untested as yet *)
    nextS(delims : SET OF CHAR; 
          VAR chunk : TEXT; skipNulls := FALSE) : BOOLEAN;

    (* same as next, except failure is signalled thru an exception *)
    nextE(delims : TEXT; skipNulls := FALSE) : TEXT RAISES { NoMore };

    (* initialize a new TextReader.T *)
    init(line : TEXT) : T;

    (* initialize from an Rd.T.  rd must eventually end (to allow in-memory
       implementations) *)
    initFromRd(rd : Rd.T) : T RAISES { Rd.Failure, Thread.Alerted };

    (* probe a TextReader.T *)
    isEmpty() : BOOLEAN;

    (* tokenize a line into Text tokens until EOT or an endDelim *)
    (* it is a checked runtime error for there to be an overlap between
       listDelims and endDelims *)
    shatter(listDelims : TEXT; 
            endDelims : TEXT; skipNulls := FALSE) : TextList.T;
  END;
END TextReader.
    
