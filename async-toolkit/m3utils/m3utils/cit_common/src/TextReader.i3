(* $Id$ *)
INTERFACE TextReader;
IMPORT TextList, Rd;
IMPORT Thread;

(* Think ``strtok''. A "TextReader.T" is initialized with 
|  txtRd := NEW(TextReader.T).init(string);

   Tokens may be parsed out by passing in a delimiter,
   as follows:

|  VAR
|    txt : TEXT;
|  BEGIN
|    WHILE txtRd.next(" ,.", txt) DO
|      ( parse txt )
|    END
|  END

   To get the rest of the line, pass ``'' as the delims.
   It is a checked run-time error to pass NIL as the delims or as line.
*)


EXCEPTION
  NoMore;

TYPE
  T <: Public;

(* All the methods of a "TextReader.T" leave the reader in a state
   to parse further untouched tokens. *)

  Public = OBJECT METHODS

    next(delims : TEXT; VAR chunk : TEXT; skipNulls := FALSE) : BOOLEAN;
(*     get next word before "delims" from reader.  If "skipNulls" is "TRUE", 
       zero-length strings are never returned.  Return value is "TRUE"
       if call succeeded.  If nothing was left, call fails, and returns
       "FALSE". *)

    nextS(delims : SET OF CHAR; 
          VAR chunk : TEXT; skipNulls := FALSE) : BOOLEAN;
(* untested as yet *)

    nextE(delims : TEXT; skipNulls := FALSE) : TEXT RAISES { NoMore };
(* same as "next", except failure is signalled thru an exception *)

    init(line : TEXT) : T;
(* initialize a new "TextReader.T" *)

    initFromRd(rd : Rd.T) : T RAISES { Rd.Failure, Thread.Alerted };
(* initialize from an "Rd.T".  "rd" must eventually end (to allow in-memory
   implementations) *)

    isEmpty() : BOOLEAN;
(* probe a "TextReader.T" *)

    shatter(listDelims : TEXT; 
            endDelims : TEXT; skipNulls := FALSE) : TextList.T;
(* tokenize a line into "TEXT" tokens until EOT or an endDelim.
   It is a checked runtime error for there to be an overlap between
   "listDelims" and "endDelims" *)

    pushBack(t: TEXT);
(* insert "t" before remaining unread "TEXT". "t" must end in
   delimiter(s) if the next call to "next" is not to run past the
   end of "t". *)

  END;
END TextReader.
    
