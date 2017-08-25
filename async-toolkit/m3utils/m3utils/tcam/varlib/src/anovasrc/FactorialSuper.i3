(* $Id: FactorialSuper.i3,v 1.6 2009/06/12 00:25:21 mika Exp $ *)

INTERFACE FactorialSuper;
IMPORT FactorialValues AS Values, FactorialBindings;
FROM FactorialBindings IMPORT Type;
IMPORT RefList;
IMPORT TextSet;

IMPORT Wr, Rd;

(* this interface implements reading and writing factorialdesigns to/from 
   readers (in human-readable format) *)

TYPE B = FactorialBindings.T;

TYPE
  T <: Class;

  Class = OBJECT 
    wroteHeader := FALSE;
    name : TEXT;
    values : REF ARRAY OF Values.T;
    responses : REF ARRAY OF TEXT;
  METHODS
    typeOf(index : CARDINAL) : Type;

    (* the following methods are used either when you create a design
       externally, or internally by FactorialSuper when it parses from
       a file.. *)
    extendValues();

    addBoolVar(named : TEXT; READONLY values : ARRAY OF BOOLEAN);
    addIntVar(named : TEXT; READONLY values : ARRAY OF INTEGER);
    addLRVar(named : TEXT; READONLY values : ARRAY OF LONGREAL);
    addTextVar(named : TEXT; READONLY values : ARRAY OF TEXT);

    (* ***** methods for writing it to writer.... ***** *) 
    writeHeader(wr : Wr.T) RAISES { Wr.Failure };
    writeResults(wr : Wr.T; 
                 bindings : B; 
                 READONLY data : ARRAY OF LONGREAL) RAISES { Wr.Failure };
    close(wr : Wr.T) RAISES { Wr.Failure };

    initFromRd(fromRd : Rd.T;
               bindings : RefList.T := NIL) : T RAISES { Rd.Failure };
    (* read it from reader.... *)
    (* bindings is a RefList.T of FactorialVarBindings.T objects *)

    resultCallback(READONLY vi : ARRAY OF CARDINAL;
                   READONLY r  : ARRAY OF LONGREAL;
                   src : TEXT);
    (* this procedure is called on itself by initFromRd, once for each
       DATA line.  so override it ...! error not to and then call!!

       s is the name of the result (listed in the data file)
    *)
    
    findFactor(named : TEXT; VAR index : CARDINAL) : BOOLEAN;
    (* returns TRUE if treatment exists *)
    
    varyingFactors(into : TextSet.T := NIL) : TextSet.T;
  END;

END FactorialSuper.

