(* $Id: FactorialBindings.i3,v 1.4 2010/04/22 20:10:05 mika Exp $ *)

INTERFACE FactorialBindings;

EXCEPTION NoSuchVar(Var);

TYPE
  Type = { Boolean, Integer, LongReal, Text, Any };
  Var = RECORD name : TEXT; type : Type END;

CONST 
  TypeNames = ARRAY Type OF TEXT {  "Boolean", "Integer", "LongReal", "Text", "**ANY**" }; 

TYPE
  T <: Public;

  Public = OBJECT METHODS
    getBool(named : TEXT) : BOOLEAN RAISES { NoSuchVar };
    getInt(named : TEXT) : INTEGER RAISES { NoSuchVar };
    getLR(named : TEXT) : LONGREAL RAISES { NoSuchVar };
    getText(named : TEXT) : TEXT RAISES { NoSuchVar };
    getIndex(named : TEXT) : CARDINAL RAISES { NoSuchVar };
    (* this one gets the index of the use, rather than the value *)

    lookup(named : TEXT) : CARDINAL RAISES { NoSuchVar };
    (* this method is called by all the above to find a binding *)

    (* each one of the above methods counts as a "use" of the named var *)

    format() : TEXT;  
    (* unique string *)

    formatValues(betweenString := "";
                 equalsString := "=";
                 afterString := "\n") : TEXT; 
    (* human-readable output of all the bindings... 
       DOES NOT count as a "use" of the variables. *)

    iterateUnused() : VarIterator;
    (* which variables have NOT been used yet? *)

    iterateAll() : VarIterator;

    initCopy(from : T) : T; 
    (* initialize a new T from an old one:
       modifying the copy will update the original's use state,
       but not the values (which shouldnt be modified anyhow).
    *)

  END;

TYPE 
  VarIterator <: PublicVarIterator;

  PublicVarIterator = OBJECT METHODS
    next(VAR var : Var) : BOOLEAN
  END;

CONST Brand = "FactorialBindings";

END FactorialBindings.
