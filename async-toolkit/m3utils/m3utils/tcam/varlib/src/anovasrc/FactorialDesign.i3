(* $Id: FactorialDesign.i3,v 1.8 2009/11/19 09:57:44 mika Exp $ *)

INTERFACE FactorialDesign;
IMPORT FactorialBindings, FactorialSuper, FactorialIndex;
IMPORT Wr, OSError;
IMPORT TextSet, IntSet;

TYPE 
  T <: Public;

  Public = FactorialSuper.T OBJECT METHODS
    init(name, directory : TEXT; 
         samplesPerSlot : CARDINAL;
         READONLY responseNames : ARRAY OF TEXT;
         doOpenFile : BOOLEAN) : T RAISES { OSError.E } ;
    (* if openFile is FALSE, we don't open the log file *)

    initWr(wr : Wr.T;
           name, directory : TEXT;
           samplesPerSlot : CARDINAL;
           READONLY responseNames : ARRAY OF TEXT) : T;

    openFile() RAISES { OSError.E };
    (* explicitly open the logfile *)

    filePath() : TEXT;
    (* path to the log file *)

    iterate() : Iterator;

    getAllTextBindings(binding : TEXT; 
                       into : TextSet.T := NIL) : TextSet.T RAISES { FactorialBindings.NoSuchVar };

    getAllIntBindings(binding : TEXT; 
                      into : IntSet.T := NIL) : IntSet.T RAISES { FactorialBindings.NoSuchVar };

    recordResults(bindings : FactorialBindings.T; 
                  READONLY data : ARRAY OF LONGREAL
                  ) RAISES { Wr.Failure };
    close() RAISES { Wr.Failure };
    size() : CARDINAL;

    getIndexedBindings(idx : FactorialIndex.T) : FactorialBindings.T;

    getSamplesPerSlot() : CARDINAL;
  END;

  Iterator <: PublicIterator;

  PublicIterator = OBJECT METHODS
    next(VAR bindings : FactorialBindings.T) : BOOLEAN;
  END;

CONST Brand = "FactorialDesign";

END FactorialDesign.
