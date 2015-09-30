INTERFACE Wrap;
IMPORT Rd;

PROCEDURE ReadTillEnd(rd : Rd.T) : TEXT RAISES { Rd.Failure } ;

PROCEDURE FilterBetween(txt              : TEXT; 
                        sTarget, eTarget : TEXT; 
                        keep             : BOOLEAN) : TEXT;

PROCEDURE FilterBetweenAts(txt    : TEXT; 
                           symbol : TEXT; 
                           keep   : BOOLEAN) : TEXT;
  (* delete or keep text from @SYMBOL>@ to @<SYMBOL@, depending on keep *)

PROCEDURE SelectBetweenAts(txt    : TEXT; 
                           symbol : TEXT; 
                           val    : TEXT) : TEXT;
  (* keep text from @SYMBOL=val>@ to @<SYMBOL=val@, delete all text 
     between @SYMBOL=x>@ to @<SYMBOL=x@ \forall x : x \neq val *)

END Wrap.
