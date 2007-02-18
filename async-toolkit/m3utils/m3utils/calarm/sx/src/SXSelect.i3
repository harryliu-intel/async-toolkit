(* $Id$ *)

INTERFACE SXSelect;
IMPORT SX, SXRef;

PROCEDURE Wait(READONLY on : ARRAY OF SX.T);
(* normal Wait: returns as soon as any of the elements of on changes *)
  
PROCEDURE WaitE(READONLY on : ARRAY OF SX.T; 
                except : SXRef.T) RAISES { Exception };
(* Exception-Wait: behaves same as Wait, except that it also raises 
   an Exception when except becomes non-NIL.  The argument of the
   exception will be the new value of except *)
EXCEPTION Exception(REFANY);

END SXSelect.
