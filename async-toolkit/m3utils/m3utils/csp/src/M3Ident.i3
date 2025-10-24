INTERFACE M3Ident;

EXCEPTION Error;

  (* escape _ *)
PROCEDURE Escape(str : TEXT) : TEXT;
PROCEDURE Unescape(str : TEXT) : TEXT RAISES { Error } ;
  
  (* don't escape _ *)
PROCEDURE EscapeU(str : TEXT) : TEXT;
(*PROCEDURE UnescapeU(str : TEXT) : TEXT RAISES { Error } ;
(* this is broken now *)
*)

END M3Ident.
