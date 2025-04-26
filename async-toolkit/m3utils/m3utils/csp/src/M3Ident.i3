INTERFACE M3Ident;

PROCEDURE Escape(str : TEXT) : TEXT;

EXCEPTION Error;
          
PROCEDURE Unescape(str : TEXT) : TEXT RAISES { Error } ;

END M3Ident.
