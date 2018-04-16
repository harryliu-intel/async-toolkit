INTERFACE IdStyles;
IMPORT TextSeq;

TYPE
  Case = { Lower, Upper, Camel };
  Sep = { None, Underscore };

PROCEDURE Parse(id : TEXT; case : Case; sep : Sep) : TextSeq.T;
  (* parse into canonical sequence of sequence of lower case words *)

PROCEDURE Format(seq : TextSeq.T; case : Case; sep : Sep) : TEXT;

PROCEDURE Convert(id : TEXT; frCase, toCase : Case; frSep, toSep : Sep) : TEXT;

CONST Brand = "IdStyles";

END IdStyles.
