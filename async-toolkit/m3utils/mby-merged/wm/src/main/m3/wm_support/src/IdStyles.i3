INTERFACE IdStyles;
IMPORT TextSeq;

TYPE
  Case = { Lower, Upper, Camel };
  Sep = { None, Underscore, Hyphen };

CONST
  CaseNames = ARRAY Case OF TEXT { "lower", "upper", "camel" };
  SepNames = ARRAY Sep OF TEXT { "none", "underscore", "hyphen" };

PROCEDURE Parse(id : TEXT; case : Case; sep : Sep) : TextSeq.T;
  (* parse into canonical sequence of sequence of lower case words *)

PROCEDURE Format(seq : TextSeq.T; case : Case; sep : Sep) : TEXT;

PROCEDURE Convert(id : TEXT; frCase, toCase : Case; frSep, toSep : Sep) : TEXT;

CONST Brand = "IdStyles";

END IdStyles.
