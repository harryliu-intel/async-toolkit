INTERFACE SpiceParse;

PROCEDURE HavePrefix(READONLY line : ARRAY OF CHAR;
                     VAR         p : CARDINAL;
                     search        : TEXT) : BOOLEAN;
(* line starts (from p) with search and has whitespace after, advances p to
   next token  *)

PROCEDURE CaseIns(a, b : CHAR) : BOOLEAN;
(* case insensitive compare of two characters *)
  
END SpiceParse.
