MODULE Main;
IMPORT Stdio, Rd;
IMPORT Thread;
IMPORT Compiler;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT IO;
IMPORT BnfTokenizer;

VAR doDebug := Debug.GetLevel() >= 10 AND Debug.This("BnfGrammar");

VAR
  st  : BnfTokenizer.State;
  buf : BnfTokenizer.Buffer;
  tok : BnfTokenizer.Token;
BEGIN
  st.rd := Stdio.stdin;

  TRY
    WHILE BnfTokenizer.GetAny(buf, st, tok) DO
    END
  EXCEPT
    Rd.EndOfFile => (* done *)
  END
END Main.
