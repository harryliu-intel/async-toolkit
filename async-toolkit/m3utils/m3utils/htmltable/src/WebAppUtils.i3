INTERFACE WebAppUtils;
IMPORT TextTable, FloatMode, Lex;

CONST VerboseDebug = TRUE;

PROCEDURE ParseQuestionMarkData(data : TEXT;
                                debug := VerboseDebug) : TextTable.T 
  RAISES { FloatMode.Trap, Lex.Error };

PROCEDURE ParseEnv(verboseDebug := VerboseDebug) : TextTable.T;

CONST Brand = "WebAppUtils";

END WebAppUtils.
