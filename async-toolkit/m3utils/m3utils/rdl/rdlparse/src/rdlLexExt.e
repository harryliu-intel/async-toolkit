%source rdl.t rdl.l
%import rdlTok rdlLex

%module {
IMPORT TextSet;
IMPORT Debug;
VAR doDebug := Debug.DebugThis("rdlLexExt");

PROCEDURE RegisterUserdefProperty(lexer : T; txt : TEXT) =
  BEGIN EVAL lexer.userDefProperties.insert(txt) END RegisterUserdefProperty;

PROCEDURE GetUserDefProperties(lexer : T) : TextSet.T =
  BEGIN RETURN lexer.userDefProperties END GetUserDefProperties;
}

%interface {
IMPORT TextSet;

PROCEDURE RegisterUserdefProperty(lexer : T; txt : TEXT);

PROCEDURE GetUserDefProperties(lexer : T) : TextSet.T;
}

%public {
  userDefProperties : TextSet.T;
}

T_mID: { val : TEXT }
T_mPROPERTY: { val : TEXT }

T_mID {IF self.userDefProperties.member($) THEN
         IF doDebug THEN Debug.Out("Returning user defined property " & $) END;
         RETURN NEW(T_mPROPERTY, val := $)
       ELSE
         RETURN NEW(T_mID, val := $)
       END}

T_mNUM: { val : TEXT }
T_mNUM { RETURN NEW(T_mNUM, val := $) }

T_mSTR: { val : TEXT }
T_mSTR { RETURN NEW(T_mSTR, val := $) }


