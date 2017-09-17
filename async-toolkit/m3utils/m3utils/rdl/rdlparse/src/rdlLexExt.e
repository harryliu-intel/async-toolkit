%source rdl.t rdl.l
%import rdlTok rdlLex

%module {
IMPORT TextSetDef, TextSet;
IMPORT Debug;
VAR doDebug := Debug.DebugThis("rdlLexExt");

VAR userDefProperties := NEW(TextSetDef.T).init();

PROCEDURE RegisterUserdefProperty(txt : TEXT) =
  BEGIN EVAL userDefProperties.insert(txt) END RegisterUserdefProperty;

PROCEDURE GetUserDefProperties() : TextSet.T =
  BEGIN RETURN userDefProperties END GetUserDefProperties;
}

%interface {
IMPORT TextSet;

PROCEDURE RegisterUserdefProperty(txt : TEXT);

PROCEDURE GetUserDefProperties() : TextSet.T;
}

T_mID: { val : TEXT }
T_mPROPERTY: { val : TEXT }

T_mID {IF userDefProperties.member($) THEN
         IF doDebug THEN Debug.Out("Returning user defined property " & $) END;
         RETURN NEW(T_mPROPERTY, val := $)
       ELSE
         RETURN NEW(T_mID, val := $)
       END}

T_mNUM: { val : TEXT }
T_mNUM { RETURN NEW(T_mNUM, val := $) }

T_mSTR: { val : TEXT }
T_mSTR { RETURN NEW(T_mSTR, val := $) }


