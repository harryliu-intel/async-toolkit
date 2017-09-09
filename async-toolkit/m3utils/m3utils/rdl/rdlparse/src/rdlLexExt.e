%source rdl.t rdl.l
%import rdlTok rdlLex

%module {
IMPORT TextSetDef;
IMPORT Debug;
VAR doDebug := Debug.DebugThis("rdlLexExt");

VAR userDefProperties := NEW(TextSetDef.T).init();

PROCEDURE RegisterUserdefProperty(txt : TEXT) =
  (* s.b. hierarchical? *)
  BEGIN EVAL userDefProperties.insert(txt) END RegisterUserdefProperty;

}

%interface {
PROCEDURE RegisterUserdefProperty(txt : TEXT);
}

T_mID: { val : TEXT }

T_mPROPERTY: { val : TEXT }

T_mID {IF userDefProperties.member($) THEN
         IF doDebug THEN Debug.Out("Returning user defined property " & $) END;
         RETURN NEW(T_mPROPERTY, val := $)
       ELSE
         RETURN NEW(T_mID, val := $)
       END}

