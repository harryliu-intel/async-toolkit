(* $Id$ *)

MODULE Request;
IMPORT TextTable, HTML, HTMLForm, HTMLInput;

REVEAL
  T = Public BRANDED Brand OBJECT
    envVars : TextTable.T;
    postVars : TextTable.T;
  OVERRIDES
    init := Init;
    getEnvVar := GetEnvVar;
    getPostVar := GetPostVar;
    addPostVarsAsHidden := AddPostVarsAsHidden;
  END;

PROCEDURE Init(t : T; envVars, postVars : TextTable.T) : T =
  BEGIN t.envVars := envVars; t.postVars := postVars; RETURN t END Init;

PROCEDURE GetEnvVar(t : T; named : TEXT; VAR value : TEXT) : BOOLEAN =
  BEGIN RETURN t.envVars.get(named,value) END GetEnvVar;

PROCEDURE GetPostVar(t : T; named : TEXT; VAR value : TEXT) : BOOLEAN =
  BEGIN RETURN t.postVars.get(named,value) END GetPostVar;

PROCEDURE AddPostVarsAsHidden(t : T; to : HTML.T) =
  VAR
    nam, val : TEXT;
  BEGIN
    WITH iter = t.postVars.iterate() DO
      WHILE iter.next(nam,val) DO
        WITH input = NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                           name := nam,
                                           value := val) DO
          NARROW(to,HTMLForm.T).add(input)
        END
      END
    END
  END AddPostVarsAsHidden;

BEGIN END Request.
