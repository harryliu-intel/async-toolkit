(* $Id$ *)

INTERFACE Request;
IMPORT TextTable, Session;
IMPORT HTML;

TYPE 
  T <: Public;

  Public = OBJECT 
    fromPage, toPage : TEXT := NIL;
    session : Session.T;
  METHODS
    init(envVars, postVars : TextTable.T) : T;
    getEnvVar(named : TEXT; VAR value : TEXT) : BOOLEAN;
    getPostVar(named : TEXT; VAR value : TEXT) : BOOLEAN;

    addPostVarsAsHidden(to : HTML.T (* must be HTMLForm.T *));
    (* add value of all "post" vars as hidden inputs to a given
       HTMLForm.T *)
  END;

CONST Brand = "Request";

END Request.
