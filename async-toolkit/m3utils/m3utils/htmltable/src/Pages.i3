(* $Id$ *)

INTERFACE Pages;
IMPORT HTMLPage;
IMPORT Session;
IMPORT DBerr;
IMPORT Request;
IMPORT PageDispatch;

EXCEPTION NotFound;

PROCEDURE Dispatch(request : Request.T) : HTMLPage.T RAISES { DBerr.Error, NotFound };

TYPE Priv = Session.Priv;

PROCEDURE AddDispatch(name : TEXT; source : PageDispatch.T; signinNeeded := TRUE); 

PROCEDURE GetPageDispatch(named : TEXT) : PageDispatch.T;
  (* get a page dispatch record just to see what is stored with this name *)

END Pages.
