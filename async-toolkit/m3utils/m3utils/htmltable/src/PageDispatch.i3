(* $Id$ *)

INTERFACE PageDispatch;
IMPORT Request, HTMLPage, DBerr;
IMPORT Session;

TYPE 
  T <: Public;

  Priv = Session.Priv;

  Public = OBJECT 
    privLevel := Priv.User;
  METHODS
    display(request : Request.T) : HTMLPage.T RAISES { DBerr.Error };
  END;
  
CONST Brand = "PageDispatch";

END PageDispatch.
