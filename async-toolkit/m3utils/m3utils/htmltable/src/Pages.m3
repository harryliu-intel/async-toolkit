(* $Id$ *)

MODULE Pages;
IMPORT HTML;
IMPORT HTMLPage;
IMPORT Request;
IMPORT Text;
IMPORT PageDispatch;
IMPORT DBerr;

REVEAL 
  PageDispatch.T = PageDispatch.Public BRANDED PageDispatch.Brand OBJECT END;

VAR mu := NEW(MUTEX);

PROCEDURE AddDispatch(name : TEXT; 
                      source : PageDispatch.T;
                      signinNeeded : BOOLEAN) =
  VAR
    newArr : REF ARRAY OF DispatchRec;
  BEGIN
    LOCK mu DO
      newArr := NEW( REF ARRAY OF DispatchRec , NUMBER(pageDispatch^) + 1);
      SUBARRAY(newArr^, 0, NUMBER(pageDispatch^)) := pageDispatch^;
      newArr[LAST(newArr^)].dispatch := source;
      newArr[LAST(newArr^)].name := name;
      newArr[LAST(newArr^)].signinNeeded := signinNeeded;
      pageDispatch := newArr
    END
  END AddDispatch;

PROCEDURE GetPageDispatch(named : TEXT) : PageDispatch.T =
  BEGIN
    FOR i := FIRST(pageDispatch^) TO LAST(pageDispatch^) DO
      IF Text.Equal(named,pageDispatch[i].name) THEN
        RETURN pageDispatch[i].dispatch
      END
    END;
    RETURN NIL
  END GetPageDispatch;

(* need a standard way of telling where we came from, where we are going, *)
(* transferring user credentials, etc. *)

PROCEDURE Dispatch(request : Request.T) : HTMLPage.T 
  RAISES { DBerr.Error, NotFound } =
  BEGIN
    IF request.toPage = NIL THEN RAISE NotFound END;
    FOR i := FIRST(pageDispatch^) TO LAST(pageDispatch^) DO
      IF Text.Equal(request.toPage,pageDispatch[i].name) THEN

        (* here we should check authorization etc. *)
        IF request.session = NIL AND pageDispatch[i].signinNeeded THEN
          HTML.Error("No valid session key presented --- PLEASE SIGN IN!")
        END;

        IF pageDispatch[i].dispatch.privLevel > Priv.User THEN
          IF request.session = NIL OR 
            request.session.getPriv() < pageDispatch[i].dispatch.privLevel THEN
            HTML.Error("Authorization insufficient.", TRUE)
          END
        END;

        RETURN pageDispatch[i].dispatch.display(request)
      END
    END;
    RAISE NotFound
  END Dispatch;

(* we could change this so that AddDispatch creates an object.. this *)
(* would let us use that object implicitly in ways that are difficult *)
(* to do without that association *)


TYPE 
  DispatchRec = RECORD
    name : TEXT := NIL;
    dispatch : PageDispatch.T;
    signinNeeded : BOOLEAN;
  END;

VAR
  pageDispatch : REF ARRAY OF DispatchRec;

BEGIN 
  pageDispatch :=  NEW( REF ARRAY OF DispatchRec , 0 );
END Pages.
