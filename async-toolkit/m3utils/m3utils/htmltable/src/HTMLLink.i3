INTERFACE HTMLLink;
IMPORT HTML;
IMPORT Request;

TYPE
  T <: Public;

  (* make an HTML Link *)
  (* it is within the site if local is true *)

  (* perhaps we should change the "to" argument to take an 
     HTMLpage-producing function as arg.  That way we would not have
     to maintain multiple mappings for the same name. 
     
     Only external refs use a TEXT arg. *)

  Public = HTML.T OBJECT
  METHODS
    init(encloses : HTML.Stuff; (* embedded "linked" object *)
         to : TEXT;         (* where does it go to? *)
         from : Request.T;
         local := TRUE) : T;
    URL() : TEXT;
  END;


(* produce a suitable string for embedding an HTML link in the output *)
(* of a SQL query *)

PROCEDURE QueryField(request : Request.T;   (* current request *)
                     to : TEXT;                (* link to *)
                     encloses : TEXT;          (* text linked from *)
                     SQLfield : TEXT;          (* name of SQL field *)
                     local : BOOLEAN
                     ) : TEXT; 

PROCEDURE MakeURL(to : TEXT; from : Request.T ; local := TRUE ) : TEXT;

END HTMLLink.
