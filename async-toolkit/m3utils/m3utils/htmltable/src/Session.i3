INTERFACE Session;
IMPORT DBerr;

TYPE
  T <: Public;
  UserId = CARDINAL;

TYPE
  (* privilege levels for different kinds of users, the order matters *)
  Priv = { User, Admin };

CONST
  (* the PrivNames must match the symbolic names in the SQL database *)
  PrivNames = ARRAY Priv OF TEXT { "user", "admin" };

  
TYPE
  Public = OBJECT
  METHODS
    getUserInfo(info : TEXT) : TEXT RAISES { DBerr.Error } ;
    getUserName() : TEXT RAISES { DBerr.Error } ;
    getUser() : UserId;
    getPriv() : Priv;
    init(userId : UserId; hostAddr : TEXT) : T RAISES { DBerr.Error };
    getId() : TEXT;
  END;

(* Validate is used to validate an existing hostAddr/key pair.  It will *)
(* return a handle to a valid session object if successful, NIL if failed. *)
PROCEDURE Validate(hostAddr, key : TEXT) : T RAISES { DBerr.Error };

END Session.
