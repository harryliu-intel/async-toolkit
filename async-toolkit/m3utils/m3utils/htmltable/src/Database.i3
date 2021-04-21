(* $Id$ *)

(* Copyright (c) 2000, 2005, Mika Nystrom.  All rights reserved. *)
(* Copyright (c) 2006, Generation Capital Ltd.  All rights reserved. *)

INTERFACE Database;
IMPORT DBerr;
IMPORT DatabaseTable;

(* N.B., on Windows, all the below routines behave as if busyWait is always
   TRUE *)

(* env. vars for debugging:

   DEBUGDATABASE      --   debug all query execs
   DBFAILPROBABILITY  --   randomly error out stated fraction of queries
*)

TYPE
  Type = { PostgreSQL, MySQL };
  
  Postgres <: T;

  MySQL <: T;

  T = (*MUTEX*) OBJECT METHODS
    init() : T
      RAISES { DBerr.Error };

    getType() : Type;

    open(dbTextName : TEXT) 
      RAISES { DBerr.Error };
    openRemote (dbHost, dbPort, dbName, dbLogin, dbPwd : TEXT) 
      RAISES { DBerr.Error };
    (* the args must be properly escaped, using \ for ' and ' for spaces,
       as described in the PostgreSQL documentation for PQconnectdb *)

    connect(conninfo : TEXT;
            specifyingHost := FALSE) RAISES { DBerr.Error };
    (* new version of the above; has two advantages: 
       1. uses new conninfo string method
       2. is non-blocking, unlike the old version;

       set specifyingHost to TRUE if you are passing host=<hostname> 
       among the input variables *)
    
    close();

    isClosed() : BOOLEAN;
    (* returns TRUE if connection is closed, either by calling close() or
       thru an error *)

    (* the exec methods are all thread-safe (LOCK internally) *)

    (*OBSOLETE*)exec(query : TEXT; busyWait := FALSE; 
          abortConnectionOnFail := TRUE) : Result 
                  RAISES { DBerr.Error };
    execCA(VAR query : ARRAY OF CHAR; busyWait := FALSE; 
          abortConnectionOnFail := TRUE) : Result 
      RAISES { DBerr.Error };
    tExec(query : TEXT; 
          busyWait := FALSE; 
          abortConnectionOnFail := TRUE) : Table 
      RAISES { DBerr.Error };
    tExecCA(VAR query : ARRAY OF CHAR; busyWait := FALSE; 
          abortConnectionOnFail := TRUE) : Table 
      RAISES { DBerr.Error };
    (* \0-terminated  character array query *)

    name() : TEXT;

  END;

TYPE
  Vector = DatabaseTable.Vector;
  Matrix = DatabaseTable.Matrix;
  Table  = DatabaseTable.T;
  Result = DatabaseTable.Result;
  ByteA  = TEXT;

PROCEDURE Open (dbTextName : TEXT) RAISES { DBerr.Error };
PROCEDURE Connect (conninfo : TEXT; 
                   specifyingHost := FALSE) RAISES { DBerr.Error };

PROCEDURE OpenRemote (dbHost, dbPort, dbName, dbLogin, dbPwd : TEXT)
  RAISES { DBerr.Error };
(* dbPort to NIL will give def. behavior *)

PROCEDURE Static() : T;
  (* return the global T used by the above routines -- it defaults to being
     a fresh instance of type Postgres *)

PROCEDURE SetStatic(to : T);
  (* make a different global T to use above *)

PROCEDURE Close();

PROCEDURE GetType() : Type;

PROCEDURE Exec(qry : TEXT; busyWait := FALSE) : Result RAISES { DBerr.Error };

PROCEDURE TExec(qry : TEXT; busyWait := FALSE) : Table RAISES { DBerr.Error };
(* same thing, but return Table directly *)

PROCEDURE TExecCA(VAR query : ARRAY OF CHAR; busyWait := FALSE) : Table 
  RAISES { DBerr.Error };
(* same thing, but return Table directly; query must be \000 terminated *)

CONST Brand = "PSQL Database";

PROCEDURE Escape(str : TEXT; quoted := FALSE) : TEXT;
  (* escape a string so that it works in an SQL insert 
     (escape single quotes) *)

PROCEDURE EscapeQ(str : TEXT) : TEXT;
  (* as Escape but add single quotes *)

PROCEDURE Unescape(str : TEXT) : TEXT;
  (* reverse of Escape, no inverse of EscapeQ needed since the DB
     doesn't insert single quotes in results *)

END Database.
