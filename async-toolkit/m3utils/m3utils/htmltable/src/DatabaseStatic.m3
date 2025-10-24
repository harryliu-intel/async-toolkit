MODULE DatabaseStatic EXPORTS Database;
IMPORT DBerr;

PROCEDURE Connect(conninfo : TEXT; 
                  specifyingHost : BOOLEAN)
  RAISES { DBerr.Error } = 
  BEGIN static.connect(conninfo,specifyingHost) END Connect;

PROCEDURE Open (dbTextName : TEXT)
  RAISES { DBerr.Error } = 
  BEGIN static.open(dbTextName) END Open;

PROCEDURE OpenRemote (dbHost, dbPort, dbName, dbLogin, dbPwd : TEXT) 
  RAISES { DBerr.Error } =
  BEGIN 
    static.openRemote(dbHost, dbPort, dbName, dbLogin, dbPwd)
  END OpenRemote;

PROCEDURE Close() =
  BEGIN static.close() END Close;

PROCEDURE Exec(query : TEXT; busyWait : BOOLEAN) : Result
  RAISES { DBerr.Error } =
  BEGIN
    RETURN static.exec(query,busyWait)
  END Exec;

PROCEDURE TExec(query : TEXT; busyWait : BOOLEAN) : Table
  RAISES { DBerr.Error } =
  BEGIN
    RETURN static.tExec(query,busyWait)
  END TExec;
 
PROCEDURE TExecCA(VAR query : ARRAY OF CHAR; busyWait : BOOLEAN) : Table
  RAISES { DBerr.Error } =
  BEGIN
    RETURN static.tExecCA(query,busyWait)
  END TExecCA;
 
PROCEDURE Static() : T =
  BEGIN RETURN static END Static;

PROCEDURE GetType() : Type =
  BEGIN RETURN static.getType() END GetType;

VAR static : T := NEW(Postgres);

PROCEDURE SetStatic(to : T) = BEGIN static := to END SetStatic;

BEGIN
END DatabaseStatic.
