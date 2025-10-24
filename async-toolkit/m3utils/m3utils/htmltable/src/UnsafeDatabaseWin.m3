(* $Id$ *)

UNSAFE MODULE UnsafeDatabaseWin EXPORTS Database, DatabaseClass;
FROM M3toC IMPORT CopyTtoS, FreeCopiedS, CopyStoT;
IMPORT DBerr;
IMPORT Fmt;
FROM Ctypes IMPORT char_star;
IMPORT Debug;
IMPORT Text;
FROM DatabaseUtils IMPORT FilterUnprintable;
IMPORT Env;

(* postgres stuff *)
FROM PQ IMPORT PGconn_star, PGresult_star, PQfinish, PQsetdbLogin,
               PQstatus,
               PQerrorMessage, PQresultStatus, PQclear, CONNECTION, PGRES, 
               PQntuples, PQgetvalue, PQnfields, PQfname, PQsendQuery, 
               PQgetResult, PQconsumeInput, PQisBusy, PQsocket, PQflush,
               PQconnectStart, PQconnectPoll, PostgresPollingStatusType;
(*IMPORT SchedulerPosix; (* Not on Windows *) *)
IMPORT Thread; (* Not on Unix *)

PROCEDURE MyExec(conn : PGconn_star;
                 query : char_star;
                 busyWait : BOOLEAN) : PGresult_star =
  VAR
    NullResult := LOOPHOLE(0,PGresult_star);
  VAR
    res, r := NullResult;
    cnt : CARDINAL;
  BEGIN 
    (* RETURN PQexec(conn,query) *)

    EVAL PQsendQuery(conn,query);

    
    REPEAT
      IF res # NullResult THEN PQclear(res) END;
      res := r;

      cnt := 0;
      IF PQisBusy(conn)=1 THEN
        WHILE PQisBusy(conn) =1 DO
          EVAL PQflush(conn); (* does this solve the timeouts? *) (* hack *)

(*
          IF NOT busyWait THEN
            Debug.Out("Calling SchedulerPosix.IOWait.",20);
            
            (* the 0.1 second delay is just a safety thing, in case we
               missed entering and there's something waiting now.  Yes,
               I know it's ugly, but it is safe, and it works... 
               (PQflush below may have fixed it)

               The select is actually pretty slow on most systems and
               for maximum performance, use busyWait, which doesn't
               wait but loops back and tries again right away. That eats
               CPU, though, ... *)

            EVAL PQflush(conn); (* does this solve the timeouts? *)

            WITH waitRes = SchedulerPosix.IOWait(PQsocket(conn),TRUE,0.1d0) DO
              IF waitRes = SchedulerPosix.WaitResult.Timeout THEN
                Debug.Out("SchedulerPosix.IOWait TIMED OUT",20)
              END
            END (*WITH*)

          END(*IF NOT busyWait*);
*)

          EVAL PQconsumeInput(conn); INC(cnt)
        END
      END;

      Debug.Out(Fmt.Int(cnt) & " attempts to consume.",11);

      r := PQgetResult(conn);
    UNTIL r = NullResult;

    RETURN res
  END MyExec;

PROCEDURE ExecCA(t : Postgres; 
                 VAR arr : ARRAY OF CHAR;
                 busyWait : BOOLEAN;
                 abortConnectionOnFail : BOOLEAN) : Result
  RAISES { DBerr.Error } =
  VAR
    query : char_star;
  BEGIN
    LOCK t.mu DO
    Debug.Out("Executing query ExecCA of array length " & 
      Fmt.Int(NUMBER(arr)),20);

    Debug.Out("conn = 0x" & Fmt.Int(LOOPHOLE(t.conn,INTEGER),16));

    arr[LAST(arr)] := '\000';
    
    query := LOOPHOLE(ADR(arr[0]),char_star);

    t.res := MyExec(t.conn,query,busyWait);

    CASE PQresultStatus(t.res) OF

      (* how do we handle the copyin, copyout statuses? *)

    | PGRES.COMMAND_OK , PGRES.TUPLES_OK => 
      Debug.Out("PQexec returned SQL: PGRES.COMMAND_OK or PGRES.TUPLES_OK");
      IF t.res = LOOPHOLE(0,PGresult_star) THEN Debug.Out("NULL PGresult_star") 
      ELSE 
        Debug.Out("non-NULL PGresult_star 0x" & 
          Fmt.Int(LOOPHOLE(t.res,INTEGER),16))
      END;
      RETURN NEW(Result, actual := t.res, status := PQresultStatus(t.res));
    ELSE
      Debug.Out("PQexec returned SQL: database error");
      t.errText := CopyStoT(PQerrorMessage(t.conn));
      PQclear (t.res);
      PQfinish (t.conn);
      RAISE DBerr.Error("Query \"" & CopyStoT(query) & "\" Failed: " & t.errText);
    END;
    END
  END ExecCA;

PROCEDURE ExecM(t : Postgres;
                query : TEXT;
                busyWait : BOOLEAN;
                abortConnectionOnFail : BOOLEAN) : Result
  RAISES { DBerr.Error } = 
  VAR dOut : TEXT;
  BEGIN
    LOCK t.mu DO
    IF Debug.GetLevel() > 100 OR Text.Length(query) <= 1000 THEN
      Debug.Out("Executing query: " & FilterUnprintable(query))
    ELSE
      Debug.Out("Executing query: " & FilterUnprintable(Text.Sub(query, 0, 1000)) & "..." )
    END;
    Debug.Out("conn = 0x" & Fmt.Int(LOOPHOLE(t.conn,INTEGER),16),21);
    VAR
      s := CopyTtoS(query);
    BEGIN
      TRY
        t.res := MyExec(t.conn,s,busyWait)
      FINALLY
        FreeCopiedS(s)
      END
    END;
    CASE PQresultStatus(t.res) OF

      (* how do we handle the copyin, copyout statuses? *)

    | PGRES.COMMAND_OK , PGRES.TUPLES_OK => 
      dOut := "PQexec returned SQL: PGRES.COMMAND_OK or PGRES.TUPLES_OK";
      IF t.res = LOOPHOLE(0,PGresult_star) THEN Debug.Out(dOut & ": NULL PGresult_star") 
      ELSE 
        Debug.Out(dOut & ": non-NULL PGresult_star 0x" & 
          Fmt.Int(LOOPHOLE(t.res,INTEGER),16))
      END;
      RETURN NEW(Result, actual := t.res, status := PQresultStatus(t.res));
    ELSE
      Debug.Out("SQL: database error");
      t.errText := CopyStoT(PQerrorMessage(t.conn));
      PQclear (t.res);
      PQfinish (t.conn);
      RAISE DBerr.Error("Query \"" & query & "\" Failed: " & t.errText);
    END
  END
  END ExecM;

PROCEDURE TExecM(t : T;
                 query : TEXT;
                 busyWait : BOOLEAN;
                 abortConnectionOnFail : BOOLEAN) : Table
  RAISES { DBerr.Error } = 
  VAR
    res := t.exec(query,busyWait);
  BEGIN
    TRY
      RETURN NEW(Table).init(res) 
    FINALLY
      IF res.actual # LOOPHOLE(0,PGresult_star) THEN 
        PQclear(LOOPHOLE(res.actual,PGresult_star))
      END
    END
  END TExecM;

PROCEDURE TExecCAM(t : T;
                   VAR query : ARRAY OF CHAR;
                   busyWait : BOOLEAN;
                   abortConnectionOnFail : BOOLEAN) : Table
  RAISES { DBerr.Error } = 
  VAR
    res := t.execCA(query,busyWait);
  BEGIN
    TRY
      RETURN NEW(Table).init(res) 
    FINALLY
      IF res.actual # LOOPHOLE(0,PGresult_star) THEN 
        PQclear(LOOPHOLE(res.actual,PGresult_star))
      END
    END
  END TExecCAM;

(* build an opaque table from a PGresult *)
PROCEDURE InitTable(self : Table; from : Result) : Table =
  VAR
    rows := PQntuples(from.actual);
    cols := PQnfields(from.actual);
  BEGIN
    self.data := NEW(Matrix, rows, cols);
    
    FOR row := 0 TO rows - 1 DO
      FOR col := 0 TO cols - 1 DO
        self.data[row,col] := CopyStoT(PQgetvalue(from.actual, row, col))
      END
    END;

    (* set the field headers *)
    self.fieldNames := NEW(Vector, cols);
    FOR col := 0 TO cols - 1 DO
      self.fieldNames[col] := CopyStoT(PQfname(from.actual, col));
    END;
    RETURN self;
  END InitTable;

PROCEDURE OpenM(t : Postgres;
                dbTextName : TEXT) RAISES { DBerr.Error } =
  BEGIN  
    t.mu := NEW(MUTEX);

    IF t.dbName # null THEN FreeCopiedS(t.dbName) END;

    t.cName := dbTextName;

    t.dbName :=  CopyTtoS (dbTextName);
    t.conn := PQsetdbLogin(null, null, null, null, t.dbName, null, null);
    IF (PQstatus(t.conn) = CONNECTION.BAD) THEN
      t.errText := CopyStoT(PQerrorMessage(t.conn));
      PQfinish (t.conn);
      RAISE DBerr.Error(t.errText);
    END;  
    Debug.Out("Opened database connection \"" & dbTextName & "\" conn = 0x" &
      Fmt.Int(LOOPHOLE(t.conn,INTEGER),16));
  END OpenM;

PROCEDURE OpenRemoteM(t : Postgres;
                      dbHost, dbPort, dbName, dbLogin, dbPwd : TEXT)
  RAISES { DBerr.Error } = 
  VAR
    connstr := "";
  BEGIN
    t.cName := dbName;

    IF dbHost = NIL THEN
      dbHost := Env.Get("DBHOST") 
    END;
    IF dbHost # NIL THEN connstr := connstr & " host=" & dbHost END;
    IF dbPort # NIL THEN connstr := connstr & " port=" & dbPort END;
    IF dbName # NIL THEN connstr := connstr & " dbname=" & dbName END;
    IF dbLogin # NIL THEN connstr := connstr & " user=" & dbLogin END;
    IF dbPwd # NIL THEN connstr := connstr & " password=" & dbPwd END;

    t.connect(connstr)
  END OpenRemoteM;

PROCEDURE CloseM(t : Postgres) = BEGIN PQfinish(t.conn) END CloseM;

VAR (* CONST *) null := LOOPHOLE (0, ADDRESS);

REVEAL 
  Postgres = T BRANDED Brand OBJECT
    dbName : char_star;
    cName : TEXT;
    conn : PGconn_star;
    res : PGresult_star;
    errText : TEXT;
    mu : MUTEX;
  OVERRIDES
    open := OpenM;
    openRemote := OpenRemoteM;
    connect := ConnectM;
    close := CloseM;
    exec := ExecM;
    execCA := ExecCA;
    tExec := TExecM;
    tExecCA := TExecCAM;
    name := GetName;
  END;

PROCEDURE GetName(t : Postgres) : TEXT = 
  BEGIN 
    IF t.cName = NIL THEN
      RETURN "(NIL)"
    ELSE
      RETURN t.cName 
    END
  END GetName;

PROCEDURE ConnectM(t : Postgres; 
                   conninfo : TEXT; 
                   specifyingHost : BOOLEAN) RAISES { DBerr.Error } =
  TYPE 
    PGRES = PostgresPollingStatusType;
  BEGIN
    t.mu := NEW(MUTEX);

    IF NOT specifyingHost THEN
      WITH dbHost = Env.Get("DBHOST") DO
        IF dbHost # NIL THEN
          conninfo := conninfo & " host=" & dbHost
        END
      END
    END;

    WITH s = CopyTtoS(conninfo) DO
      TRY
        t.dbName := CopyTtoS(conninfo);

        t.conn := PQconnectStart(s);

        IF (PQstatus(t.conn) = CONNECTION.BAD) THEN
          t.errText := CopyStoT(PQerrorMessage(t.conn));
          PQfinish (t.conn);
          RAISE DBerr.Error(t.errText);
        END;  
        Debug.Out("Starting to open database connection \"" & 
          conninfo & "\" conn = 0x" &
          Fmt.Int(LOOPHOLE(t.conn,INTEGER),16));

        LOOP
          WITH socket = PQsocket(t.conn), 
               poll = PQconnectPoll(t.conn) DO
            CASE poll OF 
              PGRES.POLLING_FAILED =>
                t.errText := CopyStoT(PQerrorMessage(t.conn));
                PQfinish (t.conn);
                RAISE DBerr.Error(t.errText);
            |
              PGRES.POLLING_READING =>
                (* EVAL SchedulerPosix.IOWait(socket,TRUE) *)
                Thread.Pause(0.1d0)
            |
              PGRES.POLLING_WRITING =>
                (* EVAL SchedulerPosix.IOWait(socket,FALSE) *)
                Thread.Pause(0.1d0)
            |
              PGRES.POLLING_OK =>
                Debug.Out("Asynchronous database connection successful: \"" & 
                  conninfo & "\" conn = 0x" &
                  Fmt.Int(LOOPHOLE(t.conn,INTEGER),16));
                EXIT
            |
              PGRES.POLLING_ACTIVE =>
                RAISE DBerr.Error("PGRES.POLLING_ACTIVE should not occur")
            END(*CASE*)
          END(*WITH*)
        END(*LOOP*)
      FINALLY
        FreeCopiedS(s)
      END
    END
  END ConnectM;

PROCEDURE Connect(conninfo : TEXT; 
                  specifyingHost : BOOLEAN)  RAISES { DBerr.Error } = 
  BEGIN static.connect(conninfo,specifyingHost) END Connect;

PROCEDURE Open (dbTextName : TEXT) RAISES { DBerr.Error } = 
  BEGIN static.open(dbTextName) END Open;

PROCEDURE OpenRemote (dbHost, dbPort, dbName, dbLogin, dbPwd : TEXT) 
  RAISES { DBerr.Error } =
  BEGIN 
    static.openRemote(dbHost, dbPort, dbName, dbLogin, dbPwd)
  END OpenRemote;

PROCEDURE Close() = BEGIN static.close() END Close;

PROCEDURE Exec(query : TEXT; busyWait : BOOLEAN) : Result RAISES { DBerr.Error } =
  BEGIN RETURN static.exec(query,busyWait) END Exec;

PROCEDURE TExec(query : TEXT; busyWait : BOOLEAN) : Table RAISES { DBerr.Error } =
  BEGIN RETURN static.tExec(query,busyWait) END TExec;
 
PROCEDURE TExecCA(VAR query : ARRAY OF CHAR; busyWait : BOOLEAN) : Table RAISES { DBerr.Error } =
  BEGIN RETURN static.tExecCA(query,busyWait) END TExecCA;
 
PROCEDURE Static() : T = BEGIN RETURN static END Static;

VAR static := NEW(Postgres, dbName := null);

BEGIN END UnsafeDatabaseWin.
