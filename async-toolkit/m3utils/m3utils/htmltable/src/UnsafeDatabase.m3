(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

UNSAFE MODULE UnsafeDatabase EXPORTS Database, DatabaseClass;
FROM M3toC IMPORT CopyTtoS, FreeCopiedS, CopyStoT;
IMPORT DBerr;
IMPORT Fmt;
FROM Ctypes IMPORT char_star;
IMPORT Debug;
IMPORT Text;
FROM DatabaseUtils IMPORT FilterUnprintable;
IMPORT Env;
IMPORT Scan, Lex, FloatMode, Random;
IMPORT Thread;
IMPORT Time (* NOT XTime *);
IMPORT TextTextTbl;

(* postgres stuff *)
FROM PQSchedulerWrap
        IMPORT PQfinish,
               PQstatus,
               PQerrorMessage, PQresultStatus, PQclear, 
               PQntuples, PQgetvalue, PQnfields, PQfname, PQsendQuery, 
               PQgetResult, PQconsumeInput, PQisBusy, PQsocket, PQflush,
               PQconnectStart, PQconnectPoll,
               PQgetisnull;

FROM PQ 
        IMPORT PGconn_star, PGresult_star, PGRES, PostgresPollingStatusType,
               CONNECTION;

FROM PGRES IMPORT PGRES_T;

IMPORT SchedulerPosix;


TYPE
  PostgresResult = Result BRANDED "PostgreSQL Query Result" OBJECT
    status : PGRES_T;
    actual : ADDRESS;
  OVERRIDES
    initTable := InitTable;
  END;

CONST DebugThis = "DATABASE";
CONST DebugThisTiming = "DATABASETIMING";

VAR (* CONST *) DebugTiming := Debug.DebugThis(DebugThisTiming);

PROCEDURE GetSocket(t : Postgres) : CARDINAL RAISES { DBerr.Error } =
  VAR 
    conn := t.conn;
  BEGIN
    WITH res = PQsocket(conn) DO
      IF res < 0 THEN

        WHILE t.reconnects > 0 DO
          Thread.Pause(ReconnectDelay);
          TRY
            t.connect(t.conninfo, t.specifyingHost);
            t.reconnects := MaxReconnects;
            EXIT
          EXCEPT
            DBerr.Error => DEC(t.reconnects)
          END
        END;

        IF PQsocket(t.conn) < 0 THEN
          Debug.Error("PQsocket(conn) = " & Fmt.Int(res) & 
            " < 0, connection error, RECOVERY ATTEMPTS FAILED.");
          <*ASSERT FALSE*>
        ELSE
          RAISE DBerr.Error("PQsocket(conn) = " & Fmt.Int(res) & 
                " < 0, connection error, recovered.")
        END
      ELSE
        RETURN res
      END
    END
  END GetSocket;

PROCEDURE MyExec(t        : Postgres;
                 query    : char_star;
                 busyWait : BOOLEAN) : PGresult_star RAISES { DBerr.Error } =
  VAR
    NullResult := LOOPHOLE(0,PGresult_star);
  VAR
    res, r := NullResult;
    cnt : CARDINAL;
    start, end : Time.T;
  BEGIN 

    IF failProb # 0.0d0 THEN
      WITH rand = NEW(Random.Default).init() DO
        IF rand.longreal(0.0d0,1.0d0) < failProb THEN
          RAISE DBerr.Error("random failure injected, DBFAILPROBABILITY=" &
                Fmt.LongReal(failProb))
        END
      END
    END;

    IF DebugTiming THEN start := Time.Now() END;

    EVAL PQsendQuery(t.conn,query);

    REPEAT
      IF res # NullResult THEN PQclear(res) END;
      res := r;

      cnt := 0;
      IF PQisBusy(t.conn)=1 THEN
        WHILE PQisBusy(t.conn) =1 DO
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

            (* this code only seems necessary for pre-pthreads code *)

            EVAL PQflush(t.conn); (* does this solve the timeouts? *)

            WITH waitRes = SchedulerPosix.IOWait(GetSocket(t),TRUE,0.1d0) DO
              IF waitRes = SchedulerPosix.WaitResult.Timeout THEN
                Debug.Out("SchedulerPosix.IOWait TIMED OUT",20)
              END
            END
          END;

          EVAL PQconsumeInput(t.conn); INC(cnt);
        END
      END;

      Debug.Out(Fmt.Int(cnt) & " attempts to consume.",11);

      r := PQgetResult(t.conn);
    UNTIL r = NullResult;

    IF DebugTiming THEN
      end := Time.Now();
      WITH delta = end - start DO
        Debug.Out("query time " & SloppyFmtNonNeg(delta))
      END
    END;

    RETURN res
  END MyExec;

PROCEDURE SloppyFmtNonNeg(x : LONGREAL) : TEXT =
  CONST
    Prec = 6;
  VAR
    i := TRUNC(x);
    c : ARRAY [0..Prec-1] OF CHAR;
  BEGIN
    x := x - FLOAT(i,LONGREAL);
    FOR p := 0 TO Prec-1 DO
      x := x * 10.0d0;
      i := TRUNC(x);
      c[p] := VAL(ORD('0') + i,CHAR);
      x := x - FLOAT(i,LONGREAL);
    END;

    RETURN Fmt.F("%s.%s",Fmt.Int(i),Text.FromChars(c))
  END SloppyFmtNonNeg;

PROCEDURE ExecCA(t : Postgres; 
                 VAR arr : ARRAY OF CHAR; 
                 busyWait, abortConnectionOnFail : BOOLEAN) : Result 
  RAISES { DBerr.Error } =
  VAR
    query : char_star;
  BEGIN
    LOCK t.mu DO
    Debug.Out("Executing query ExecCA of array length " & 
      Fmt.Int(NUMBER(arr)),20, this := DebugThis);

    Debug.Out("conn = 0x" & Fmt.Int(LOOPHOLE(t.conn,INTEGER),16),
              this := DebugThis);

    arr[LAST(arr)] := '\000';
    
    query := LOOPHOLE(ADR(arr[0]),char_star);

    t.res := t.myExec(query,busyWait);

    CASE PQresultStatus(t.res) OF

      (* how do we handle the copyin, copyout statuses? *)

    | PGRES.COMMAND_OK , PGRES.TUPLES_OK => 
      Debug.Out("PQexec returned SQL: PGRES.COMMAND_OK or PGRES.TUPLES_OK",
                this := DebugThis);
      IF t.res = LOOPHOLE(0,PGresult_star) THEN Debug.Out("NULL PGresult_star",
                this := DebugThis) 
      ELSE 
        Debug.Out("non-NULL PGresult_star 0x" & 
          Fmt.Int(LOOPHOLE(t.res,INTEGER),16),
                this := DebugThis)
      END;
      RETURN NEW(PostgresResult,
                 actual := t.res,
                 status := PQresultStatus(t.res));
    ELSE
      ReportQueryError(t, CopyStoT(query), abortConnectionOnFail);
      <*ASSERT FALSE*> (* not reached *)
    END;
    END
  END ExecCA;

CONST PGRESNames = ARRAY PGRES OF TEXT {   
  "EMPTY_QUERY", 
  "COMMAND_OK",  
  "TUPLES_OK",   
  "COPY_OUT",
  "COPY_IN",
  "BAD_RESPONSE",
  "NONFATAL_ERROR",
  "FATAL_ERROR" };


PROCEDURE ReportQueryError(t                     : Postgres; 
                           query                 : TEXT;
                           abortConnectionOnFail : BOOLEAN) 
  RAISES { DBerr.Error } = 
  BEGIN
    Debug.Out("PQexec returned SQL: database error, " & 
      PGRESNames[PQresultStatus(t.res)]);

    t.errText := CopyStoT(PQerrorMessage(t.conn));
    PQclear (t.res);

    IF abortConnectionOnFail THEN
      Debug.Out("ReportQueryError : connection aborted");
      PQfinish (t.conn);
      t.conn := NIL;
    ELSE
      Debug.Out("ReportQueryError : connection NOT aborted")
    END;
    Debug.Out("SQL: database error is " & t.errText);
    RAISE DBerr.Error("Query \"" & query & "\" Failed: " & t.errText)
  END ReportQueryError;

PROCEDURE ExecM(t                               : Postgres; 
                query                           : TEXT; 
                busyWait, abortConnectionOnFail : BOOLEAN) : Result 
  RAISES { DBerr.Error } = 
  VAR dOut : TEXT;
  BEGIN
    LOCK t.mu DO
    IF Debug.GetLevel() > 100 OR Text.Length(query) <= 1000 THEN
      Debug.Out("Executing query: " & FilterUnprintable(query),
                this := DebugThis)
    ELSE
      Debug.Out("Executing query: " & FilterUnprintable(Text.Sub(query, 0, 1000)) & "...", this := DebugThis )
    END;
    Debug.Out("conn = 0x" & Fmt.Int(LOOPHOLE(t.conn,INTEGER),16),21,
              this := DebugThis);
    VAR
(*    s := CopyTtoS(query); *)
      s := NEW(REF ARRAY OF CHAR, Text.Length(query)+1);
    BEGIN
      Text.SetChars(s^,query);
      s[LAST(s^)] := VAL(0,CHAR);
      TRY
        t.res := t.myExec(LOOPHOLE(ADR(s[0]),char_star),busyWait)
      FINALLY
        (*FreeCopiedS(s)*)
      END
    END;
    CASE PQresultStatus(t.res) OF

      (* how do we handle the copyin, copyout statuses? *)

    | PGRES.COMMAND_OK , PGRES.TUPLES_OK => 
      dOut := "PQexec returned SQL: PGRES.COMMAND_OK or PGRES.TUPLES_OK";
      IF t.res = LOOPHOLE(0,PGresult_star) THEN Debug.Out(dOut & ": NULL PGresult_star") 
      ELSE 
        Debug.Out(dOut & ": non-NULL PGresult_star 0x" & 
          Fmt.Int(LOOPHOLE(t.res,INTEGER),16),
          this := DebugThis)
      END;
      RETURN NEW(PostgresResult,
                 actual := t.res,
                 status := PQresultStatus(t.res))
    ELSE
      ReportQueryError(t, query, abortConnectionOnFail);
      <*ASSERT FALSE*> (* not reached *)
    END
  END
  END ExecM;

PROCEDURE TExecM(t : T; 
                 query : TEXT; 
                 busyWait, abortConnectionOnFail : BOOLEAN) : Table 
  RAISES { DBerr.Error } = 
  VAR
    res : PostgresResult := t.exec(query,busyWait,abortConnectionOnFail);
  BEGIN
    TRY
      RETURN NEW(Table).init(res) 
    FINALLY
      IF res.actual # LOOPHOLE(0,PGresult_star) THEN 
        PQclear(LOOPHOLE(res.actual,PGresult_star));
      END
    END
  END TExecM;

PROCEDURE TExecCAM(t : T; 
                   VAR query : ARRAY OF CHAR; 
                   busyWait, abortConnectionOnFail : BOOLEAN) : Table 
  RAISES { DBerr.Error } = 
  VAR
    res : PostgresResult := t.execCA(query,busyWait,abortConnectionOnFail);
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
PROCEDURE InitTable(from : PostgresResult; self : Table) : Table =
  VAR
    rows := PQntuples(from.actual);
    cols := PQnfields(from.actual);
  BEGIN
    self.data := NEW(Matrix, rows, cols);
    
    FOR row := 0 TO rows - 1 DO
      FOR col := 0 TO cols - 1 DO
        IF PQgetisnull(from.actual, row, col) # 0 THEN
          self.data[row,col] := NIL
        ELSE
          self.data[row,col] := CopyStoT(PQgetvalue(from.actual, row, col))
        END
      END
    END;

    (* set the field headers *)
    self.fieldNames := NEW(Vector, cols);
    FOR col := 0 TO cols - 1 DO
      self.fieldNames[col] := MemoizeT(CopyStoT(PQfname(from.actual, col)));
    END;
    RETURN self;
  END InitTable;

VAR ttbl := NEW(TextTextTbl.Default).init();
VAR tmu  := NEW(MUTEX);

PROCEDURE MemoizeT(t : TEXT) : TEXT =
  VAR tt : TEXT;
  BEGIN
    (* hope this works---no locking in common case *)
    IF NOT ttbl.get(t, tt) THEN 
      LOCK tmu DO
        IF NOT ttbl.get(t, tt) THEN
          tt := t;
          EVAL ttbl.put(t,t)
        END
      END
    END;
    RETURN tt
  END MemoizeT;


PROCEDURE OpenM(t : T; dbTextName : TEXT) RAISES { DBerr.Error } =
  BEGIN t.openRemote(NIL, NIL, dbTextName, NIL, NIL) END OpenM;

PROCEDURE OpenRemoteM(t                                      : Postgres;
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

    t.connect(connstr, specifyingHost := dbHost # NIL)
  END OpenRemoteM;

PROCEDURE CloseM(t : Postgres) =
  BEGIN PQfinish(t.conn); t.conn := NIL END CloseM;

CONST MaxReconnects = 5;      (* somewhat arbitrary *)
      ReconnectDelay = 5.0d0; (* also *)

REVEAL 
  Postgres = T BRANDED "(" & Brand & "(Postgres))" OBJECT
    cName    : TEXT;
    conn     : PGconn_star := NIL;
    res      : PGresult_star;
    errText  : TEXT;
    mu       : MUTEX;

    conninfo : TEXT;
    specifyingHost : BOOLEAN;

    reconnects : CARDINAL := MaxReconnects;
  METHODS
    myExec(query : char_star; busyWait : BOOLEAN) : PGresult_star 
      RAISES { DBerr.Error } := MyExec;
  OVERRIDES
    init       := InitNop;
    open       := OpenM;
    openRemote := OpenRemoteM;
    connect    := ConnectM;
    close      := CloseM;
    exec       := ExecM;
    execCA     := ExecCA;
    tExec      := TExecM;
    tExecCA    := TExecCAM;
    name       := GetName;
    isClosed   := IsClosed;
    getType    := GetTypeM;
  END;

PROCEDURE GetTypeM(<*UNUSED*>t : Postgres) : Type =
  BEGIN RETURN Type.PostgreSQL END GetTypeM;
  
PROCEDURE InitNop(t : Postgres) : T = BEGIN RETURN t END InitNop;
  
PROCEDURE IsClosed(t : Postgres) : BOOLEAN =
  BEGIN RETURN t.conn = NIL END IsClosed;

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
    t.conninfo := conninfo; (* remember for restart if desired *)
    t.specifyingHost := specifyingHost;

    IF NOT specifyingHost THEN
      WITH dbHost = Env.Get("DBHOST") DO
        IF dbHost # NIL THEN
          conninfo := conninfo & " host=" & dbHost
        END
      END
    END;

    WITH s = CopyTtoS(conninfo) DO
      TRY
        t.conn := PQconnectStart(s);

        IF (PQstatus(t.conn) = CONNECTION.BAD) THEN
          t.errText := CopyStoT(PQerrorMessage(t.conn));
          PQfinish (t.conn);
          t.conn := NIL;
          RAISE DBerr.Error(t.errText);
        END;  
        Debug.Out("Starting to open database connection \"" & 
          conninfo & "\" conn = 0x" &
          Fmt.Int(LOOPHOLE(t.conn,INTEGER),16));

        LOOP
          WITH socket = GetSocket(t), 
               poll = PQconnectPoll(t.conn) DO
            CASE poll OF 
              PGRES.POLLING_FAILED =>
                t.errText := CopyStoT(PQerrorMessage(t.conn));
                PQfinish (t.conn);
                t.conn := NIL;
                RAISE DBerr.Error(t.errText);
            |
              PGRES.POLLING_READING =>
                EVAL SchedulerPosix.IOWait(socket,TRUE)
            |
              PGRES.POLLING_WRITING =>
                EVAL SchedulerPosix.IOWait(socket,FALSE)
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

VAR failProb := 0.0d0;

BEGIN 
  WITH envFailProb = Env.Get("DBFAILPROBABILITY") DO
    IF envFailProb # NIL THEN
      TRY
        failProb := Scan.LongReal(envFailProb)
      EXCEPT
        Lex.Error, FloatMode.Trap =>
        Debug.Error("Please either unset DBFAILPROBABILITY or set it to a parseable number, not \"" & envFailProb & "\"")
      END
    END
  END
END UnsafeDatabase.
