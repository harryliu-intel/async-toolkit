MODULE UnsafeDatabaseMySQL EXPORTS Database;

(* 
   An implementation of Database.i3 using MySQL/MariaDB bindings.

   Author: Mika Nystrom <mika@alum.mit.edu>
   April, 2021
*)
IMPORT DatabaseClass;

IMPORT MySQL AS Impl;
IMPORT MySQLMaps AS ImplMaps;
IMPORT DBerr;
IMPORT Scan, Lex, FloatMode;
IMPORT Env;
IMPORT TextReader;
IMPORT Text;

IMPORT Fmt;
FROM Fmt IMPORT FN, F, Int;

IMPORT Debug;
FROM Debug IMPORT UnNil;
IMPORT DatabaseVector, DatabaseVectorSeq;

CONST doDebug = TRUE;

CONST TE = Text.Equal;

TYPE
  MySQLResult = Result BRANDED "MySQL Query Result" OBJECT
    tab : Table;
  OVERRIDES
    initTable := InitTable;
  END;

PROCEDURE InitTable(res : MySQLResult; tab : Table) : Table =
  BEGIN
    tab.fieldNames := res.tab.fieldNames;
    tab.data       := res.tab.data;
    RETURN tab
  END InitTable;

REVEAL 
  MySQL = T BRANDED "(" & Brand & "(MySQL))" OBJECT
    conn           : Impl.T := NIL;
    cName          : TEXT;

    conninfo       : TEXT;
    specifyingHost : BOOLEAN;

    mu             : MUTEX;
    
  OVERRIDES
    init       := Init;
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
  END;

PROCEDURE Init(t : MySQL) : T
  RAISES { DBerr.Error } =
  BEGIN
    TRY
      t.conn := Impl.Init(NIL)
    EXCEPT
      Impl.ConnE =>
      RAISE DBerr.Error("UnsafeDatabaseMySQL.Init: MySQL connection failed : " & Impl.Error(t.conn))
    END;
    RETURN t
  END Init;

PROCEDURE OpenM(t : T; dbTextName : TEXT) RAISES { DBerr.Error } =
  (* could be in a higher-level object type, shared with PGSQL *)
  BEGIN
    t.openRemote(NIL, NIL, dbTextName, NIL, NIL)
  END OpenM;

PROCEDURE OpenRemoteImpl(t                                      : MySQL;
                         dbHost, dbPort, dbName, dbLogin, dbPwd : TEXT)
  RAISES { DBerr.Error } =
  CONST
    DefaultPort = 0;
  VAR
    port : CARDINAL := DefaultPort;
  BEGIN
    IF dbHost = NIL THEN
      dbHost := Env.Get("DBHOST")
    END;
    TRY
      IF dbPort # NIL THEN
        TRY
          WITH q = Scan.Int(dbPort) DO
            IF q < FIRST(CARDINAL) OR q > LAST(CARDINAL) THEN
              RAISE DBerr.Error("port out of range")
            ELSE
              port := q
            END
          END
        EXCEPT
          Lex.Error, FloatMode.Trap =>
          RAISE DBerr.Error("attempting to connect to non-integer port")
        END
      END;

      IF doDebug THEN
        Debug.Out(FN("UnsafeDatabaseMySQL.OpenRemoteImpl: dbHost=\"%s\" dbLogin=\"%s\" dbPwd=\"%s\" dbName=\"%s\" port=%s",
                     ARRAY OF TEXT { UnNil(dbHost),
                                     UnNil(dbLogin),
                                     UnNil(dbPwd),
                                     UnNil(dbName),
                                     Int(port) }));
      END;
      
      WITH res = Impl.RealConnect(t.conn,
                                  dbHost,
                                  dbLogin,
                                  dbPwd,
                                  dbName,
                                  port,
                                  NIL,
                                  0
        ) DO

        (* NIL res means we should get an exception below vvv *)
        <*ASSERT res # NIL*>
      END
    EXCEPT
      Impl.ConnE =>
      RAISE DBerr.Error("MySQL connection failed : " & Impl.Error(t.conn))
    END
  END OpenRemoteImpl;

PROCEDURE OpenRemoteM(t                                      : MySQL;
                      dbHost, dbPort, dbName, dbLogin, dbPwd : TEXT)
  RAISES { DBerr.Error } = 
  (* identical to postgres code, share? *)
  VAR
    connstr := "";
  BEGIN
    Debug.Out("UnsafeDatabaseMySQL.OpenRemoteM");
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

PROCEDURE ConnectM(t              : MySQL; 
                   conninfo       : TEXT; 
                   specifyingHost : BOOLEAN)
  RAISES { DBerr.Error } =
  VAR
    dbHost, dbPort, dbName, dbLogin, dbPwd : TEXT := NIL;
    reader := NEW(TextReader.T).init(conninfo);
    chunk : TEXT;
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

    Debug.Out(F("UnsafeDatabaseMySQL.ConnectM : conninfo=\"%s\"",
                UnNil(conninfo)));

    WHILE reader.next(" \t", chunk, skipNulls := FALSE) DO
      WITH eqi = Text.FindChar(chunk, '=') DO
        IF eqi # -1 THEN
          Debug.Out(F("UnsafeDatabaseMySQL.ConnectM : processing chunk \"%s\"",
                      chunk));
          WITH key = Text.Sub(chunk, 0, eqi),
               len = Text.Length(chunk),
               val = Text.Sub(chunk, eqi + 1, len) DO

            Debug.Out(F("UnsafeDatabaseMySQL.ConnectM : chunk key=\"%s\" val=\"%s\"",
                        key, val));
            
            IF    TE(key, "host") THEN
              dbHost := val
            ELSIF TE(key, "port") THEN
              dbPort := val
            ELSIF TE(key, "dbname") THEN
              dbName := val
            ELSIF TE(key, "user") THEN
              dbLogin := val
            ELSIF TE(key, "password") THEN
              dbPwd := val
            ELSE
              (*skip*)
            END
          END
        END
      END
    END;
    OpenRemoteImpl(t, dbHost, dbPort, dbName, dbLogin, dbPwd)
  END ConnectM;

PROCEDURE CloseM(t : MySQL) =
  BEGIN
    Impl.Close(t.conn);
    t.conn := NIL
  END CloseM;

PROCEDURE ExecM(t                               : MySQL; 
                query                           : TEXT; 
                busyWait, abortConnectionOnFail : BOOLEAN) : Result  (*??*)
  RAISES { DBerr.Error } =
  VAR
    result : MySQLResult;
    rowSeq : DatabaseVectorSeq.T;
  BEGIN
    LOCK t.mu DO
      TRY
        VAR
          resi := Impl.Query(t.conn, query);
          res  := Impl.UseResult(t.conn);
          row : REF ARRAY OF TEXT;
          lengths : Impl.RefLengthsT;
          nfields := Impl.NumFields(res);
        BEGIN
          IF resi # 0 THEN
            RAISE DBerr.Error("Query returned non-zero error: " & Fmt.Int(resi))
          END;

          result := NEW(MySQLResult, tab := NEW(Table));
          
          WITH fields  = ImplMaps.FieldList(res),
               fa      = NEW(Vector, nfields)
           DO
            IF doDebug THEN
              Debug.Out(F("UnsafeDatabaseMySQL.ExecM: query return nfields %s",
                          Int(nfields)))
            END;
            
            FOR j := FIRST(fa^) TO LAST(fa^) DO
              IF doDebug THEN
                Debug.Out(F("UnsafeDatabaseMySQL.ExecM: field[%s] : %s",
                            Int(j), fields[j].name))
              END;
              fa[j] := fields[j].name
            END;
            result.tab.fieldNames := fa;

            rowSeq := NEW(DatabaseVectorSeq.T).init();

          END;

          row := Impl.FetchRow(res);
          WHILE row # NIL DO

            WITH vec = NEW(Vector, NUMBER(row^)) DO
              FOR j := FIRST(row^) TO LAST(row^) DO
                IF doDebug THEN
                  Debug.Out(F("UnsafeDatabaseMySQL.ExecM: data[%s] : %s",
                              Int(j), row[j]))
                END;
                vec[j] := row[j]
              END;
            END;

            row     := Impl.FetchRow(res);

          END;
          Impl.FreeResult(res);

          result.tab.data := NEW(Matrix, rowSeq.size(), nfields);
          FOR i := 0 TO rowSeq.size() - 1 DO
            result.tab.data[i] := rowSeq.get(i)^
          END;
          RETURN result
        END
      EXCEPT
        Impl.ResultE => 
        RAISE DBerr.Error("Result Error: num " & Fmt.Int(Impl.Errno(t.conn)) & " " & Impl.Error(t.conn) & "\n");
       | Impl.ReturnE =>
         RAISE DBerr.Error("Return Error: num " & Fmt.Int(Impl.Errno(t.conn)) & " " & Impl.Error(t.conn) & "\n"); 
      END
    END
  END ExecM;
  
PROCEDURE ExecCA(t                               : MySQL; 
                 VAR arr                         : ARRAY OF CHAR; 
                 busyWait, abortConnectionOnFail : BOOLEAN) : Result 
  RAISES { DBerr.Error } =
  VAR
    txt := Text.FromChars(arr);
  BEGIN
    RETURN ExecM(t, txt, busyWait, abortConnectionOnFail)
  END ExecCA;

PROCEDURE TExecM(t                               : T; 
                 query                           : TEXT; 
                 busyWait, abortConnectionOnFail : BOOLEAN) : Table 
  RAISES { DBerr.Error } =
  (* not special for MySQL? *)
  VAR
    res := t.exec(query,busyWait,abortConnectionOnFail);
  BEGIN
    RETURN NEW(Table).init(res) 
  END TExecM;

PROCEDURE TExecCAM(t                               : T; 
                   VAR query                       : ARRAY OF CHAR; 
                   busyWait, abortConnectionOnFail : BOOLEAN) : Table 
  RAISES { DBerr.Error } =
  (* not special for MySQL? *)
  VAR
    res := t.execCA(query,busyWait,abortConnectionOnFail);
  BEGIN
    RETURN NEW(Table).init(res) 
  END TExecCAM;
  
PROCEDURE GetName(t : MySQL) : TEXT =
  (* could be shared *)
  BEGIN 
    IF t.cName = NIL THEN
      RETURN "(NIL)"
    ELSE
      RETURN t.cName 
    END
  END GetName;

PROCEDURE IsClosed(t : MySQL) : BOOLEAN =
  (* could be shared? *)
  BEGIN
    RETURN t.conn = NIL
  END IsClosed;


BEGIN
END UnsafeDatabaseMySQL.
