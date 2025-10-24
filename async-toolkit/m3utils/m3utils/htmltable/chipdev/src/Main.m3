(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT Params, Pathname;
IMPORT Database, HTML;
IMPORT HTMLOutput,HTMLList;
IMPORT Scan;
IMPORT Rd, Stdio, Fmt;
IMPORT Debug;
IMPORT TextTable;
IMPORT Session;
IMPORT DBerr;
IMPORT Pages;
IMPORT TextUtils;
IMPORT FloatMode, Lex, Thread; (* for various exceptions *)
IMPORT Request;
IMPORT Env;
IMPORT Process;
IMPORT Chipdev;
IMPORT Wr;
IMPORT DbSettings;
FROM WebAppUtils IMPORT ParseEnv, ParseQuestionMarkData, VerboseDebug;

(* this file should be split into many things:
   -- URL processing
   -- random string processing
   -- environment variable handling...
   -- generic (key, value) pair table handling
   ...
*)
(**********************************************************************)

(**********************************************************************)

VAR myID : INTEGER;

<* FATAL Rd.Failure, Thread.Alerted *>
VAR
  res   : Database.Result;
  table : Database.Table;
  envVars := ParseEnv();
  debugStuff := NEW(HTMLList.T);
  contentLength : CARDINAL;
  queryString := Env.Get("QUERY_STRING");
  inputData : TEXT;
  postVars,getVars : TextTable.T;
  request : Request.T; 
  key : TEXT;
  getData : TEXT;
  dbName := Pathname.LastBase(Params.Get(0));
BEGIN 
  debugStuff.add("<br><br><hr>--- DEBUGGING INFORMATION BELOW THIS LINE, PLEASE PAY NO ATTENTION ---<br>");

  IF VerboseDebug THEN myID := Process.GetMyID() END;
  
  TRY
    WITH c = Env.Get("CONTENT_LENGTH") DO
      IF c = NIL THEN
        contentLength := 0 
      ELSE
        contentLength := Scan.Int(c)
      END
    END
  EXCEPT FloatMode.Trap, Lex.Error => 
    HTML.Error("Problem scanning CONTENT_LENGTH!", TRUE)
  END;

  inputData := Rd.GetText(Stdio.stdin, contentLength);
  debugStuff.add("<p><p><hr>Reading from Stdin:<p>" & inputData &
    "\n<p><hr>");

  IF VerboseDebug THEN
    <*FATAL Wr.Failure*>
    BEGIN
      Wr.PutText(Stdio.stderr,
                 Fmt.Int(myID) & " PAGEINPUT " & inputData & " ENDPAGEINPUT;\n"&Fmt.Int(myID)&" ENDPAGE.\n");

      Wr.PutText(Stdio.stderr,
                 Fmt.Int(myID) & " QUERYSTRING " & Debug.UnNil(queryString) & " ENDQUERYSTRING;\n"&Fmt.Int(myID)&" ENDPAGE.\n");
      
    END
  END;

  TRY
    postVars := ParseQuestionMarkData(inputData); 
  EXCEPT FloatMode.Trap, Lex.Error =>
    HTML.Error("Invalid POST data received from browser.",TRUE )
  END;


  (* must have postVars and envVars and getVars before this *)
  request := NEW(Request.T).init(envVars  := envVars,
                                 postVars := postVars,
                                 getVars  := getVars);
  TextUtils.SplitText(queryString, '?', request.toPage, request.fromPage);
  TextUtils.SplitText(request.fromPage, '?', request.fromPage, key);
  TextUtils.SplitText(key, '?', key, getData);

  debugStuff.add(
      "<p>toPage:   " & Debug.UnNil(request.toPage) & "<br>" &
      "<p>fromPage: " & Debug.UnNil(request.fromPage) & "<br>" &
      "<p>key:      " & Debug.UnNil(key) & "<br>"
  );
  debugStuff.add(
      "<p>getData:  " & Debug.UnNil(getData) & "<br>");

  TRY
    getVars := ParseQuestionMarkData(getData);
  EXCEPT FloatMode.Trap, Lex.Error =>
    HTML.Error("Invalid GET data received from browser.",TRUE )
  END;

  EVAL request.init(envVars  := envVars,
                    postVars := postVars,
                    getVars  := getVars); (* XXX hack *)
  
  TRY
    Debug.Out("Opening database");

    VAR
      conn : Database.T;
    BEGIN
      CASE DbSettings.Type OF
        Database.Type.MySQL =>
        conn := NEW(Database.MySQL).init();
      |
        Database.Type.PostgreSQL =>
        conn := NEW(Database.Postgres);
      END;

      Database.SetStatic(conn)
    END;

    
    Database.OpenRemote(DbSettings.Hostname,
                        DbSettings.Port,
                        DbSettings.Name,
                        DbSettings.User,
                        DbSettings.Passwd);

    Chipdev.InitDB();

    VAR 
      remoteAddr := Env.Get("REMOTE_ADDR");
    BEGIN
      IF remoteAddr = NIL THEN
        Process.Crash("??? REMOTE_ADDR env variable is NIL ???")
      END;
      IF key = NIL THEN
        request.session := NEW(Session.T).init(0, remoteAddr, multiOk := TRUE)
        (* we allow multiple sessions for this user *)
      ELSE
        request.session := Session.Validate(remoteAddr,key)
      END
    END;

    debugStuff.add("<p><p><hr>Environment vars:<p>");
    debugStuff.add(envVars.toHTML());

    debugStuff.add("<p><p><hr>POST vars:<p>");
    debugStuff.add(postVars.toHTML());
    
    debugStuff.add("<p><p><hr>GET vars:<p>");
    debugStuff.add(getVars.toHTML());


    HTMLOutput.SetFooter(debugStuff);
    (* ship it out---last thing we do. *)
    TRY
      HTMLOutput.Ship(Pages.Dispatch(request));
    EXCEPT
      Pages.NotFound =>
      IF request.toPage = NIL THEN request.toPage := "(null)" END;
      HTML.Error("Sorry, we could not find the page \"" &
        request.toPage & "\" that you requested.<br>")
    END;

    (* close database connection *)
    Database.Close();

  EXCEPT
    DBerr.Error(errstr) => HTML.Error("Fatal Database error: " & errstr)
  END
END Main.

