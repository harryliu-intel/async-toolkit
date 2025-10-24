(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

MODULE Main;
IMPORT Params, Pathname;
IMPORT Database, HTML, Text;
IMPORT HTMLOutput,HTMLList;
IMPORT Scan;
IMPORT Rd, Stdio, Fmt;
IMPORT Debug;
IMPORT TextTable;
IMPORT Session;
IMPORT DBerr;
IMPORT Pages;
IMPORT URL, TextUtils;
IMPORT FloatMode, Lex, Thread; (* for various exceptions *)
IMPORT Request;
IMPORT Env;
IMPORT Process;
IMPORT GCSite;
IMPORT Wr;

(* this file should be split into many things:
   -- URL processing
   -- random string processing
   -- environment variable handling...
   -- generic (key, value) pair table handling
   ...
*)

CONST VerboseDebug = TRUE;

VAR myID := Process.GetMyID();

PROCEDURE ParseEnv() : TextTable.T =
  VAR
    count := Env.Count;
    res :=  NEW(TextTable.T).init();
  BEGIN
    IF VerboseDebug THEN
      <*FATAL Wr.Failure*>
      BEGIN
        Wr.PutText(Stdio.stderr, Fmt.Int(myID) & " STARTPAGE:\n")
      END
    END;
    FOR i := 0 TO count - 1 DO
      VAR 
        key, value : TEXT;
      BEGIN
        Env.GetNth(i,key,value);
        IF value = NIL THEN value := "" END;
        EVAL res.put(key,value);
        IF VerboseDebug THEN
          <*FATAL Wr.Failure*>
          BEGIN
            Wr.PutText(Stdio.stderr,Fmt.Int(myID) & " PAGEDATAKEY " & key & " PAGEDATAVALUE " & value & " ENDPAGEDATA;\n")
          END
        END
      END
    END;
    RETURN res
  END ParseEnv;

PROCEDURE ParseQuestionMarkData(data : TEXT) : TextTable.T 
  RAISES { FloatMode.Trap, Lex.Error } =
  VAR
    res := NEW(TextTable.T).init();
  BEGIN
    IF data # NIL THEN
      LOOP
        VAR 
          this, name, value, newdata : TEXT;
        BEGIN
          
          TextUtils.SplitText(data, '&', this, newdata);
          data := newdata;
          this := URL.PlusToSpace(this);
          this := URL.Unescape(this);
          TextUtils.SplitText(this,'=', name, value);

          VAR
            res := "";
          BEGIN
            Debug.Out("value:");
            IF value = NIL THEN value := "" 
            ELSE
              FOR i := 0 TO Text.Length(value) - 1 DO
                res := res & Fmt.Int(ORD(Text.GetChar(value,i)))& ", ";
              END;
            END;
            Debug.Out(res);
          END;

          EVAL res.put(name, value); (* must return false? *)
          IF data = NIL THEN EXIT END
        END
      END
    END;
    RETURN res
  END ParseQuestionMarkData;

(**********************************************************************)

(**********************************************************************)

<* FATAL Rd.Failure, Thread.Alerted *>
VAR
  res : Database.Result;
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

  inputData := Rd.GetText(Stdio.stdin,contentLength);
  debugStuff.add("<p><p><hr>Reading from Stdin:<p>" & inputData &
    "\n<p><hr>");

  IF VerboseDebug THEN
    <*FATAL Wr.Failure*>
    BEGIN
      Wr.PutText(Stdio.stderr,Fmt.Int(myID) & " PAGEINPUT " & inputData & " ENDPAGEINPUT;\n"&Fmt.Int(myID)&" ENDPAGE.\n")
    END
  END;

  TRY
    postVars := ParseQuestionMarkData(inputData); 
  EXCEPT FloatMode.Trap, Lex.Error =>
    HTML.Error("Invalid POST data received from browser.",TRUE )
  END;
    

  (* must have postVars and envVars before this *)
  request := NEW(Request.T).init(envVars := envVars, postVars := postVars);
  TextUtils.SplitText(queryString, '?', request.toPage, request.fromPage);
  TextUtils.SplitText(request.fromPage, '?', request.fromPage, key);
  TextUtils.SplitText(key, '?', key, getData);

  debugStuff.add("<p>toPage: " & Debug.UnNil(request.toPage) & "<br>" &
    "<p>fromPage: " & Debug.UnNil(request.fromPage) & "<br>" &
    "<p>key: " & Debug.UnNil(key) & "<br>");
  debugStuff.add("<p>getData: " & Debug.UnNil(getData) & "<br>");

  TRY
    getVars := ParseQuestionMarkData(getData);
  EXCEPT FloatMode.Trap, Lex.Error =>
    HTML.Error("Invalid GET data received from browser.",TRUE )
  END;
    
  TRY
    Debug.Out("Opening database");
    (*Database.Open("vf");*)
    
    Database.OpenRemote("cam.async.caltech.edu",
                        NIL,
                        dbName,
                        "trading",
                        "sharpe3");
    Debug.Out("Running sample query");
    res := Database.Exec("SELECT * FROM userinfo");
    table := NEW(Database.Table).init(res);

    GCSite.InitDB();
    IF Env.Get("NORECORD") = NIL THEN
      TRY
        GCSite.InitPW("cam.async.caltech.edu",dbName)
      EXCEPT
        DBerr.Error =>
          HTML.Error("Unable to initialize connection to pricewatch DB.")
      END
    END;

    IF key # NIL THEN 
      VAR 
        remoteAddr := Env.Get("REMOTE_ADDR");
      BEGIN
        IF remoteAddr = NIL THEN
          Process.Crash("??? REMOTE_ADDR env variable is NIL ???")
        END;
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

