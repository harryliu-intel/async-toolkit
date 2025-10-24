(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

MODULE Session;
IMPORT DatabaseClass; (* debug only *)
IMPORT Random,Text;
IMPORT Pages;
IMPORT Database, DatabaseUtils, Scan, Fmt;
FROM Fmt IMPORT F, Int;
IMPORT Debug;
IMPORT DBerr;
IMPORT Pathname, Params;

IMPORT FloatMode, Lex; (* exceptions req'd *)

REVEAL 
  T = Public BRANDED "HTML Session" OBJECT
    userId := -1;              (* an illegal value *)
    hostAddr : TEXT := NIL;
    key : TEXT := NIL;
    priv : Pages.Priv;
  OVERRIDES
    getPriv := GetPriv;
    getUser := GetUser;
    getUserName := GetUserName;
    getUserInfo := GetUserInfo;
    getId := GetId;
    init := Init;
  END;

CONST
  ExpiryHours = 1;

PROCEDURE GetPriv(self : T) : Pages.Priv =
  BEGIN RETURN self.priv END GetPriv;

PROCEDURE GetId(self:T) : TEXT =
  BEGIN RETURN self.key END GetId;

PROCEDURE GetUser(self : T) : CARDINAL =
  BEGIN RETURN self.userId END GetUser;

VAR random := NEW(Random.Default).init(fixed := FALSE);
VAR CGIname := Pathname.Last(Params.Get(0));

PROCEDURE RandomString(len : CARDINAL := 20) : TEXT =
  VAR
    res := NEW(REF ARRAY OF CHAR, len);
  BEGIN
    FOR i := 0 TO len - 1 DO
      res[i] := VAL(random.integer(ORD('A'), ORD('Z')), CHAR);
    END;
    RETURN Text.FromChars(res^)
  END RandomString;

PROCEDURE Init(self     : T;
               userId   : UserId;
               hostAddr : TEXT;
               multiOk  : BOOLEAN) : T 
  RAISES { DBerr.Error } =
  BEGIN 
    <* ASSERT self.userId < 0 AND self.hostAddr = NIL *>
    self.userId := userId; 
    self.hostAddr := DatabaseUtils.Sanitize(hostAddr); 
    self.key := RandomString();

    (* set privileges ?? *)

    (* we should delete all old sessions for this user *)
    EVAL Database.Exec("BEGIN");
    VAR
      delQ := "DELETE FROM session_tbl WHERE expires < 'now'";
    BEGIN
      IF NOT multiOk THEN
        delQ := delQ & " OR uid = " & Int(self.userId)
      END;
      EVAL Database.Exec(delQ)
    END;

    (* at this point, we need to insert ourselves in the database ... *)
      EVAL Database.Exec(F(
     "INSERT INTO session_tbl (uid, session_key, host_ip, expires) values "&
                            " (%s, '%s', '%s', %s)",
                           Int(self.userId),
                           self.key,
                           self.hostAddr,
                           ExpiryString()));
    
    EVAL Database.Exec("COMMIT WORK");
    RETURN self 
  END Init;

PROCEDURE ExpiryString() : TEXT =
  BEGIN
    CASE Database.GetType() OF
      Database.Type.PostgreSQL =>
      RETURN F(" 'now'::timestamp with time zone + '%s hour'::interval)",
               Int(ExpiryHours))
    |
      Database.Type.MySQL =>
      RETURN F("date_add(now(), interval %s hour)", Int(ExpiryHours))
    END
  END ExpiryString;

PROCEDURE GetUserName(self : T) : TEXT RAISES { DBerr.Error } =
  BEGIN RETURN GetUserInfo(self,"name") END GetUserName;

(* this stuff should be memoized *)
PROCEDURE GetUserInfo(self : T; infoName : TEXT) : TEXT 
  RAISES { DBerr.Error } =
  VAR
    sanInfoName := DatabaseUtils.Sanitize(infoName);
    queryResult := NEW(Database.Table).init(Database.Exec("SELECT " & 
                       sanInfoName & " FROM userinfo " &
                       "WHERE uid = " & Fmt.Int(self.getUser())));
    res := queryResult.getUniqueEntry(sanInfoName);
  BEGIN RETURN res END GetUserInfo;

PROCEDURE Validate(hostAddr, key : TEXT; updateExpiry : BOOLEAN) : T
  RAISES { DBerr.Error } =
  <* FATAL FloatMode.Trap, Lex.Error *>
  VAR
    res := NEW(T);
    searchKey := DatabaseUtils.Sanitize(key);
    queryResult := NEW(Database.Table).init(Database.Exec("SELECT u.status, s.uid, s.host_ip FROM session_tbl s, userinfo u " &
                       "WHERE s.expires > 'now' AND " & 
                       "u.uid = s.uid AND " & 
                       "s.session_key = '" & searchKey & "'"));
  BEGIN 
    <* ASSERT hostAddr # NIL *>
    <* ASSERT key # NIL *>
    Debug.Out("Attempting to validate (hostAddr,key) = (" & 
      hostAddr & "," & key & ")");

    (* if queryResult doesnt have exactly one row, something is wrong *)
    (* ASSERT that we have less than two rows (else database integrity 
       is shot. *)

    (* check that we have one row, else its time for an HTML.Error *)

    (* validate result value---check that the hostAddr matches and that
       the session hasnt expired (perhaps ought to be done in the SQL query
       itself) *)

    IF queryResult = NIL THEN
      Debug.Out("NIL queryResult")
    ELSE
      Debug.Out(F("non-NIL queryResult fields %s data %s",
                  Int(NUMBER(queryResult.fieldNames^)),
                  Int(NUMBER(queryResult.data^))))
    END;

    TRY
      VAR
        queryHostIP := queryResult.getUniqueEntry("host_ip");
      BEGIN
        
        Debug.Out("queryHostIP = " & Debug.UnNil(queryHostIP) & ", " &
          "hostAddr = " & Debug.UnNil(hostAddr));
        IF NOT Text.Equal(queryHostIP,
                          hostAddr) THEN RETURN NIL END
      END
    EXCEPT DBerr.Error(e) =>
      (* ok this stuff shoudlnt be hard-coded... *)
      RAISE DBerr.Error("Session key lookup failed with error:<br> " & e &
            "<br><br><br><br>Your session may have expired.  Please re-register.<br>" & "<a href=\"" & CGIname & "?signin\">Signin page.</a>" );
    END;
      

    (* alls well, we need to set up the result value and return it *)
    res.userId := Scan.Int(queryResult.getUniqueEntry("uid"));
    res.key := key;

    (* and extend validity of session *)
    IF updateExpiry THEN
      EVAL Database.TExec(F(
   "UPDATE session_tbl SET expires=%s WHERE session_key='%s'",
      ExpiryString(), key
      ))
    END;

    (* add code to parse privilege level *)
    VAR
      privName := queryResult.getUniqueEntry("status");
    BEGIN
      <* ASSERT privName # NIL *>
      FOR i := FIRST(Priv) TO LAST(Priv) DO
        IF Text.Equal(PrivNames[i],privName) THEN
          res.priv := i;
          EXIT
        END;
        <* ASSERT i # LAST(Priv) *> (* should never get here *)
      END
    END;

    res.hostAddr := hostAddr;

    RETURN res
  END Validate;

BEGIN END Session.
