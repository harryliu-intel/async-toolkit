UNSAFE MODULE UnixGroupList;
IMPORT M3toC;
IMPORT UnixGetIds;
FROM UnixGetIds IMPORT actual_gid_t;
IMPORT UnixGidSetDef;
IMPORT Utypes;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Ctypes;

CONST doDebug = TRUE;

PROCEDURE GetUid(user : TEXT) : Utypes.uid_t RAISES { NotFound } =
  VAR
    uid : Utypes.uid_t;
  BEGIN
    WITH res = Get(user, uid) DO
      IF res = NIL THEN RAISE NotFound END;
      RETURN uid
    END
  END GetUid;
  
PROCEDURE GetGids(user : TEXT) : REF ARRAY OF Utypes.gid_t RAISES { NotFound } =
  VAR
    uid : Utypes.uid_t;
    arr : REF ARRAY OF Utypes.gid_t;
  BEGIN
    WITH res = Get(user, uid) DO
      IF res = NIL THEN RAISE NotFound END;
      arr := NEW(REF ARRAY OF Utypes.gid_t, res.size());
      WITH iter = res.iterate() DO
        FOR i := 0 TO NUMBER(arr^) - 1 DO
          EVAL iter.next(arr[i])
        END
      END;
      RETURN arr
    END
  END GetGids;
  
PROCEDURE Get(user : TEXT; VAR uid : Utypes.uid_t) : T =
  VAR
    cuser := M3toC.CopyTtoS(user);
    pwd   := UnixGetIds.alloc_getpwnam(cuser);
    gid    : Utypes.gid_t;

    n      := 1;
    groups := NEW(UNTRACED REF ARRAY OF actual_gid_t, n);

    ngroups := NEW(UNTRACED REF Ctypes.int);
    res : INTEGER;

  BEGIN
    TRY
      IF pwd = NIL THEN
        RETURN NIL
      END;

      uid := UnixGetIds.extract_pwd_uid(pwd);
      gid := UnixGetIds.extract_pwd_gid(pwd);

      LOOP
        ngroups^ := n;
        res := UnixGetIds.getgrouplist(cuser, gid, ADR(groups[0]), ngroups);

        IF doDebug THEN
          Debug.Out(F("UnixGroupList.Get: n=%s ngroups^=%s res=%s",
                      Int(n), Int(ngroups^), Int(res)))
        END;
        
        IF res # -1 THEN
          <*ASSERT ngroups^ <= n*>
          EXIT
        ELSE
          DISPOSE(groups);
          n      := n * 2;
          groups := NEW(UNTRACED REF ARRAY OF actual_gid_t, n);
        END
      END;

      VAR
        set := NEW(UnixGidSetDef.T).init();
      BEGIN
        FOR i := 0 TO ngroups^ - 1 DO
          IF doDebug THEN
            Debug.Out(F("UnixGroupList.Get: groups[%s]=%s",
                        Int(i),
                        Int(groups[i])))
          END;
          EVAL set.insert(groups[i])
        END;

        RETURN set
      END
      
    FINALLY
      M3toC.FreeCopiedS(cuser);
      DISPOSE(groups);
      DISPOSE(ngroups);
    END
  END Get;

BEGIN END UnixGroupList.
