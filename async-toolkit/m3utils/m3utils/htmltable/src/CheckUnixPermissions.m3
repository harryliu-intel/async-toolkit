MODULE CheckUnixPermissions;
IMPORT Utypes;
IMPORT Pathname;
IMPORT Debug;
IMPORT UnixGroupList;
IMPORT TextSeq;
FROM Fmt IMPORT F;
IMPORT OSError;
IMPORT UnixPermissions;
FROM UnixPermissions IMPORT R, PT, Type;
IMPORT FS;

PROCEDURE Check(user : TEXT; path : Pathname.T) : BOOLEAN
  RAISES { Pathname.Invalid } =
  BEGIN
    RETURN GetReason(user, path).result
  END Check;

PROCEDURE GetReasonImpl(user : TEXT; path : Pathname.T) : Reason
  RAISES { Pathname.Invalid } =
  VAR
    arcs := Pathname.Decompose(path);
    uid : Utypes.uid_t;
    gidSet := UnixGroupList.Get(user, uid);
    needPerm : UnixPermissions.PT;

  PROCEDURE Fail(p : Pathname.T) : Reason =
    BEGIN
      RETURN Reason { FALSE, p, NIL }
    END Fail;

  PROCEDURE Succeed(p : Pathname.T) : Reason =
    BEGIN
      RETURN Reason { TRUE, p, NIL }
    END Succeed;

  BEGIN
    FOR i := 1 TO arcs.size() DO

      TRY
        WITH thisPath = Pathname.Compose(TextSeq.Sub(arcs, 0, i)) DO
          Debug.Out(F("Checking path \"%s\"", thisPath));
          WITH perms    = UnixPermissions.Get(thisPath)   DO
            (* if it is a directory, we need execute permission to skip it,
               but read permission to list it.

               so-- we need to treat directories differently if they are
               the final arc vs. middle arcs
            *)
            
            IF perms.type = Type.Directory AND i # arcs.size() THEN
              needPerm := PT.X
            ELSE
              needPerm := PT.R
            END;

            (* there are some subtleties in permissions! *)
            
            IF gidSet # NIL AND uid = perms.uid THEN
              (* I am owner *)
              IF    NOT needPerm IN perms.perms[R.Owner] THEN
                (* owner fail *)
                RETURN Fail(thisPath)
              ELSIF NOT needPerm IN perms.perms[R.Group] AND
                    gidSet.member(perms.gid) THEN
                (* I am owner, AND I am member of file group, no owner fail,
                   and group fail *)
                RETURN Fail(thisPath)
              ELSE
                (* skip *)
              END
            ELSIF gidSet # NIL AND gidSet.member(perms.gid) THEN
              (* group member, but not owner *)
              IF needPerm IN perms.perms[R.Group] THEN
                (* skip *)
              ELSE
                RETURN Fail(thisPath)
              END
            ELSE
              (* I am not owner and I do not have group membership *)
              IF needPerm IN perms.perms[R.Other] THEN
                (* skip *)
              ELSE
                RETURN Fail(thisPath)
              END
            END
          END
        END;

      EXCEPT
        OSError.E(x) => RETURN Reason { FALSE, path, x }
      END
    END;
    RETURN Succeed(path)
  END GetReasonImpl;

PROCEDURE GetReason(user : TEXT; path : Pathname.T) : Reason
  RAISES { Pathname.Invalid } =
  VAR
    realPath : Pathname.T;
    reason : Reason;
  BEGIN
    TRY
      realPath := FS.GetAbsolutePathname(path)
    EXCEPT
      OSError.E(x) => RETURN Reason { FALSE, path, x }
    END;
    reason := GetReasonImpl(user, path);
    IF NOT reason.result THEN RETURN reason END;
    reason := GetReasonImpl(user, realPath);
    RETURN reason
  END GetReason;
  
BEGIN END CheckUnixPermissions.
