INTERFACE UnixPermissions;
IMPORT Pathname;
IMPORT Utypes;
IMPORT OSError;

TYPE
  PT = { R, W, X };

  P = SET OF PT;

  R = { Owner, Group, Other };

  Perms = ARRAY R OF P;

  Type = { Regular, Directory, Unknown };
  
  T = RECORD
    path     : Pathname.T;
    realPath : Pathname.T;
    uid      : Utypes.uid_t;
    gid      : Utypes.gid_t;
    perms    : Perms;
    type     : Type;
  END;

PROCEDURE Get(path : Pathname.T) : T
  RAISES { OSError.E };
  
CONST Brand = "UnixPermissions";

END UnixPermissions.
