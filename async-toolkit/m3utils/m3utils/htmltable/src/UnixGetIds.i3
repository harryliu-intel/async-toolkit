UNSAFE INTERFACE UnixGetIds;
IMPORT Utypes;
IMPORT Ctypes;

TYPE
  Opaque = RECORD END;

  T = UNTRACED BRANDED REF Opaque;

<*EXTERNAL UnixGetIds__extract_pwd_uid*>
PROCEDURE extract_pwd_uid(pwd : T) : Utypes.uid_t;

<*EXTERNAL UnixGetIds__extract_pwd_gid*>
PROCEDURE extract_pwd_gid(pwd : T) : Utypes.gid_t;

<*EXTERNAL UnixGetIds__alloc_getpwnam*>
PROCEDURE alloc_getpwnam(name : Ctypes.const_char_star) : T;

<*EXTERNAL UnixGetIds__free_pwd*>
PROCEDURE free_pwd(pwd : T);

TYPE
  actual_gid_t = Utypes.uint32_t; (* this is right for AMD64_LINUX *)
  
<*EXTERNAL*>
PROCEDURE getgrouplist(user    : Ctypes.const_char_star;
                       group   : Utypes.gid_t;
                       groups  : UNTRACED REF actual_gid_t;
                       ngroups : UNTRACED REF Ctypes.int) : Ctypes.int;

END UnixGetIds.
