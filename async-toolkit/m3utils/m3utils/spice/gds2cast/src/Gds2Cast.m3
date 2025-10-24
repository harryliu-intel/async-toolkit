(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Gds2Cast;
IMPORT RAC;
IMPORT MyWx AS Wx;
    
(*

/*** translate from GDS2 name to CAST name and strip v(.) ***/
char *gds2cast(char *name) {
  if (translate) {
    int strip=0;
    char *s=name, *t=name;
    if ( *s=='\'') { s++; strip++; }
    if (!strncasecmp(s,"v(",2)) { s+=2; strip++; }
    while( *s) {
      if      (!strncmp(s,"_D_",3))   { s+=3; *(t++) = '.'; }
      else if (!strncmp(s,"_l_",3))   { s+=3; *(t++) = '['; }
      else if (!strncmp(s,"_r_",3))   { s+=3; *(t++) = ']'; }
      else if (!strncmp(s,"_C_",3))   { s+=3; *(t++) = ','; }
      else if (!strncmp(s,"_U_",3))   { s+=3; *(t++) = '_'; }
      else if (!strncmp(s,"_3a_",4))  { s+=4; *(t++) = ':'; }
      else                            { *(t++) = *(s++); }
    }
    *(t-strip)=0;
  }
  return name;
}

*)

PROCEDURE Gds2CastInt(name : RAC.T; wx : Wx.T) =
  CONST
    SQ = '\'';
  TYPE
    M = RECORD t : RAC.T; c : CHAR; n := LAST(CARDINAL) END;
  CONST
    R = RAC.FromText;
  VAR 
    ToConv:= ARRAY [0..5] OF M {
              M { R("_D_") , '.', 3 },
              M { R("_l_") , '[', 3 },
              M { R("_r_") , ']', 3 },
              M { R("_C_") , ',', 3 },
              M { R("_U_") , '_', 3 },
              M { R("_3a_"), ':', 4 }
             };

  VAR
    n, i  : CARDINAL;
    mapped : BOOLEAN;
  BEGIN
    Wx.Reset(wx);
    
    IF name[0] = SQ THEN
      name := RAC.Sub(name, 1, NUMBER(name^) - 2)
    END;

    n := NUMBER(name^);

    i := 0;
    WHILE i < n DO
      mapped := FALSE;
      FOR m := FIRST(ToConv) TO LAST(ToConv) DO
       WITH tr = ToConv[m] DO
         IF RAC.SubIs(name, tr.t, i, tr.n) THEN
           Wx.PutChar(wx, tr.c);
           INC(i, tr.n);
           mapped := TRUE;
           EXIT
         END
       END
      END;
      IF NOT mapped THEN              
        Wx.PutChar(wx, name[i]);
        INC(i)
      END
    END
  END Gds2CastInt;

PROCEDURE Gds2Cast(nm : TEXT) : TEXT =
  (* for external use *)
  VAR
    wx := Wx.New();
  BEGIN
    WITH rac = RAC.FromText(nm) DO
      Gds2CastInt(rac, wx)
    END;
    RETURN Wx.ToText(wx)
  END Gds2Cast;

PROCEDURE Gds2CastT(name : RAC.T; wx : Wx.T) : TEXT =
  BEGIN
    Gds2CastInt(name, wx);
    RETURN Wx.ToText(wx)
  END Gds2CastT;

PROCEDURE Gds2CastR(name : RAC.T; wx : Wx.T) : RAC.T =
  BEGIN
    Gds2CastInt(name, wx);
    RETURN Wx.ToChars(wx)
  END Gds2CastR;


BEGIN END Gds2Cast.
