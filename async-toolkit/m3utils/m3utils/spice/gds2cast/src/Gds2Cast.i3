INTERFACE Gds2Cast;
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

PROCEDURE Gds2Cast(nm : TEXT) : TEXT;

  
PROCEDURE Gds2CastR(name : RAC.T; wx : Wx.T) : RAC.T;

PROCEDURE Gds2CastT(name : RAC.T; wx : Wx.T) : TEXT;

END Gds2Cast.
