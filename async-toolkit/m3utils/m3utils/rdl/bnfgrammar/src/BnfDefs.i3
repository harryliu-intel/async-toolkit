INTERFACE BnfDefs;

CONST BufSiz = 1024; (* needs to be at least longest token + 1 *)

TYPE SC = SET OF CHAR;

CONST Lower   = SC { 'a' .. 'z' };
      Upper   = SC { 'A' .. 'Z' };
      Alpha   = Lower + Upper;
      Digit   = SC { '0' .. '9' };
      Ident1  = Alpha + SC { '_' };
      Ident2  = Ident1 + Digit;

      LB      = '{';
      RB      = '}';

      LP      = '(';
      RP      = ')';

      LR      = '[';
      RR      = ']';

      CM      = ',';
      VB      = '|';
      SK      = ';';
      EQ      = '=';

      NL      = '\n';
      CR      = '\r';
      TB      = '\t';
      SP      = ' ';

      White   = SC { NL, CR, TB, SP };

      Special = SC { LB, RB, LP, RP, CM, VB, SK, EQ, LR, RR };

      CComments = TRUE;
      DoString  = TRUE;
      StringQuote = '"';
      
CONST Brand = "BnfDefs";

END BnfDefs.
