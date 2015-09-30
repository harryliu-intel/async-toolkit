INTERFACE TaggedToken;

TYPE Token = { Line,
               Int, 
               Fix, 
               ParenStuff, 
               String }; 

CONST TokenNames = ARRAY Token OF TEXT { 
  "Line", 
  "Int", 
  "Fix", 
  "ParenStuff", 
  "String" 
  };

TYPE 
  T = RECORD
    token : Token;
    str   : TEXT;
    col   := -1; (* decorate later *)
  END;

CONST Brand = "TaggedToken";

END TaggedToken.
