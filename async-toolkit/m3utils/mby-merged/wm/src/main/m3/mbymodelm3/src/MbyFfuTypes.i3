INTERFACE MbyFfuTypes;
IMPORT Word;

CONST LS = Word.LeftShift;

TYPE
  Keys            = RECORD END; (* fill this in *)
  Actions         = RECORD END; (* fill this in *)
  Scenario        = [0..LS(1,6)-1];
  Vrid            = [0..LS(1,4)-1];
  PriorityProfile = [0..LS(1,5)-1];

CONST Brand = "MbyFfuTypes";

END MbyFfuTypes.
