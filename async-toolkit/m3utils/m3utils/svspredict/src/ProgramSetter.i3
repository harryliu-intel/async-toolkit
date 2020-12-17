INTERFACE ProgramSetter;
IMPORT Power;

TYPE
  T = PROCEDURE(VAR p                       : Power.Params;
                VAR Trunc                   : LONGREAL);

CONST Brand = "ProgramSetter";

END ProgramSetter.
