INTERFACE Cloudbreak;
FROM SvsTypes IMPORT CornerData;
IMPORT Power;

PROCEDURE SetProgram(VAR params                  : Power.Params;
                     VAR Trunc                   : LONGREAL);

CONST Brand = "Cloudbreak";

END Cloudbreak.
