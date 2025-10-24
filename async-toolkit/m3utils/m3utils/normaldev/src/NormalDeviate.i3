INTERFACE NormalDeviate;
IMPORT Random;

PROCEDURE Get(rand : Random.T; mean, sdev : LONGREAL) : LONGREAL;

PROCEDURE Trunc(rand : Random.T; mean, sdev, trunclo, trunchi : LONGREAL) : LONGREAL;
  (* dist truncated at trunclo,trunchi *)

PROCEDURE Get2(rand                 : Random.T;
               mean, sdev           : LONGREAL;
               VAR (* OUT *) d1, d2 : LONGREAL);

CONST Brand = "NormalDeviate";

END NormalDeviate.
