INTERFACE LibertyArcs;
IMPORT SchemePair;

PROCEDURE Parse(spec : TEXT) : SchemePair.T;
  (* parse a spec in the form a.b.c[0].d.e[11] into the form
     ((field . a)
      (field . b)
      (field . c)
      (get . 0)
      (field . d)
      (field . e)
      (get . 1))
  *)

CONST Brand = "LibertyArcs";

END LibertyArcs.
