(* $Id$ *)

INTERFACE CITRandom;
IMPORT Random;

TYPE
  T <: Public;

  Public = Random.T OBJECT METHODS

    (* for a fixed generator, call with init(TRUE, SEED) *)
    (* call with init(FALSE) for a random-random generator *)
    init(fixed : BOOLEAN; seed : INTEGER := 0) : T;
    seed() : INTEGER;
  END;

CONST Brand = "CITRandom";

END CITRandom.
