INTERFACE MpzRep;
IMPORT Mpz;
IMPORT Word;

REVEAL
  Mpz.T = BRANDED Mpz.Brand OBJECT
    val : Val;
  END;


TYPE
  Val = RECORD
    w : ARRAY [0..2] OF Word.T;
    (* this will be cast to an mpz_t, and it is AT LEAST big enough. *)
  END;

END MpzRep.
