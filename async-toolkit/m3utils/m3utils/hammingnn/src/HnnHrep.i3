(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE HnnHrep;
IMPORT Word;

TYPE
  T = REF RECORD
    (* s/b REF RECORD? do we ever need the non-REF version? *)

    bits        : REF ARRAY OF Word.T;
    (* the bits, in little-endian order 
       protected by hashValid!
    *)
    
    sz          : CARDINAL;
    (* may not need this *)
    
    hashV       : Word.T;
    hashValid := FALSE;
    (* hash value and valid bit for storage *)

    id          : CARDINAL; 
    (* id, s/n into seq in parent master table *)
    
  END;

PROCEDURE SetHash(t : T);

PROCEDURE Equal(a, b : T) : BOOLEAN;
  (* checks a.bits^ = b.bits^, and sz
     ignores id
  *)

PROCEDURE Hash(t : T) : Word.T;
  (* hash the bits (only), caches in hashV *)

CONST Brand = "HnnHrep";

PROCEDURE New(READONLY a : ARRAY OF BOOLEAN) : T;
  (* allocate a new T and fill it in with contents of a,
     with hashValid FALSE and id set to an arbitrary value *)

PROCEDURE ToArray(t : T; VAR a : ARRAY OF BOOLEAN);

PROCEDURE Length(t : T) : CARDINAL;

PROCEDURE GetBits(t : T; from, n : CARDINAL) : Word.T;
  (* get the bits from from to from + n - 1 inclusive;
     it is a checked runtime error for (n > Word.Size) *)

PROCEDURE Distance(a, b : T) : CARDINAL;
  (* Hamming distance *)

PROCEDURE DistanceLessEqual(a, b : T; maxDist : CARDINAL) : CARDINAL;
  (* Distance(a,b) <= maxDist  -> returns dist  ; else returns LAST(CARDINAL) *)
      
END HnnHrep.
