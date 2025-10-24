(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RandomVector;
IMPORT Random;

TYPE Base = LONGREAL;
     
PROCEDURE GetDir(rand    : Random.T;     (* random generator     *)
                 r       : Base;         (* radius               *)
                 VAR v   : ARRAY OF Base (* workspace and output *)
  );
  (* generate a random vector on the N-1-sphere (surface of the N-ball) *)
  
PROCEDURE GetDirV(rand    : Random.T;     (* random generator     *)
                  dims    : CARDINAL;
                  r       : Base          (* radius               *)
                  ) : REF ARRAY OF Base;
  (* as GetDir, but allocate memory for the result, too *)

PROCEDURE GetPoint(rand    : Random.T;     (* random generator     *)
                   r       : Base;         (* radius               *)
                   VAR v   : ARRAY OF Base (* workspace and output *)
  );
  (* generate a random vector in the N-ball *)
  
PROCEDURE GetPointV(rand    : Random.T;     (* random generator     *)
                   dims    : CARDINAL;
                   r       : Base          (* radius               *)
  ) : REF ARRAY OF Base;
  (* as GetPoint, but allocate memory for the result, too *)

CONST Brand = "RandomVector";
        
END RandomVector.
