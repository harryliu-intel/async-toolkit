(* $Id$ *)

GENERIC MODULE Factory(Of);
IMPORT Word, Text;

REVEAL
  T = Public BRANDED Brand OBJECT 
  OVERRIDES
    build := Build;
  END;

PROCEDURE Hash(<*UNUSED*>a : T) : Word.T = 
  BEGIN RETURN Text.Hash(Brand) END Hash;

PROCEDURE Equal(<*UNUSED*>a, b : T) : BOOLEAN = BEGIN RETURN TRUE END Equal;

PROCEDURE Build(<*UNUSED*>a : T) : Of.T = BEGIN RETURN NEW(Of.T) END Build;

BEGIN END Factory.


 
