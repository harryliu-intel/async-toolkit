(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE PathnameUtils;
IMPORT Pathname;
TYPE
  T = Pathname.T;

PROCEDURE Slashify(path: T): T;
(* make "path" end in "/" *)

PROCEDURE SlashedPrefix(path: T): T;
PROCEDURE Complete(path: T): T;

PROCEDURE Join(pn, base: T; ext: TEXT := NIL): T;
(* like "Pathname.Join", but just returns (possibly extended) base
   (instead of crashing) in the case that base is absolute. *)

PROCEDURE DirOf(pn: T): T;
(* like "Pathname.Prefix", but returns "." if pn has no slash. *)

END PathnameUtils. 
