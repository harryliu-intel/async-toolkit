(* $Id$ *)

GENERIC INTERFACE Fifo(Elem);

(* Copyright (c) 2005-2006, Generation Capital Ltd.  All rights reserved. *)
(* Author: Mika Nystrom <mika@gcapltd.com> *)

(* fifo of Elem.T; 
   Elem must export
   
   Equal(a, b : Elem.T) : BOOLEAN;
   where the mode of a, b can be VALUE, VAR, or READONLY

   and a text constant
   Elem.Brand
*)

(* ALL METHODS ARE UNMONITORED, You have to lock things yourself! *)

TYPE 
  T <: Public;

  Public = MUTEX OBJECT METHODS
    init() : T;

    put(t : Elem.T);

    get() : Elem.T;

    empty() : BOOLEAN;

    member(t : Elem.T) : BOOLEAN;

    size() : CARDINAL;
  END;

CONST Brand = Elem.Brand & " Fifo";

END Fifo.
