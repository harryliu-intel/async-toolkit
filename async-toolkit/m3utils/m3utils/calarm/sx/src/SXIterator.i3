(* $Id: SXIterator.i3,v 1.1 2011/01/19 16:01:40 mika Exp $ *)

INTERFACE SXIterator;

(* utility functions for making debugging iterators *)

IMPORT SXRoot;

TYPE T = SXRoot.Iterator;

PROCEDURE NullIterator() : T;

PROCEDURE One(a : SXRoot.T) : T;

PROCEDURE Two(a, b : SXRoot.T) : T;

PROCEDURE Many(READONLY a : ARRAY OF SXRoot.T) : T;

CONST Brand = "SXIterator";


PROCEDURE NullNull(a : SXRoot.T) : T;
  (* can be used directly as a method for SXRoot.T *)

END SXIterator.
