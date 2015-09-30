(* $Id: Factory.ig,v 1.4 2008/11/11 09:33:05 mika Exp $ *)

GENERIC INTERFACE Factory(Of);
IMPORT ObjectFactory;
(* A Factory.T is an object that allocates objects of the type Of.T *)
(* Call the build() method to allocate an object (ObjectFactory interface) *)
TYPE
  T <: Public;

  Public = ObjectFactory.T OBJECT METHODS buildT() : Of.T END;

CONST Brand = "Factory of " & Of.Brand;

END Factory.
