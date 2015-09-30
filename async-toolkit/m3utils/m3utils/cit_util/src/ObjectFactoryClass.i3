(* $Id: ObjectFactoryClass.i3,v 1.1 2003/02/01 22:12:58 mika Exp $ *)

INTERFACE ObjectFactoryClass;
IMPORT ObjectFactory;
IMPORT RTType;

REVEAL
  ObjectFactory.T <: Private;

TYPE
  Private = ObjectFactory.Public OBJECT
    code : RTType.Typecode
  END;

END ObjectFactoryClass.
