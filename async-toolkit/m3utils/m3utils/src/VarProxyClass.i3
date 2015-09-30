(* $Id: VarProxyClass.i3,v 1.1 2009/04/24 07:09:13 mika Exp $ *)

INTERFACE VarProxyClass;
IMPORT VarUI;

TYPE
  Private = VarUI.PubVarProxy OBJECT
    mu : MUTEX;
    mode : VarUI.ProxyMode;
  END;

REVEAL VarUI.VarProxy <: Private;

END VarProxyClass.

  
