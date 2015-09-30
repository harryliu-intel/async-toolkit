(* $Id: ReadLineHelpClass.i3,v 1.1 2009/04/24 07:09:13 mika Exp $ *)

INTERFACE ReadLineHelpClass;
IMPORT ReadLineHelp;
IMPORT ReadLineHelpNode AS Node;

TYPE
  Private = ReadLineHelp.Public OBJECT METHODS
    getRoot() : Node.T;
  END;

REVEAL ReadLineHelp.T <: Private;

END ReadLineHelpClass.
    
