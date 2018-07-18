INTERFACE MbyMeta;
IMPORT Metadata;
IMPORT MbyTypes;

(* basic metadata from the top-level model *)

TYPE
  T = Metadata.T BRANDED Brand OBJECT
    rxPort : MbyTypes.Port;
  END;

CONST Brand = "MbyMeta";

END MbyMeta.
