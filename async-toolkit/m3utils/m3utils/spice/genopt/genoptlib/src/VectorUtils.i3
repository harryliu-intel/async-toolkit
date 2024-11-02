INTERFACE VectorUtils;
IMPORT LRVector;

PROCEDURE Orthogonalize(READONLY da : ARRAY OF LRVector.T);
  (* orthogonalizes (orthonormalizes) the first N elements of da;
     doesnt touch da[0] *)

END VectorUtils.
