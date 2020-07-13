INTERFACE Plane;
FROM PizzaTypes IMPORT Base;

TYPE T = ARRAY [0..3] OF Base;

(* t : T

   Plane equation is 

   t[0] * x + t[1] * y + t[2] * z + t[3] * 1 = 0  

   where it is required that

   SS(t[0..3]) = 1 

   (t[0..3] is a unit vector)
*)

CONST Brand = "Plane";

END Plane.
