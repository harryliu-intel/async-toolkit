INTERFACE OptVar;

(* 
   one of the variables over which we optimize

   The optimization space is over the scaled variable.

   We do NOT shift the variable; the "defval" is simply used to set the
   starting point of the optimization.

   So the physical variable phys[i] is related to the ith optimization
   variable p[i] in the sense that

   phys[i] = p[i] * defstep[i]

   where p[i] initially is given by p0[i] = defval[i] / defstep[i]
   (i.e., phys0[i] = defval[i])

   phys[i] is, finally, bound to nm in the Scheme environment(s).
   
*)

TYPE
  T = RECORD
    nm              : TEXT;
    defval, defstep : LONGREAL;
  END;

CONST Brand = "OptVar";

END OptVar.

