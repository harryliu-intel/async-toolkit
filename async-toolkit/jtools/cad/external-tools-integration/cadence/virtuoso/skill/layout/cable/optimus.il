; Copyright Intel Corporation.  All rights reserved.
; $Author: pankala1 $

(defun TransformOptimus (ObjList)
   let( ( Ovp InstHier TransList FinObj I Trnf )

     Ovp = ObjList
     InstHier = nil
     TransList = nil

     while( listp(Ovp)
       InstHier = cons(car(Ovp) InstHier)
       Ovp = cadr(Ovp)
     )

     FinObj = Ovp
     TransList = list(FinObj~>bBox)

     for( I 0 length(InstHier)-1
        Trnf = dbTransformBBox(car(TransList) nth(I InstHier)~>transform)
        TransList = cons(Trnf TransList)
      )
   list(FinObj car(TransList))
 )
)
