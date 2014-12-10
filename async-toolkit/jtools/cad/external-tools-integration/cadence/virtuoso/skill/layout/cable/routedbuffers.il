(defun  BuffersUpTo (x)
    (let (TempStr)
       for(n 0 (59 - x)
          println(x)
          sprintf(TempStr "%d" x)
          DeleteMyObject(TempStr)
          x = x + 1
          )
    )      
    t
)


(defun DeleteMyObject (x)
    (let (ObjL0 ObjL1 ObjLe ObjL2 ObjL3 
          ObjR0 ObjR1 ObjRe ObjR2 ObjR3 
          InstName view shapes)
    	        
  
        ObjL0=strcat("L[" x "].0")
        ObjL1=strcat("L[" x "].1")
        ObjLe=strcat("L[" x "].e")
        ObjL2=strcat("L[" x "].2")
        ObjL3=strcat("L[" x "].3")

        ObjR0=strcat("R[" x "].0")
        ObjR1=strcat("R[" x "].1")
        ObjRe=strcat("R[" x "].e")
        ObjR2=strcat("R[" x "].2")
        ObjR3=strcat("R[" x "].3")

        InstName=strcat("mbuf[0][" x "]")

        view=(geGetEditCellView) 

        (foreach shape view~>shapes

	   (when shape~>net~>name==ObjL0
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjL1
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjLe
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjL2
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjL3
              (dbDeleteObject shape)
           )


           (when shape~>net~>name==ObjR0
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjR1
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjRe
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjR2
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjR3
              (dbDeleteObject shape)
           )
        )

        (foreach via view~>vias

	   (when via~>net~>name==ObjL0
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjL1
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjLe
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjL2
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjL3
              (dbDeleteObject via)
           )


           (when via~>net~>name==ObjR0
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjR1
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjRe
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjR2
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjR3
              (dbDeleteObject via)
           )
        )

        (foreach instance view~>instances

	   (when instance~>name==InstName
              (dbDeleteObject instance)
           )
        )
    )
    t
)

(defun  BuffersUpTo (x)
    (let (TempStr)
       for(n 0 (59 - x)
          println(x)
          sprintf(TempStr "%d" n)
          DeleteMyObject(TempStr)
          n = n + 1
          )

       w = 60 - x       
       println(w)
       for(m 0 (x - 1)
          y = w - 60 + x
          println(y)
          sprintf(TempStr1 "%d" w)
          sprintf(TempStr2 "%d" y)
          SetObjProp(TempStr1 TempStr2)
          w = w + 1
          )
    )      
    t
)


(defun DeleteMyObject (z)
    (let (ObjL0 ObjL1 ObjLe ObjL2 ObjL3 
          ObjR0 ObjR1 ObjRe ObjR2 ObjR3 
          InstName view shapes)
    	        
  
        ObjL0=strcat("L[" z "].0")
        ObjL1=strcat("L[" z "].1")
        ObjLe=strcat("L[" z "].e")
        ObjL2=strcat("L[" z "].2")
        ObjL3=strcat("L[" z "].3")

        ObjR0=strcat("R[" z "].0")
        ObjR1=strcat("R[" z "].1")
        ObjRe=strcat("R[" z "].e")
        ObjR2=strcat("R[" z "].2")
        ObjR3=strcat("R[" z "].3")

        InstName=strcat("mbuf[0][" z "]")

        view=(geGetEditCellView) 

        (foreach shape view~>shapes

	   (when shape~>net~>name==ObjL0
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjL1
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjLe
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjL2
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjL3
              (dbDeleteObject shape)
           )


           (when shape~>net~>name==ObjR0
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjR1
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjRe
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjR2
              (dbDeleteObject shape)
           )
           (when shape~>net~>name==ObjR3
              (dbDeleteObject shape)
           )
        )

        (foreach via view~>vias

	   (when via~>net~>name==ObjL0
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjL1
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjLe
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjL2
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjL3
              (dbDeleteObject via)
           )


           (when via~>net~>name==ObjR0
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjR1
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjRe
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjR2
              (dbDeleteObject via)
           )
           (when via~>net~>name==ObjR3
              (dbDeleteObject via)
           )
        )

        (foreach instance view~>instances

	   (when instance~>name==InstName
              (dbDeleteObject instance)
           )
        )
    )
    t
)


(defun SetObjProp (w y)
    (let (ObjL0 ObjL1 ObjLe ObjL2 ObjL3 
          ObjR0 ObjR1 ObjRe ObjR2 ObjR3 
          InstName view shapes)   

         
  
        ObjL0=strcat("L[" w "].0")
        ObjL1=strcat("L[" w "].1")
        ObjLe=strcat("L[" w "].e")
        ObjL2=strcat("L[" w "].2")
        ObjL3=strcat("L[" w "].3")

        ObjL0r=strcat("L[" y "].0")
        ObjL1r=strcat("L[" y "].1")
        ObjLer=strcat("L[" y "].e")
        ObjL2r=strcat("L[" y "].2")
        ObjL3r=strcat("L[" y "].3")

        ObjR0=strcat("R[" w "].0")
        ObjR1=strcat("R[" w "].1")
        ObjRe=strcat("R[" w "].e")
        ObjR2=strcat("R[" w "].2")
        ObjR3=strcat("R[" w "].3")

        ObjR0r=strcat("R[" y "].0")
        ObjR1r=strcat("R[" y "].1")
        ObjRer=strcat("R[" y "].e")
        ObjR2r=strcat("R[" y "].2")
        ObjR3r=strcat("R[" y "].3")

        InstName=strcat("mbuf[0][" w "]")

        InstNamer=strcat("mbuf[0][" y "]")

        view=(geGetEditCellView) 

        (foreach shape view~>shapes

	   (when shape~>net~>name==ObjL0
              (dbSet shape ObjL0r "name")
           )
           (when shape~>net~>name==ObjL1
              (dbSet shape ObjL1r "name")
           )
           (when shape~>net~>name==ObjLe
              (dbSet shape ObjLer "name")
           )
           (when shape~>net~>name==ObjL2
              (dbSet shape ObjL2r "name")
           )
           (when shape~>net~>name==ObjL3
              (dbSet shape ObjL3r "name")
           )


           (when shape~>net~>name==ObjR0
              (dbSet shape ObjR0r "name")
           )
           (when shape~>net~>name==ObjR1
              (dbSet shape ObjR1r "name")
           )
           (when shape~>net~>name==ObjRe
              (dbSet shape ObjRer "name")
           )
           (when shape~>net~>name==ObjR2
              (dbSet shape ObjR2r "name")
           )
           (when shape~>net~>name==ObjR3
              (dbSet shape ObjR3r "name")
           )
        )

        (foreach via view~>vias

	   (when via~>net~>name==ObjL0
              (dbSet via ObjL0r "name")
           )
           (when via~>net~>name==ObjL1
              (dbSet via ObjL1r "name")
           )
           (when via~>net~>name==ObjLe
              (dbSet via ObjLer "name")
           )
           (when via~>net~>name==ObjL2
              (dbSet via ObjL2r "name")
           )
           (when via~>net~>name==ObjL3
              (dbSet via ObjL3r "name")
           )


           (when via~>net~>name==ObjR0
              (dbSet via ObjR0r "name")
           )
           (when via~>net~>name==ObjR1
              (dbSet via ObjR1r "name")
           )
           (when via~>net~>name==ObjRe
              (dbSet via ObjRer "name")
           )
           (when via~>net~>name==ObjR2
              (dbSet via ObjR2r "name")
           )
           (when via~>net~>name==ObjR3
              (dbSet via ObjR3r "name")
           )
        )

        (foreach instance view~>instances

	   (when instance~>name==InstName
              (dbSet instance InstNamer "name")
           )
        )
    )
    t
)
