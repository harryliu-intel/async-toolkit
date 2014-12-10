; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;given List(x1, x2...xn), Func()
;run y=Func(x) for x1...xn
;returns Table[y]=list(all_xs_that_Func(x)=y)
(defun PartitionInverse ( List 
                          Func )
  (let (
        ( Map ( makeTable `partition nil ) ) )
    ( foreach Element List 
              (let (
                    ( Value
                      ( apply Func ( list Element ) ) ) )
                ( setarray Map 
                           Value 
                           ( cons Element ( arrayref Map Value ) ) ) ) )
    Map ) )

;given List(x1, x2...xn), Func()
;run y=Func(x) for x1...xn
;returns Table[x]=list(y all_xs_that_Func(x)=y)
(defun Partition ( List 
                   Func )
  (let (
        ( PartitionInverse ( PartitionInverse List Func ) ) )
    ( PartitionInvertInverse 
      PartitionInverse ) ) )

(defun PartitionInvertInverse ( PartitionInverse )
  (let (
        ( Partition ( makeTable `inverse nil ) ) )
    ( foreach Value PartitionInverse
              (let (
                    ( Elements ( arrayref PartitionInverse Value ) ) )
                ( foreach Element Elements 
                          ( setarray Partition Element ( list Value Elements ) ) ) ) )
    Partition ) )
