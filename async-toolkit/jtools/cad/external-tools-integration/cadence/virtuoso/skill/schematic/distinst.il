; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun DistributeInstances ( CellView StartX StartY MaxX ) 
  (let (
        ( Instances ( getq CellView instances ) )
        ( CurrX StartX )
        ( CurrY StartY )
        ( CurrMaxHeight 0 )
        )
    ( foreach 
      Inst 
      Instances
      (let (
            ( InstBBox ( getq Inst bBox ) ) 
            )
        (let (
              ( InstX ( RectGetLeft InstBBox  ) )
              ( InstY ( RectGetBottom  InstBBox  ) )
              ( InstWidth ( RectGetWidth  InstBBox  ) )
              ( InstHeight ( RectGetHeight InstBBox ) )
              )
                           
          (when ( greaterp 
                  ( plus CurrX InstWidth )
                  MaxX
                  )
            
            ( setq CurrY ( plus CurrY CurrMaxHeight ) )
            ( setq CurrMaxHeight 0 )
            ( setq CurrX StartX )
            )
          ( dbMoveFig
            Inst
            nil
            ( list
              ( list
                ( difference
                  CurrX
                  InstX
                  )
                ( difference
                  CurrY
                  InstY
                  )
                )
              "R0"
              )
            )
          ( setq CurrX ( plus CurrX InstWidth ) )
          (when ( lessp CurrMaxHeight InstHeight )
            ( setq CurrMaxHeight InstHeight )
            )
          )
        
        )
      )
    )
  nil
  )
