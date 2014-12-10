; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;FIXME - should deal with mosaics
; 

; a path is a path through the hierarchy as returned by dbGetOverlaps
; ( I1 ( I2 ( I3 Fig ) ) )
(defun TransformGetBBoxFromPath ( Path )
  (let (
        ( Stack ( reverse ( TransformCanonicalizeInstancePath Path ) ) ) )
  ( dbTransformBBox
    ( getq ( car Stack ) bBox )
    ( TransformGetTransformFromInstanceStack ( cdr Stack ) )
    )
  ) )

(defun TransformGetTransformFromInstancePath ( Path )
  ( TransformGetTransformFromInstanceStack
    ( reverse ( TransformCanonicalizeInstancePath Path ) ) ) )

(defun TransformGetTransformFromInstanceStack ( StackOfInstances )
 (let (
       ( FullTransform ( list 0:0 "R0" ) )
       )
   (while StackOfInstances
     (let (
           ( CurrInstance ( car StackOfInstances ) ) )
       ( setq StackOfInstances ( cdr StackOfInstances ) )

       (let (
             ( Transform 
               (cond (
                      ( listp CurrInstance )
                      ( TransformGetMosaicInstTransform
                        ( car CurrInstance )
                        ( cadr CurrInstance )
                        ( caddr CurrInstance )
                        ) )
                     (
                      ( getq CurrInstance transform ) )
                     ) )
             )
         (cond ( ( equal ( getq CurrInstance objType ) "stdVia" )
                 ( setq Transform ( list 
                                    ( getq CurrInstance origin )
                                    ( getq CurrInstance orient ) ) ) ) )

         ( setq
           FullTransform
           ( dbConcatTransform
             FullTransform
             Transform
             ) )


) ) )
   FullTransform ) )

(defun TransformCanonicalizeInstancePath ( Path )
  (let (
        ( Components 
          (cond (
                 ( listp Path )
                 ( ListFlatten Path )
                 )
                ( 
                 ( list Path ) ) ) )
        ( Ret nil )
        )
    (while Components
      (cond (
             ( and 
               ( equal ( getq ( car Components ) objType ) "mosaicInst" )
               ( numberp ( cadr Components ) )
               ( numberp ( caddr Components ) ) )
             ( setq Ret ( tconc Ret ( list ( car Components )
                                           ( caddr Components )
                                           ( cadr Components ) )
                                ) )
             ( setq Components ( cdddr Components ) )
              )
            (
             ( equal ( getq ( car Components ) objType ) "mosaicInst" )
             ( setq Ret ( tconc Ret ( list ( car Components )
                                           1
                                           1 ) ) ) )
            (
             ( setq Ret ( tconc Ret ( car Components ) ) )
             ( setq Components ( cdr Components ) )
             )
            ) )

    ( car Ret ) ) )
      
        
;convert a instancenamelist
(defun TransformGetInstancePathFromInstanceNamePath 
  ( CellView 
    InstanceNamePath )
  ( mapcar 
    (lambda ( InstanceName )
      (let (
            ( Instance
              (if CellView
                  ( dbFindAnyInstByName
                    CellView
                    InstanceName ) ) ) )
        ( setq CellView
               ( getq Instance master ) )
        Instance ) )
    InstanceNamePath ) )

(defun TransformIsValidInstanceNamePath ( CellView 
                                          InstanceNamePath )
  ( not 
    ( exists
      Instance
      ( TransformGetInstancePathFromInstanceNamePath 
        CellView 
        InstanceNamePath )
      ( null Instance ) ) ) )

(defun TransformGetMosaicInstTransform ( MosaicInst I J )
  (let (
        ( Mosaic ( getq MosaicInst mosaic ) ) )
    ( list
      ( PointAdd
        ( list
          ( times I ( getq Mosaic uX ) )
          ( times J ( getq Mosaic uY ) ) )
        ( getq Mosaic xy )
        )
      ( car ( getq Mosaic tileArray ) ) ) ) )


(defun TransformGetInverseTransform ( Transform )
    (let (
          ( Unscaled
            ( dbConcatTransform 
              ( list ( PointDifference 0:0 ( car Transform ) )
                     "R0"
                     1.0 )
              ( list 0:0
                     (cond (
                            ( equal ( cadr Transform ) "R90" )
                            "R270" )
                           (
                            ( equal ( cadr Transform ) "R270" )
                            "R90" )
                           (
                            t
                            ( cadr Transform ) ) )
                     1.0 ) ) ) )
      (if ( caddr Transform )
          ( dbConcatTransform Unscaled 
                              ( list 0:0
                                     "R0"
                                     ( quotient 1.0 ( caddr Transform ) ) ) )
        Unscaled  ) ) )
                 
