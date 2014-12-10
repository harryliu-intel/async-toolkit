; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$




(defun FindPlugRectInPlugInstance ( PlugInstance ImplantLPP )
  (when PlugInstance
    (let (
          ( PlugMaster ( getq PlugInstance master ) ) )
      (let (
            ( PlugImplantShapes ( setof
                                  Shape
                                  ( getq
                                    PlugMaster
                                    shapes
                                    )
                                  ( equal ( getq Shape lpp ) ImplantLPP ) ) ) )
        (let (
              ( PlugImplantBoundingRect ( ListApplyFuncToListAndAccumulateResult
                                          PlugImplantShapes
                                          (lambda
                                            ( ImplantShape CurrResult )
                                            ( RectGetBoundRect
                                              CurrResult
                                              ( getq ImplantShape bBox )
                                            ) )
                                          nil
                                          nil ) ) )
          (when PlugImplantBoundingRect
            (let (
                  ( PlugRectInPlug
                    ( dbTransformBBox 
                      PlugImplantBoundingRect
                      ( getq
                        PlugInstance
                        transform ) ) ) )
              PlugRectInPlug ) ) ) ) ) ) )

(defun FindPlugRectInGateInstance ( GateInstance
                                    PlugLibName
                                    PlugCellName
                                    PlugViewName
                                    ImplantLPP
                                    EvilGatePRegionOffset
                                    NImplantLPP
                                    ContactLPP )
  (let (
        ( GateInstanceMaster ( getq GateInstance master ) )
        ( GateInstanceTransform ( getq GateInstance transform ) )
        )
    (let (
          ( Instances ( getq GateInstanceMaster instances ) )
          )
      (let (
            ( PlugInstance ( car
                             ( setof
                               Instance
                               Instances
                               ( and
                                 ( equal
                                   ( getq Instance libName )
                                   PlugLibName
                                   )
                                 ( equal
                                   ( getq Instance cellName )
                                   PlugCellName
                                   )
                                 ( equal
                                   ( getq Instance viewName )
                                   PlugViewName ) ) ) ) ) )
        
       (let (
             ( PlugRectInPlug 
               ( FindPlugRectInPlugInstance 
                 PlugInstance
                 ImplantLPP ) ) )
         (if PlugRectInPlug
             ( dbTransformBBox 
               PlugRectInPlug 
               GateInstanceTransform )
             ( FindPlugRectInGateInstanceHack
               GateInstance
               ImplantLPP
               EvilGatePRegionOffset
               NImplantLPP
               ContactLPP
               ) ) ) ) ) ) )

(defun FindPlugRectInGateInstanceHack ( GateInstance ImplantLPP EvilGatePRegionOffset NImplantLPP ContactLPP )
  (when GateInstance
    (let (
          ( PlugMaster ( getq GateInstance master ) ) )
      (let (
            ( ContactShapes
              ( setof
                Shape
                ( getq
                  PlugMaster
                  shapes
                  )
                ( equal ( getq Shape lpp )
                        ContactLPP ) ) ) )
        (when ContactShapes
          (let (
                ( PlugShape
                  ( car
                    ( exists
                      Shape
                      ( getq
                        PlugMaster
                        shapes
                        )
                      ( and ( equal ( getq Shape lpp ) ImplantLPP )
                            (if ( equal ImplantLPP NImplantLPP )
                                ( greaterp ( RectGetRight ( getq Shape bBox ) ) EvilGatePRegionOffset )
                              ( lessp ( RectGetLeft ( getq Shape bBox ) ) EvilGatePRegionOffset ) )
                            ( exists ContactShape ContactShapes
                                     ( RectIsRectInRectClose ( getq ContactShape bBox )
                                                             ( getq Shape bBox ) 1e-9 ) ) ) ) ) ) )
            (when PlugShape
              ( dbTransformBBox 
                ( getq PlugShape bBox )
                ( getq GateInstance transform ) ) ) ) ) ) ) ) )
