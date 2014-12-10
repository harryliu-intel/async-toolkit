; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: FixWireExtension
; Parameter: CellView (CVId)
; Return:  CellView (CVId)
;
; Fix Wire Extension

defun( DelExtraPolyContact ( CellView )

 ; get rid of unused contacts on inputs to gates
 ; this is to get rid of m1 area violations and
 ; to reduce stray capacitance
    ( foreach
      Gate
      ( cadr ( NameFilterInstances
               ( getq CellView instances )
               GateLibCellPairRegExs ) )
      ;if we can change poly contacts and there's a single fold
      (when ( and
              ( if ( NPSearchGateGetFolds Gate ) 
                    ( leqp ( NPSearchGateGetFolds Gate ) 1 )
                  t )
              ( PCellGetParameterValue Gate "poly_contact" ) )
        ( printf "gate: %L\n" Gate->name )
        ;find the first m1 contact shape... 
        ( exists 
          Fig
          ( PinUtilGetPinFigsForInstance 
            Gate
            (lambda ( Net ) t )
            (lambda ( LPP ) ( equal LPP Metal1LPP ) )
            ?TerminalP (lambda ( Term )
                         ( or ( equal Term "a" )
                              ( equal Term "in" )
                              ) )
            )
          ;that doesn't overlap any m1 shapes in the cellview...
          (let (
                ( Overlap
                  ( exists
                    Overlap
                    ( dbGetTrueOverlaps
                      CellView
                      ( dbTransformBBox
                        ( getq Fig bBox )
                        ( getq Gate transform ) )
                      Metal1LPP
                      0:32
                      )
                    ;that aren't inside the gate instance
                    ( or
                      ( not ( listp Overlap ) )
                      ( not ( equal ( car Overlap ) Gate ) ) ) ) ) )
            ( printf "overlap: %L %L\n" Overlap Overlap~>bBox )

            (when ( null Overlap )
            ;this contact doesn't overlap anything
            ;if it's to the right of the origin in instance master,
            ;then only have poly contacts on left, and vice versa
             (cond (
                    ( geqp
                      ( car
                        ( RectGetCenter ( getq Fig bBox ) ) )
                      0.0 )
                    ( dbReplaceProp 
                      Gate "poly_contact" "string" "left" ) )
                   (
                    ( dbReplaceProp 
                      Gate "poly_contact" "string" "right" ) ) ) ) ) ) ) )


    ( dbSave CellView )
    ( dbPurge CellView )
CellView
)
                  
        
