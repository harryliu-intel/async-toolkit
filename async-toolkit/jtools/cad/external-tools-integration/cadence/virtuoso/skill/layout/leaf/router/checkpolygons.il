; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: CheckPolygons
; Parameter: CellView (CVId)
; Return:  CellView (CVId)
;
; Check polygons

defun( CheckPolygons ( CellView )

  (let (
        ( Polygons ( setof Shape ( getq CellView shapes )
                           ( equal ( getq Shape objType ) "polygon" ) ) ) )
;find polygons with angled edges
  ( println
    ( setof Shape Polygons
            ( PolygonIsAngled ( getq Shape points ) ) )~>bBox )
        
;find polygons with duplicate points
  
  ( foreach Shape Polygons
            (let (
                  ( Points ( PolygonCanonicalizePoints
                             ( getq Shape points ) ) ) )
              (when ( not ( equal Points ( getq Shape points ) ) )
                ( dbSetq Shape Points points ) ) ) )
  t )

  ( dbSave CellView )
 CellView 
)
