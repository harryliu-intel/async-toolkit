; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: RouterSearchInit
; Parameter: none
; Return: none
;
; Initialize the router

defun( RouterSearchInit ( )

(when ( null PreRouteViewNames )

  PreRouteViewList = setof( CellView ddGetObj( LibName CellName ) ~> views 
      rexMatchp( sprintf( nil "%s_[0-9]+" PreRouteViewName ) CellView~>name ) )

  PreRouteViewTempList =
    foreach( mapcar CellView PreRouteViewList 
      ViewName = CellView~>name
      CellView = dbOpenCellViewByType( LibName CellName ViewName nil "r" )
      BoundaryBox = PinUtilFindPRBoundBBox( BoundaryLPP CellView)
      Area = RectGetArea( BoundaryBox )
      list( ViewName Area )
    )

  PreRouteViewTempList = 
    sort( copy( PreRouteViewTempList ) 
      (lambda ( arg1 arg2 )     
         cadr(arg1) < cadr(arg2)
       ) 
    )
    
  PreRouteViewNames = 
    foreach( mapcar arg PreRouteViewTempList 
      car(arg)
    )
)
(printf "views: %L\n" PreRouteViewTempList )
(printf "viewnames: %L\n" PreRouteViewNames )
(defvar CurrentPreRouteViewName ( car PreRouteViewNames  ) )

)                    
