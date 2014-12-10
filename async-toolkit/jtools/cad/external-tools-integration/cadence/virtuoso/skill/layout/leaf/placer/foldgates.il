; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: FoldGates
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Change the folding of the Gates to adjust the width of them.
;
;
;


defun( FoldGates ( CellView )

  foreach( Gate cadr( NameFilterInstances(
                          CellView~>instances 
                          GateLibCellPairRegExs ) )
    when( PCellGetParameterValue( Gate "allow_overfolding" )
      dbReplaceProp( Gate "allow_overfolding" "boolean" "FALSE" ) 
    )
    when( PCellGetParameterValue( Gate "use_poly_contact" ) && PCellGetParameterValue( Gate "NW1" ) 
      if( PCellGetParameterValue( Gate "NW1" )<0.72/MicronsPerMeter then
        dbReplaceProp( Gate "use_poly_contact" "boolean" "FALSE" ) 
      )
    ) ; turn off poly contact for small gate.INV cells
    when( PCellGetParameterValue( Gate "mid_poly_contact" ) 
        dbReplaceProp( Gate "mid_poly_contact" "boolean" "TRUE" )              
    ) ; turn on mid_poly_contact for gate.WINV cells.
  )
  NPSearchFoldGates(
    cadr( NameFilterInstances(
             getq( CellView instances )
             GateLibCellPairRegExs ) )
    NColumnWidthParamName
    PColumnWidthParamName
    NWidth
    PWidth
    UserUnitsPerMeter )
  dbSave( CellView )
  CellView 
)

defun( FixMustConnectTerm ( CellView )
  let((inst foo term2 term1)
    foreach( inst CellView~>instances
      foreach( term1 inst~>terminals
        if( rexMatchp( "mustConnect_" term1->name) then
          foo=parseString(term1->name "_")
          rexCompile(strcat(car(foo) "_" cadr(foo) "_")) 
          term2Name=rexReplace(term1->name "" 0)
          foreach( term2 inst->terminals
            if( term2->name == term2Name then
              term1->net=term2->net
            )
          )
        )
      )
      if( inst->cellName=="stack.PPLUG.0" then
        dbCreateConnByName( dbMakeNet( CellView VddNetName )
           inst "Vdd" )
      )  
      if( inst->cellName=="stack.NPLUG.0" then
        dbCreateConnByName( dbMakeNet( CellView GNDNetName )
           inst "GND" )
      )  
    )
  )
)
