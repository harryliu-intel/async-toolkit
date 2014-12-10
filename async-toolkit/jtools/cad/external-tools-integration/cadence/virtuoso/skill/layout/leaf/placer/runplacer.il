; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


; $Id$
; $DateTime$
; $Author$


; Function: RunPlacer
; Parameter: ScratchViewName (string)
;            CurrentPlacedOnlyViewName (string)
; Return: PlacedOnlyCellView (CVId)
;
; Run Placer.
;
;
;

defun( CreateSchematicView ()
  let( (ViewDDObj CellView)
    ViewDDObj=ddGetObj( LibName CellName "schematic" )
    if( and( 
            ViewDDObj 
            ddIsObjReadable( ViewDDObj )
            car( ViewDDObj~>files )
            ddIsObjReadable( car( ViewDDObj~>files ) )) then
      schView=dbOpenCellViewByType(
        LibName
        CellName
        "schematic"
        "schematic"
        "r" )
    else schView=dbOpenCellViewByType(
        LibName
        CellName
        "schematic"
        "schematic"
        "w" )
      dbSave(schView)

    )
    schView
  )
)

defun( RunPlacer ( TargetViewName TargetPlacerViewName )

;( LicenseGetLicense "Virtuoso_Layout_Suite_GXL" VirtuosoXLVersion 0 )

(let ( CellView PlacerView )
     PlacerView = CopyView( TargetViewName TargetPlacerViewName )

( LicenseGetLicense "Virtuoso_XL" VirtuosoXLVersion 0 )


vcpfeRunCustomDigitalPlacer(schView PlacerView
  ?groupCMOSPairs nil
  ?preserveChains nil
  ?groupMFactors  nil
  ?allowRotation  nil
  ?ecoMode nil
  ?globalPlacement t
  ?optimizePlacement t
  ?runTime "moderate"
  ?insertFillerCells nil
  ?insertSubstrateContacts nil
  ?componentEdge "Bounding Box"
  ?vcpRulesConstraintGroup "virtuosoDefaultSetup"

)

dbSave( PlacerView )

;( LicenseReturnLicense "Virtuoso_Layout_Suite_GXL"  )
( LicenseReturnLicense "Virtuoso_XL"  )

SnapInstancesToColums( PlacerView )

)
)

defun( SnapInstancesToColums ( CellView )
  let((Columns Column GateColumns PColumns NColumns)
    Columns= PlacerGetColumnsFromCellRegionData( 
      CellRegionData
      ( list GateRegionComponentClassName )
      ( list NRegionComponentClassName )
      ( list PRegionComponentClassName )
      ( times UserUnitsPerMeter NRegionOffsetFromColumnCenter )
      ( times UserUnitsPerMeter PRegionOffsetFromColumnCenter )
      ( times UserUnitsPerMeter EvilGatePRegionOffset ) 
    )
    GateColumns= setof( Column Columns car(caddr(Column))=="GATE")
    PColumns= setof( Column Columns car(caddr(Column))=="PSTACK")
    NColumns= setof( Column Columns car(caddr(Column))=="NSTACK")

    foreach( inst CellView~>instances
      if( inst~>cellName== GateSuperStackCellName then 
        SnapToColumn( inst GateColumns )
      )  
      if( inst~>cellName== PSuperStackCellName then 
        SnapToColumn( inst PColumns )
      )  
      if( inst~>cellName== NSuperStackCellName then 
        SnapToColumn( inst NColumns )
      )  
    )
  )

)

defun( SnapToColumn ( inst Columns )
  let( (thisDist thisOrient Column bBox alignment x instX instY orient)
    thisDist=1e9
    thisOrient="R0"
    instX=car(inst~>xy)
    instY=cadr(inst~>xy)
    foreach( Column Columns
      bBox=car(Column)
      alignment=cadr(Column)
      if( alignment=="right" then x=rightEdge(bBox)
      else x=leftEdge(bBox))
      orient=car(nth(3 Column))
      if( abs(x-instX)<abs(thisDist) then 
        thisDist=x-instX
        thisOrient=orient
      ) 
    )
    inst->xy=list(instX+thisDist instY)
    inst->orient=thisOrient
  )
)

defun( TurnOffGateContact ( CellView )
  let((inst)
    foreach( inst CellView~>instances
      if( rexMatchp("gate.NAND" inst~>cellName) || rexMatchp("gate.NOR" inst~>cellName) then        
        dbReplaceProp( inst "a_poly_contact" "string" "none" )
        dbReplaceProp( inst "b_poly_contact" "string" "none" )
         
      )
      if( rexMatchp("gate.INV" inst->cellName) then
        if( PCellGetParameterValue( inst "Folds" )<2 then
          dbReplaceProp( inst "poly_contact" "string" "none" ) 
        else 
          dbReplaceProp( inst "poly_contact" "string" "both" ) 
        )
      )
    )
  )
)

