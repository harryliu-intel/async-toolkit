; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/CreateCDL.il#11 $
; $DateTime: 2004/08/21 16:39:54 $
; $Author: clayton $


( list "Check Subcell Connectivity" "UICheckSubcell" )

( UICreateMenuItemCallBack 
  `UICheckSubcellCB
  (lambda ()
  (let ( CellView subCellView reportList uniqueInstances instances subCellView shapes)
    CellView= UIGetCellView()
    LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs ) ) )
    instances= car( NameFilterInstances( CellView~>instances LibCellsToIgnore ))          
    uniqueInstances=nil
    foreach( inst instances
      if( !member( inst~>cellName uniqueInstances~>cellName ) then 
        uniqueInstances=cons( inst uniqueInstances )
      )
    )
    reportList=nil
    foreach( inst uniqueInstances
      subCellView=nrOpenCellViewReadable( inst~>libName inst~>cellName inst~>viewName )
      shapes=setof( shape subCellView~>shapes 
        strncmp( shape~>layerName "METAL" 5)==0 && 
       (shape~>purpose=="drawing" || shape~>purpose=="pin") && shape~>objType!="label")
      totalMetalShapes=length(shapes)
      FullViaWidth=(ContactExtension *2 + ContactCutWidth)*MicronsPerMeter
      shapes=setof( shape shapes defRectArea(shape~>bBox)>FullViaWidth*FullViaWidth*1.01 )
      shapes=setof( shape shapes shape~>net==nil )
      noConnectivityShapes=length(shapes)
      reportList=cons( list( inst~>cellName totalMetalShapes noConnectivityShapes) reportList)
    )
    UICheckSubcell_Form->UICheckSubcell_Report->choices=reportList
     hiDisplayForm( UICheckSubcell_Form )     
  )
  )
)
( UICreateComponent
  "UICheckSubcell_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list( list(
                ( hiCreateReportField
                  ?name 'UICheckSubcell_Report
                  ?headers '(("Cell Name" 270 'left 'string t 'strcmp) 
                     ("Total Metal Shapes" 150 'left 'int t lambda((x y) cond((x>y 1)(x<y -1)(t 0))) )
                     ("Unconnected Shapes" 150 'left 'int t lambda((x y) cond((x>y 1)(x<y -1)(t 0))) ))
                  ?choices list(list("foo" 1 1 ))
                 ) 0:0 600:350)
          )
        ( hiCreateAppForm
          ?name `UICheckSubcell_Form
          ?formTitle "Check Subcell Connectivity"  
          ?fields Fields
          ) 
      ) 
    ) 
  ) 
)
