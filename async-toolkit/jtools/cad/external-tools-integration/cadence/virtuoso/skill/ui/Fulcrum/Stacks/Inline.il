; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/Stacks/Stack.il#1 $
; $DateTime: 2005/11/07 15:21:21 $
; $Author: aubrey $


( list "Inline Gate to Stacks" "UIInlineGates" )

defun( khWithinBBox (rect bBox)
; test if rect is inside bBox
    leftEdge(rect)>=leftEdge(bBox) && 
    rightEdge(rect)<=rightEdge(bBox) &&
    bottomEdge(rect)>=bottomEdge(bBox) && 
    topEdge(rect)<=topEdge(bBox)
)


defun( khConnectShapes ( fig allFigs )
; propagates connectivy for fig to all connecting shapes
  let((shapes shape filterLPPs)
    cond(
      (fig~>lpp==Metal2LPP filterLPPs=list( Metal2LPP Via23LPP Via12LPP ))
      (fig~>lpp==Via12LPP filterLPPs=list( Metal1LPP Metal2LPP ))
      (fig~>lpp==Metal1LPP filterLPPs=list( Metal1LPP ContactLPP Via12LPP ))
      (fig~>lpp==ContactLPP filterLPPs=list( Metal1LPP PolyLPP ))
      (fig~>lpp==PolyLPP filterLPPs=list( ContactLPP PolyLPP ))
    )
    shapes=setof( shape dbGetTrueOverlaps( fig~>cellView fig~>bBox ) shape~>net==nil && member( shape~>lpp filterLPPs) && member( shape allFigs ))
    foreach( shape shapes
      dbAddFigToNet( shape fig~>net)
      khConnectShapes( shape allFigs )        
    )
  )
)

defun( khConnectInstances ( instances allFigs )
; connects instTerms of instances with shape that overlaps and has connectivity
  let((inst instTerm shapes shape net fig pinRect)
    foreach( inst instances
      foreach( instTerm inst~>instTerms
        net=nil
        foreach( pin instTerm~>term~>pins
          fig=pin~>fig
          pinRect=dbTransformBBox( fig~>bBox inst~>transform )
          shapes=setof( shape dbGetTrueOverlaps( inst~>cellView pinRect ) shape~>net!=nil && shape~>lpp==fig~>lpp && member( shape allFigs ))
          if(shapes then 
            net=car(shapes)~>net
            dbCreateConn(net inst instTerm~>term)
          )
        )
        if( net!=nil then
        ; feed through connectivity to all other shapes that overlap instTerms
          foreach( pin instTerm~>term~>pins
            fig=pin~>fig
            pinRect=dbTransformBBox( fig~>bBox inst~>transform )
            shapes=setof( shape dbGetTrueOverlaps( inst~>cellView pinRect ) shape~>net==nil && shape~>lpp==fig~>lpp && member( shape allFigs ))
            foreach( shape shapes
              dbAddFigToNet( shape net)
              khConnectShapes( shape allFigs )
            )
          )
        )
      )
    )
  )
)
defun( khCreateInstTerms ( instances )
; create instTerms for instances
  let((inst term)
    foreach( inst instances
      foreach( term inst~>master~>terminals
        if( term~>name!=GNDNetName && term~>name!=VddNetName then
          dbCreateInstTerm( nil inst term )
        )
      )
    )
  )
)

defun( khInlineGateToStacks ( CellView inst 
                         @key 
                         ( DeletePcellWiring nil )
                         )
  let((tempPinRectList term bBox instName pinRect pin stacks pstacks nstacks count figs obj allFigs)
    tempPinRectList=nil
    foreach( term inst~>instTerms
      foreach( pin term~>term~>pins
        pinRect=dbCreateRect( CellView pin~>fig~>lpp dbTransformBBox(pin~>fig~>bBox inst~>transform))
        tempPinRectList=cons( pinRect tempPinRectList )
        dbCreatePin( term~>net pinRect)
      )
    )
    instName=inst~>name
    bBox=inst~>bBox
    leFlattenInst( inst 1 t nil )
    stacks=setof( obj dbGetTrueOverlaps( CellView bBox ) obj~>objType=="inst" && obj~>libName=="stack" )
    pstacks=cadr( NameFilterInstances( stacks PChainLibCellPairRegExs))
    nstacks=cadr( NameFilterInstances( stacks NChainLibCellPairRegExs))
    count=1
    foreach( obj pstacks
      if(rexMatchp("I[0-9]*" obj~>name) then
        obj~>name=sprintf(nil "%s.P%d" instName count)
        count=count+1
      )
    ) 
    count=1
    foreach( obj nstacks
      if(rexMatchp("I[0-9]*" obj~>name) then
        obj~>name=sprintf(nil "%s.N%d" instName count)
        count=count+1
      )
    )
    khCreateInstTerms( stacks )
    figs=setof( obj dbGetOverlaps( CellView bBox )  obj~>objType=="inst" && obj~>libName==TechLibName && khWithinBBox(obj~>bBox bBox))
    foreach( obj figs leFlattenInst( obj 1 t nil ))
    allFigs=setof( obj dbGetOverlaps( CellView bBox )  obj~>objType!="inst" && khWithinBBox(obj~>bBox bBox))
    foreach( pinRect tempPinRectList khConnectShapes( pinRect allFigs ))
    khConnectInstances( stacks allFigs )
    khConnectInstances( stacks allFigs )
    foreach( pinRect tempPinRectList dbDeleteObject( pinRect ))
    if( DeletePcellWiring then
      figs=setof( obj dbGetOverlaps( CellView bBox )  obj~>objType!="inst" && khWithinBBox(obj~>bBox bBox))
      foreach( obj figs dbDeleteObject( obj ))
    )
  )
)

( UICreateMenuItemCallBack 
  `UIInlineGatesCB
  (lambda ()
    ( hiDisplayForm UIInlineGates_Form ) ) )

( UICreateAction
  'UIInlineGatesFormCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) 
          ( Selected ( setof Fig ( geGetSelectedSet )
                             ( equal Fig->objType "inst" )
                             ) )
          instances inst 
         )
    instances= cadr( NameFilterInstances( Selected GateLibCellPairRegExs ))
    if( UIInlineGates_Form->UIInlineGates_Extent->value=="Entire Domain" then
      instances=cadr( NameFilterInstances( CellView~>instances GateLibCellPairRegExs ))
    )
    foreach( inst instances
      khInlineGateToStacks( CellView inst 
              ?DeletePcellWiring UIInlineGates_Form->UIInlineGates_DeletePcellWiring->value
              )
    ) 
 ) ) ) 
          
( UICreateComponent
  "UIInlineGates_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateCyclicField
                  ?name `UIInlineGates_Extent
                  ?choices ( list "Selected" "Entire Domain" )
                  ?defValue "Selected"
                  ?prompt "Gate..." )
                ( hiCreateBooleanButton
                  ?name `UIInlineGates_DeletePcellWiring
                  ?buttonText "Delete Pcell Wiring"
                  ?defValue nil )
                ) ) )
        ( hiCreateAppForm
          ?name `UIInlineGates_Form
          ?formTitle "Inline Gate to Stacks"  
          ?fields Fields
          ?callback "( UIInlineGatesFormCB )"
          ) ) ) 
    ) )
        
 
