; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;
; menu item Nano/Prelayout
;
( list "1.Create Prelayout Cellviews..." "UICreatePrelayout" )

( UICreateMenuItemCallBack
  `UICreatePrelayoutCB
  (lambda ()
    hiDisplayForm( UICreatePrelayout_Form ) 
  ) 
)

defun( nrCreatePrelayout (libName cellName viewName prelayoutViewName
                         @key
                         ( routedDirectiveList nil )
                         ( Verbose nil )
                         ( RegenerateFromCast nil )
                         ( Overwrite nil )
                         ( SyncToNetlist t )
                         )
  let( (CellView PrelayoutView LayoutView LibCellsToIgnore routedStatus 
        transform InstName NewMasterCellView)
    LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs ) ) )
    if( nrIsRoutedCell( libName cellName viewName
          ?routedDirectiveList routedDirectiveList
          ?Verbose Verbose
          ?RegenerateFromCast RegenerateFromCast) then
      CellView=nrOpenCellViewReadable(libName cellName "layout") || nrOpenCellViewReadable(libName cellName "prelayout" ) || 
        nrOpenCellViewReadable(libName cellName "floorplan") 
    else
      PrelayoutView=nrOpenCellViewReadable(libName cellName prelayoutViewName)
      if( !PrelayoutView || ( Overwrite && !member( cellName processedCellList) ) then
        CellView=nrOpenCellViewReadable(libName cellName viewName)
        PrelayoutView=betterCopyCellView(CellView libName cellName 
          prelayoutViewName nil nil Overwrite)
        if( PrelayoutView then
          DeletePins( ?cv PrelayoutView )
          CopyAlignmentProps( CellView PrelayoutView ) 
          instances= car( NameFilterInstances( PrelayoutView~>instances LibCellsToIgnore ))
          foreach( inst instances
            InstName=inst~>name
            transform=inst~>transform
            NewMasterCellView=nrCreatePrelayout(inst~>libName inst~>cellName inst~>viewName prelayoutViewName
                         ?routedDirectiveList routedDirectiveList
                         ?Verbose Verbose
                         ?RegenerateFromCast RegenerateFromCast
                         ?Overwrite Overwrite 
                         ?SyncToNetlist SyncToNetlist
                         )
            dbDeleteObject( inst )
;            dbCreateInst(
            CreateInstWithAlignmentProps(
              PrelayoutView
              NewMasterCellView
              InstName
              car( transform )
              cadr( transform ) )
          )
          dbSave(PrelayoutView)
          printf("Creating %s(%s)...\n" PrelayoutView~>cellName PrelayoutView~>viewName)
          processedCellList=cons( PrelayoutView~>cellName processedCellList )
          if( SyncToNetlist nrSyncToNetlist(PrelayoutView))
          dbSave(PrelayoutView)
        else
          PrelayoutView=nrOpenCellViewReadable(libName cellName 
            prelayoutViewName)
        )
      )
      CellView=PrelayoutView
      SetCellAlignment(CellView)
      when( CheckRoutedAlignment(CellView)
        printf("There are already cell misalignments inherited from the floorplan.")
      )
    )
    CellView  
  )
)

 
( UICreateAction
  'UICreatePrelayoutFormCB
  (lambda ()
    let( (CellView)
    CellView= UIGetCellView()
    processedCellList=nil
    nrCreatePrelayout(CellView~>libName CellView~>cellName CellView~>viewName 
        UICreatePrelayout_Form->UICreatePrelayout_ViewName->value
        ?routedDirectiveList nrCastQuery( CellView 
             ?RegenerateFromCast UICreatePrelayout_Form->UICreatePrelayout_RegenerateFromCast->value)
        ?Verbose UICreatePrelayout_Form->UICreatePrelayout_Verbose->value
        ?RegenerateFromCast UICreatePrelayout_Form->UICreatePrelayout_RegenerateFromCast->value
        ?Overwrite UICreatePrelayout_Form->UICreatePrelayout_Overwrite->value
        ?SyncToNetlist UICreatePrelayout_Form->UICreatePrelayout_SyncToNetlist->value
    )
    )
  )
)

( UICreateComponent
  "UICreatePrelayout_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
           ( hiCreateStringField
             ?name `UICreatePrelayout_ViewName
             ?prompt "Prelayout View Name"
             ?defValue "prelayout" )
           ( hiCreateBooleanButton
             ?name `UICreatePrelayout_SyncToNetlist
             ?buttonText "Run Sync To Netlist on created Prelayout"
             ?defValue t )
           ( hiCreateBooleanButton
             ?name `UICreatePrelayout_Overwrite
             ?buttonText "Overwrite Existing Prelayout"
             ?defValue nil )
           ( hiCreateBooleanButton
             ?name `UICreatePrelayout_RegenerateFromCast
             ?buttonText "Regenerate Routed Directive From Cast"
             ?defValue t )
           ( hiCreateBooleanButton
             ?name `UICreatePrelayout_Verbose
             ?buttonText "Verbose"
             ?defValue nil )
           ( hiCreateFormButton
             ?name `UICreatePrelayout_Help
             ?buttonText "Tool Help"
             ?callback "nrOpenHtml( \"Create_Prelayout_Cellviews_help.html\" )" )
          )
        ( hiCreateAppForm
          ?name `UICreatePrelayout_Form
          ?formTitle "Create Prelayout Cellviews"  
          ?fields Fields
          ?callback "( UICreatePrelayoutFormCB )"
        ) 
      ) 
    ) 
  ) 
)
