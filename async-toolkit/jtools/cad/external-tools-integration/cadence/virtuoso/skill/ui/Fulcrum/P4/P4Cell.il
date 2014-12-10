
( list "P4 Cell Components" "UIP4WholeCell" )

( UICreateMenuItemCallBack 
  `UIP4WholeCellCB
  (lambda ()    
    ( hiDisplayForm UIP4WholeCell_Form ) ) 
)

defun( P4WholeCell ( cellView p4action)
  let( (cv instances uniqueInstances pgname busname autobusname basename subtype client nraction)

   (when UIP4WholeCell_Form->UIP4WholeCell_Form_floorplan->value
     cv = ( dbOpenCellViewByType cellView->libName cellView->cellName "floorplan" )
     (when cv ( CDSP4CellView cv p4action ))
   )
   (when UIP4WholeCell_Form->UIP4WholeCell_Form_prelayout->value
     cv = ( dbOpenCellViewByType cellView->libName cellView->cellName "prelayout" )
     (when cv ( CDSP4CellView cv p4action ))
   )
   (when UIP4WholeCell_Form->UIP4WholeCell_Form_layout->value
     cv = ( dbOpenCellViewByType cellView->libName cellView->cellName "layout" )
     (when cv ( CDSP4CellView cv p4action ))
   )
   (when UIP4WholeCell_Form->UIP4WholeCell_Form_layoutpg->value
     cv = ( dbOpenCellViewByType cellView->libName cellView->cellName "layout_pg" )
     (when cv ( CDSP4CellView cv p4action ))
   )
   (when UIP4WholeCell_Form->UIP4WholeCell_Form_abstract->value
     cv = ( dbOpenCellViewByType cellView->libName cellView->cellName "abstract" )
     (when cv ( CDSP4CellView cv p4action ))
   )

   basename = (cadr (reverse (parseString cellView->cellName ".")))
   subtype = (car (reverse (parseString cellView->cellName ".")))
   pgname = (strcat cellView->libName ".wires." basename "_" subtype "_POWER_GRID_TIEOFF")
   (when UIP4WholeCell_Form->UIP4WholeCell_Form_pg->value
     cv = ( dbOpenCellViewByType cellView->libName pgname "layout" )
     (when cv ( CDSP4CellView cv p4action ))
   )

   busname = (strcat cellView->libName ".wires." basename "_buswires")
   (when UIP4WholeCell_Form->UIP4WholeCell_Form_buswires->value
     cv = ( dbOpenCellViewByType cellView->libName busname "layout" )
     (when cv ( CDSP4CellView cv p4action ))
   )

   autobusname = (strcat cellView->libName ".wires." basename "_autobuswires")
   (when UIP4WholeCell_Form->UIP4WholeCell_Form_autobuswires->value
     cv = ( dbOpenCellViewByType cellView->libName autobusname "layout" )
     (when cv ( CDSP4CellView cv p4action ))
   )

   (when UIP4WholeCell_Form->UIP4WholeCell_Form_skill->value
     ( CDSP4CellFile cellView "wires.il" p4action )
   )

   (when UIP4WholeCell_Form->UIP4WholeCell_Form_wires->value
     client = ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
     (cond (p4action=="add" nraction="Add")
           (p4action=="edit" nraction="Edit")
           (p4action=="revert" nraction="Revert")
     )
     (foreach inst cellView->instances
       (when (IsCustomWiringCell inst)
          cv = (dbOpenCellViewByType inst->libName inst->cellName inst->viewName)
          (nrP4EditSubcells cv cv->viewName client nraction)
       )
     )     
   )

  )
)


( UICreateAction
  'UIP4WholeCellFormCB
  (lambda ()
    ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
 
    (let (CellView pinLpps)
    CellView = UIGetCellView( )
    P4WholeCell( CellView 
                  UIP4WholeCell_Form->UIP4WholeCell_Form_P4Action->value 
         )
    ) 
  ) 
)
    
( UICreateComponent
  "UIP4WholeCell_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateRadioField
                  ?name `UIP4WholeCell_Form_P4Action
                  ?prompt "P4 Action"
                  ?choices list( "add" "edit" "revert")
                  ?defValue "add" )
                 ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_floorplan
                  ?buttonText "floorplan view"
                  ?defValue nil )
               ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_prelayout
                  ?buttonText "prelayout view"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_layout
                  ?buttonText "layout view"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_layoutpg
                  ?buttonText "layout_pg view"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_abstract
                  ?buttonText "abstract view"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_pg
                  ?buttonText "POWER_GRID layout view"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_buswires
                  ?buttonText "buswires cell"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_autobuswires
                  ?buttonText "autobuswires cell"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_skill
                  ?buttonText "wires.il"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIP4WholeCell_Form_wires
                  ?buttonText "custom wiring cells"
                  ?defValue t )
                ) ) )
        ( hiCreateAppForm
          ?name `UIP4WholeCell_Form
          ?formTitle "P4 Cell Components"  
          ?fields Fields
          ?callback "( UIP4WholeCellFormCB )"
          ) ) )
    ) )
        
 
