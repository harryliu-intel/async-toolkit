; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "RouteClean" "UIRouteClean" )

( UICreateMenuItemCallBack 
  `UIRouteCleanCB
  (lambda ()
    ( hiDisplayForm UIRouteClean_Form ) ) )

( UICreateAction
  'UIRouteCleanFormCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) )
          ( DirectiveTable
            ( CellInfoGetTableForCellName
              "null"
              ( sprintf 
                nil
                "%s/share/Fulcrum"
                ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
              ) )

          ( Area
            (when UIRouteClean_Form->UIRouteClean_Form_Area->value
              ( enterBox ?prompts ( list  "Select area to clean" ) ) ) )
          )

      (cond (
             ( PinUtilFindPRBoundShape BoundaryLPP CellView )
      (let (
            ( MetalLPPs
              ( append
                (if  UIRouteClean_Form->UIRouteClean_Form_Metal1->value
                    ( list Metal1LPP ) )
                ( append
                  (if  UIRouteClean_Form->UIRouteClean_Form_Metal2->value
                      ( list Metal2LPP ) )
                  ( append
                    (if  UIRouteClean_Form->UIRouteClean_Form_Metal3->value
                        ( list Metal3LPP ) )
                    ( append
                      (if  UIRouteClean_Form->UIRouteClean_Form_Metal4->value
                          ( list Metal4LPP ) )
                      ( append
                        (if  UIRouteClean_Form->UIRouteClean_Form_Metal5->value
                            ( list Metal5LPP ) )
                        ( append
                          (if  UIRouteClean_Form->UIRouteClean_Form_Metal6->value
                              ( list Metal6LPP ) )
                          (if  UIRouteClean_Form->UIRouteClean_Form_Metal7->value
                              ( list Metal7LPP ) ) ) ) ) ) ) ) ) )
        
        
;        (when UIRouteClean_Form->UIRouteClean_Form_OnGrid->value
;          (let (
;                ( Command
;                  ( sprintf 
;                    nil
;                    "%s/cast2skill --cast-path=%s --cell=%s --output-dir=%s --cadence-name --suppress-pins --root-only"
;                    ( PackageGetBinRoot )
;                    ( ConfigFileGetValue TheCDSConfigTable "CAST_PATH" )
;                    "standard.null.NULL"
;                    ( ConfigFileGetValue TheCDSConfigTable "CDS_CACHE_DIR" )
;                    ) ) )
;            ( printf "%s\n" Command )
;            ( shell Command ) )
;
;          ( KeepOutDrawKeepOut
;            CellView
;            ( CellInfoGetTableForCellName
;              "null"
;              ( sprintf
;                nil
;                "%s/ildirectives"
;                ( ConfigFileGetValue
;                  TheCDSConfigTable
;                  "CDS_CACHE_DIR" ) ) )
;            ( mapcar
;              (lambda ( LPP )
;                ( KeepOutCreateRoutingLayerStruct LPP ?Purpose "dummy" ) )
;              ( ListIntersect HorizontalMetalLPPs MetalLPPs ) )
;            ( mapcar
;              (lambda ( LPP )
;                ( KeepOutCreateRoutingLayerStruct LPP ?Purpose "dummy" ) )
;              ( ListIntersect VerticalMetalLPPs MetalLPPs ) )
;            BoundaryLPP
;            UserUnitsPerMeter
;            DirectiveUnitsPerMeter
;            ( list GNDNetName VddNetName )
;            ( list nil )                  ;    nil
;            t
;            nil
;            0
;            ?Area Area
;            ) )

        ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
        ( NotchesFillNotchesOnCellView
          CellView

          ( append
            (if UIRouteClean_Form->UIRouteClean_Form_NWell->value
                ( list ( list "nwellFix" NWellLPP ) ) )
          ( append
            (if UIRouteClean_Form->UIRouteClean_Form_NWell->value
                ( list ( list "npFix" NImplantLPP ) ) )
          ( append
            (if UIRouteClean_Form->UIRouteClean_Form_NWell->value
                ( list ( list "ppFix" PImplantLPP ) ) )
            ( append
              (if UIRouteClean_Form->UIRouteClean_Form_Poly->value
                  ( list ( list "polyfix" PolyLPP ) 
                         ( list "notchpoly" PolyLPP ) ) )
              ( append
                (if UIRouteClean_Form->UIRouteClean_Form_AreaFix->value
                    ( mapcar
                      (lambda ( MetalLPP )
                        ( rexCompile "M" )
                        ( list ( rexReplace ( car MetalLPP ) "area" 0 )
                               MetalLPP ) )
                      MetalLPPs ) )
                ( append
                  (if UIRouteClean_Form->UIRouteClean_Form_Notch->value                      
                      ( mapcar
                        (lambda ( MetalLPP )
                          ( rexCompile "M" )
                          ( list ( rexReplace ( car MetalLPP ) "notch" 0 )  MetalLPP ) )
                        MetalLPPs ) )
                  (if UIRouteClean_Form->UIRouteClean_Form_Extension->value                                      
                      ( mapcar
                        (lambda ( MetalLPP )
                          ( rexCompile "M" )
                          ( list ( rexReplace ( car MetalLPP ) "extension" 0 )  MetalLPP ) )
                        MetalLPPs ) ) ) ) ) ) ) )

            ( append 
              ( mapcar 
                (lambda ( MetalLPP )
                  ( car MetalLPP ) )
                MetalLPPs )
            ( append
              ( setof 
                Set
                ( list 
                  (if UIRouteClean_Form->UIRouteClean_Form_Poly->value
                      "POLY" )
                  (if UIRouteClean_Form->UIRouteClean_Form_AreaFix->value
                      "AREA" )
                  (if UIRouteClean_Form->UIRouteClean_Form_Notch->value
                      "NOTCH" )
                  (if UIRouteClean_Form->UIRouteClean_Form_Extension->value
                      "EXTENSION" )
                  (if UIRouteClean_Form->UIRouteClean_Form_NImp->value
                      "NIMP" )
                  (if UIRouteClean_Form->UIRouteClean_Form_PImp->value
                      "PIMP" )
                  (if UIRouteClean_Form->UIRouteClean_Form_NWell->value
                      "NWELL" ) )
                ( stringp Set ) )
              ( append
                (if UIRouteClean_Form->UIRouteClean_Form_OnGrid->value
                    ( list
                      "M2_AREA_KEEPIN"
                      "M3_AREA_KEEPIN"
                      "M4_AREA_KEEPIN"
                      "M5_AREA_KEEPIN"
                      "M6_AREA_KEEPIN"
                      "M7_AREA_KEEPIN" ) )
                (if  ( equal 
                       ( VersionGetMajorVersion )
                       "4" )
                    ( list "VERSION_4X" ) ) ) ) )

          ( sprintf 
            nil
            "%s/share/Fulcrum/notch/fill_notches.assura.rules.all" 
            ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
          ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
          ?Area Area
          ?NoPCells UIRouteClean_Form->UIRouteClean_Form_NoPCells->value
          )
        ( foreach LPP
                  ( mapcar
                    (lambda ( LPP )
                      ( list ( car LPP ) "dummy" ) )
                    MetalLPPs )
                  ( foreach Shape ( PinUtilGetAllShapesOnLPP CellView LPP )
                            ( dbDeleteObject Shape ) ) )
        ) )
       (
        ( printf "No prBoundary shape!!!\n" ) )

        ) ) ) )



    ( UICreateComponent
      "UIRouteClean_Form"
      ( UICreateConstructor 
        (lambda ( Children )
          (let (
                ( Fields
                  ( list             
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Area
                      ?buttonText "Select Area"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_NoPCells
                      ?buttonText "Don't export gates/stacks"
                      ?defValue nil )
;                    ( hiCreateBooleanButton
;                      ?name `UIRouteClean_Form_OnGrid
;                      ?buttonText "On Grid"
;                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Notch
                      ?buttonText "Notch"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_AreaFix
                      ?buttonText "Area"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Extension
                      ?buttonText "Extension"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Poly
                      ?buttonText "Poly"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_NWell
                      ?buttonText "NWell"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_NImp
                      ?buttonText "N Implant"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_PImp
                      ?buttonText "P Implant"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Metal1
                      ?buttonText "Metal 1"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Metal2
                      ?buttonText "Metal 2"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Metal3
                      ?buttonText "Metal 3"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Metal4
                      ?buttonText "Metal 4"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Metal5
                      ?buttonText "Metal 5"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Metal6
                      ?buttonText "Metal 6"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean_Form_Metal7
                      ?buttonText "Metal 7"
                      ?defValue t )
                    ( hiCreateFormButton
                      ?name `UIRouteClean_Form_Help
                      ?buttonText "Tool Help"
                      ?callback "nrOpenHtml( \"Clean_Base_Layers_help.html\" )" )
                    ) ) )
            ( hiCreateAppForm
              ?name `UIRouteClean_Form
              ?formTitle "Route Clean"  
              ?fields Fields
              ?callback "( UIRouteCleanFormCB )"
            ) 
      hiSetFormSize( UIRouteClean_Form list(601 630))
      ) 
    ) 
  ) 
)

