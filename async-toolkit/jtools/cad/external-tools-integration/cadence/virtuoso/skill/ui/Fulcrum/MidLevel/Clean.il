

; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/Route/Clean.il#1 $
; $DateTime: 2011/10/03 13:24:55 $
; $Author: aubrey $

( list "5. RouteClean" "UIMidRouteClean" )

( UICreateMenuItemCallBack 
  `UIMidRouteCleanCB
  (lambda ()
    ( hiDisplayForm UIMidRouteClean_Form ) ) )

( UICreateAction
  'UIMidRouteCleanFormCB
  (lambda ()
    (let ( left bottom right top
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
            (when UIMidRouteClean_Form->UIMidRouteClean_Form_Area->value
              ( enterBox ?prompts ( list  "Select area to clean" ) ) ) )
          )
	left=round(caar(GetPrbound(CellView)->bBox)*1000)
	bottom=round(cadar(GetPrbound(CellView)->bBox)*1000)
	right=round(caadr(GetPrbound(CellView)->bBox)*1000)
	top=round(cadadr(GetPrbound(CellView)->bBox)*1000)
        dbCreateRect(CellView BoundaryLPP list(left/1000.0:bottom/1000.0 right/1000.0:top/1000.0))
      (cond (
             ( PinUtilFindPRBoundShape BoundaryLPP CellView )
      (let (
            ( MetalLPPs
              ( append
                (if  UIMidRouteClean_Form->UIMidRouteClean_Form_Metal1->value
                    ( list Metal1LPP ) )
                ( append
                  (if  UIMidRouteClean_Form->UIMidRouteClean_Form_Metal2->value
                      ( list Metal2LPP ) )
                  ( append
                    (if  UIMidRouteClean_Form->UIMidRouteClean_Form_Metal3->value
                        ( list Metal3LPP ) )
                    ( append
                      (if  UIMidRouteClean_Form->UIMidRouteClean_Form_Metal4->value
                          ( list Metal4LPP ) )
                      ( append
                        (if  UIMidRouteClean_Form->UIMidRouteClean_Form_Metal5->value
                            ( list Metal5LPP ) )
                        ( append
                          (if  UIMidRouteClean_Form->UIMidRouteClean_Form_Metal6->value
                              ( list Metal6LPP ) )
                          (if  UIMidRouteClean_Form->UIMidRouteClean_Form_Metal7->value
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
            (if UIMidRouteClean_Form->UIMidRouteClean_Form_NWell->value
                ( list ( list "nwellFix" NWellLPP ) ) )
          ( append
            (if UIMidRouteClean_Form->UIMidRouteClean_Form_NWell->value
                ( list ( list "npFix" NImplantLPP ) ) )
          ( append
            (if UIMidRouteClean_Form->UIMidRouteClean_Form_NWell->value
                ( list ( list "ppFix" PImplantLPP ) ) )
            ( append
              (if UIMidRouteClean_Form->UIMidRouteClean_Form_Poly->value
                  ( list ( list "polyfix" PolyLPP ) 
                         ( list "notchpoly" PolyLPP ) ) )
              ( append
                (if UIMidRouteClean_Form->UIMidRouteClean_Form_AreaFix->value
                    ( mapcar
                      (lambda ( MetalLPP )
                        ( rexCompile "M" )
                        ( list ( rexReplace ( car MetalLPP ) "area" 0 )
                               MetalLPP ) )
                      MetalLPPs ) )
                ( append
                  (if UIMidRouteClean_Form->UIMidRouteClean_Form_Notch->value                      
                      ( mapcar
                        (lambda ( MetalLPP )
                          ( rexCompile "M" )
                          ( list ( rexReplace ( car MetalLPP ) "notch" 0 )  MetalLPP ) )
                        MetalLPPs ) )
                  (if UIMidRouteClean_Form->UIMidRouteClean_Form_Extension->value                                      
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
                  (if UIMidRouteClean_Form->UIRouteClean_Form_Poly->value
                      "POLY" )
                  (if UIMidRouteClean_Form->UIMidRouteClean_Form_AreaFix->value
                      "AREA" )
                  (if UIMidRouteClean_Form->UIMidRouteClean_Form_Notch->value
                      "NOTCH" )
                  (if UIMidRouteClean_Form->UIMidRouteClean_Form_Extension->value
                      "EXTENSION" )
                  (if UIMidRouteClean_Form->UIMidRouteClean_Form_NImp->value
                      "NIMP" )
                  (if UIMidRouteClean_Form->UIMidRouteClean_Form_PImp->value
                      "PIMP" )
                  (if UIMidRouteClean_Form->UIMidRouteClean_Form_NWell->value
                      "NWELL" ) )
                ( stringp Set ) )
              ( append
                (if UIMidRouteClean_Form->UIRouteClean_Form_OnGrid->value
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
          ?NoPCells UIMidRouteClean_Form->UIRouteClean_Form_NoPCells->value
          )
        ( foreach LPP
                  ( mapcar
                    (lambda ( LPP )
                      ( list ( car LPP ) "dummy" ) )
                    MetalLPPs )
                  ( foreach Shape ( PinUtilGetAllShapesOnLPP CellView LPP )
                            ( dbDeleteObject Shape ) ) )
	        foreach(SHAPE CellView-> shapes 
        	when( SHAPE~>layerName=="prBoundary"
		  dbDeleteObject(SHAPE)
                ))
        ) )
       (
        ( printf "No prBoundary shape!!!\n" ) )
        ) ) ) )



    ( UICreateComponent
      "UIMidRouteClean_Form"
      ( UICreateConstructor 
        (lambda ( Children )
          (let (
                ( Fields
                  ( list             
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_Area
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
                      ?name `UIMidRouteClean_Form_Notch
                      ?buttonText "Notch"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_AreaFix
                      ?buttonText "Area"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_Extension
                      ?buttonText "Extension"
                      ?defValue nil )
;                    ( hiCreateBooleanButton
;                      ?name `UIRouteClean_Form_Poly
;                      ?buttonText "Poly"
;                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_NWell
                      ?buttonText "NWell"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_NImp
                      ?buttonText "N Implant"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_PImp
                      ?buttonText "P Implant"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_Metal1
                      ?buttonText "Metal 1"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_Metal2
                      ?buttonText "Metal 2"
                      ?defValue nil )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_Metal3
                      ?buttonText "Metal 3"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_Metal4
                      ?buttonText "Metal 4"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_Metal5
                      ?buttonText "Metal 5"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_Metal6
                      ?buttonText "Metal 6"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIMidRouteClean_Form_Metal7
                      ?buttonText "Metal 7"
                      ?defValue t )
                    ( hiCreateFormButton
                      ?name `UIRouteClean_Form_Help
                      ?buttonText "Tool Help"
                      ?callback "nrOpenHtml( \"Clean_Base_Layers_help.html\" )" )
                    ) ) )
            ( hiCreateAppForm
              ?name `UIMidRouteClean_Form
              ?formTitle "Route Clean"  
              ?fields Fields
              ?callback "( UIMidRouteCleanFormCB )"
            ) 
      hiSetFormSize( UIMidRouteClean_Form list(601 630))
      ) 
    ) 
  ) 
)
