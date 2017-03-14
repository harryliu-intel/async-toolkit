/**
 * INTEL TOP SECRET
 * Copyright 2002-2012 Intel Corporation. All Rights Reserved.
 * $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/pins/midlevelpins.il#0 $
 */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/*DOC

    DrawMidLevelPins()
--> Draws pins in a mid level cell
--> Exits with an error when run on a Leaf Cell
--> Returns "TEMP/cellname.pins.warn" with the pins drawn inplace from a leaf cell
--> Returns "TEMP/cellname.pins.err" if a pin is missing in a subcell instance

    MidPinsInheritPin()
--> Promotes a pin from the subcell to top level
--> Returns the FigId of the created pin

    MidPinsScanBusPins()
--> Scans for the pins drawn with BusScript
--> Returns a list of the same

*/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DrawMidLevelPins ( @key (CV (UIGetCellView)) (include_supply nil) (supply_topMetal -1))
  (let (TEMP
        WarningFile
        ErrorFile
        Warnport
        Errport
        CellTerms
        BusPinList
        InPlacePinFig)

    TEMP        = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WarningFile = (strcat TEMP "/" CV->cellName ".pins.warn")
    ErrorFile   = (strcat TEMP "/" CV->cellName ".pins.err")
    Warnport    = (outfile WarningFile "w")
    Errport     = (outfile ErrorFile "w")
    CellTerms   = (setof x CV->terminals include_supply || (member x->name PowerGroundNets)==nil)
    BusPinList  = (MidPinsScanBusPins CV)

    (foreach Term CellTerms
      (foreach InstTerm Term->net->instTerms
        (unless (member Term->name BusPinList)
          InPlacePinFig = (MidPinsInheritPin CV InstTerm->inst InstTerm->name Term->name
                                             ?topMetal supply_topMetal)
          (when InPlacePinFig->LeafInPlacePin
            (printf "*WARNING* Top Level pin \"%s\" is drawn inplace from leaf cell instance \"%s\" \n" Term->name InstTerm->inst->name)
            (fprintf Warnport "*WARNING* Top Level pin \"%s\" is drawn inplace from leaf cell instance \"%s\" \n" Term->name InstTerm->inst->name)
            )
          (when !InPlacePinFig && (member Term->name PowerGroundNets)!=nil
            (printf "*ERROR* Failed to make top pin \"%s\" due to missing pin \"%s\" inside instance \"%s\" \n" Term->name InstTerm->name InstTerm->inst->name)
            (fprintf Errport "*ERROR* Failed to make top pin \"%s\" due to missing pin \"%s\" inside instance \"%s\" \n" Term->name InstTerm->name InstTerm->inst->name) 
            )
          )
        )
      )
    (close Warnport)
    (close Errport)
    )
  t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Promote pins from a subcell to inplace pins for a given terminal.
; All pins on the top metal layer used by them get promoted.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MidPinsInheritPin (CV Inst TermName PinName @key (topMetal -1))
  (let (InPlacePinList
        transform
        fig
        TopPinFig
        TopPinNet
        TopPin
        PinLabel PinShape
        )

    ; candidate pin figures
    (foreach pin (dbFindTermByName Inst->master TermName)->pins
             InPlacePinList = (append InPlacePinList pin->figs)
             )

    ; limit the candidates to those on the top metal layer among the set for power nets
    (when (member PinName PowerGroundNets)!=nil
          (foreach fig InPlacePinList
                   (for m 0 TopMetal (when fig->layerName==Metal[m] topMetal=(max topMetal m)))
                   )
          InPlacePinList = (setof x InPlacePinList x->layerName==Metal[topMetal])
          )

    ; draw inplace pins
    (foreach fig InPlacePinList
      transform = (dbTransformBBox fig->bBox Inst->transform)
      TopPinFig = (dbCreateRect CV (list (car fig->lpp) "pin") transform)
      TopPinNet = (dbMakeNet CV PinName)
      TopPin    = (dbCreatePin TopPinNet TopPinFig)
      TopPin->accessDir = (list "top" "bottom" "left" "right" )
      PinLabel = (MidPinsCreateLabel CV TopPinFig)
      PinLabel->parent = TopPinFig
      (if fig->PinType=="BusScript" || fig->BusPin=="TRUE"
          (MidPinsSetPinType TopPinFig "bus")
          (MidPinsSetPinType TopPinFig "leaf")
          )
      )
    TopPinFig
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( MidPinsSetPinType (PinShape InPlacePinType)
    
    when( InPlacePinType == "leaf"

       dbSet(PinShape "InPlace" "PinType")
       dbReplaceProp(PinShape "LeafInPlacePin" "boolean" "TRUE")
      )

    when( InPlacePinType == "bus"

       dbSet(PinShape "InPlace" "PinType")
       dbReplaceProp(PinShape "BusInPlacePin" "boolean" "TRUE")
      )

t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( MidPinsScanBusPins (CV)
  let( (
          BusPinList
          PinShape
        )

   foreach( PinShape CV~>shapes

      when( PinShape~>pin~>net~>name && (PinShape~>PinType=="BusScript" || PinShape~>PinType==nil)
          BusPinList = cons(PinShape~>pin~>net~>name BusPinList)
        )
    )
BusPinList
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( MidPinsCreateLabel (CV PinShape)
  let( (
          UpRtX
          LwLtX
          UpRrY
          LwLtY
          PinHt
          PinWd
          LabelPt
          LabelName
          LabelRot
          LabelHt
          PinLabel
        )

     UpRtX =  caadr(PinShape~>bBox)
     LwLtX =   caar(PinShape~>bBox)
     UpRrY = cadadr(PinShape~>bBox)
     LwLtY =  cadar(PinShape~>bBox)

     PinHt = UpRrY - LwLtY
     PinWd = UpRtX - LwLtX

     LabelPt = list(LwLtX+PinWd/2 LwLtY+PinHt/2)
     LabelName = PinShape~>net~>name

     if( PinWd > PinHt
         then
           LabelRot = "R0"
           LabelHt  = PinHt/2
         else
           LabelRot = "R90"
           LabelHt  = PinWd/2
       )
    PinLabel = dbCreateLabel(CV list( car(PinShape~>lpp) "pin") LwLtX+PinWd/2:LwLtY+PinHt/2 LabelName "centerCenter" LabelRot "stick" LabelHt)

    PinLabel
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RefreshPinDatabase ( @key (CV (UIGetCellView)) )
  (foreach Term CV->terminals
           (unless (Term~>name == GNDNetName) || (Term~>name == VddNetName)
                   (foreach TermPin Term~>pins (when TermPin!=nil (dbDeleteObject TermPin)))
                   )
           )
  t
  )

(defun DrawSyncInPlacePins ( @key (CV (UIGetCellView)) )
 (let ( mapfile maplist  rtlname_parts pinmap
        apr      rtlname fixname_0     pin
        cmd     castname fixname_1     rtlparts
                         inplace_0     name
                         inplace_1     pin2

 )

  cmd = sprintf( nil "fulcrum --before 283418 generate_port_mapping --cast-path=%s --verilog-block=rtl --cell=%s > %s.portmap" 
                 ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" ) CV~>cellName CV~>cellName )
  (sh cmd)

  mapfile = infile( sprintf( nil "%s.portmap" CV~>cellName ) )
  apr     = dbFindAnyInstByName(CV "apr" )

  foreach( s CV~>shapes when( s~>SyncInPlacePin == "TRUE" dbDeleteObject(s) ) )

  while( gets( pinmap mapfile ) 
   maplist  = parseString( pinmap " \n" )
    rtlname = nth(0 maplist)
   castname = nth(1 maplist)
   (rexCompile ",")
   castname = (rexReplace castname "][" 0 )

   pin = MidPinsInheritPin(CV apr rtlname castname) 
   pin~>SyncInPlacePin = "TRUE"

   rtlparts = parseString( rtlname "[<>]" )

   when( length( rtlparts ) > 1 
     when( !pin 
       sprintf( name "%s<%s>" nth(0 rtlparts) nth(1 rtlparts) )
       pin2 = MidPinsInheritPin(CV apr name castname) 
       pin2~>SyncInPlacePin = "TRUE"
     )
    )
   when( !pin2 && !pin  printf( "ERROR: Cannot inherit %s pin, due to missing %s pin inside %s\n" castname rtlname apr~>cellName ) )
   pin = nil
   pin2 = nil

   )

close(mapfile)

 )
t
)
