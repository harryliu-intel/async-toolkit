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
--> Returns "CWD/cellname.pins.warn" with the pins drawn inplace from a leaf cell
--> Returns "CWD/cellname.pins.err" if a pin is missing in a subcell instance

    MidPinsInheritPin()
--> Promotes a pin from the subcell to top level
--> Returns the FigId of the created pin

    MidPinsScanBusPins()
--> Scans for the pins drawn with BusScript
--> Returns a list of the same

*/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( DrawMidLevelPins ( @key (CellVue (UIGetCellView)) )
  let( ( 
        CellTerms
        Term
        InstTerm
        CellPins
        BusPinList
        InPlacePinFig
        WarningFile
        ErrorFile
        Warnport
        Errport
       )

;    when( IsLeafCell(CellVue~>cellName)
;          error("This is a leaf cell. DrawLeafCellPins() should be used instead !")
;        )

    WarningFile = strcat(getWorkingDir() "/" CellVue~>cellName ".pins.warn")
    ErrorFile   = strcat(getWorkingDir() "/" CellVue~>cellName ".pins.err")

    warnport = outfile(WarningFile "w")
    Errport  = outfile(ErrorFile "w")

    CellTerms = CellVue~>terminals
    BusPinList = MidPinsScanBusPins(CellVue)

    foreach( Term CellTerms
       unless( (Term~>name == "GND") || (Term~>name == "Vdd")
            foreach( InstTerm Term~>net~>instTerms
;             when( InstTerm~>inst~>Protected
               unless( member(Term~>name BusPinList) || member(Term~>name CellPins)
                 InPlacePinFig = MidPinsInheritPin(CellVue InstTerm~>inst InstTerm~>name Term~>name)
                   when( InPlacePinFig~>LeafInPlacePin
              printf("*WARNING* Top Level pin \"%s\" is drawn inplace from leaf cell instance \"%s\" \n" Term~>name InstTerm~>inst~>name)
             fprintf(warnport "*WARNING* Top Level pin \"%s\" is drawn inplace from leaf cell instance \"%s\" \n" Term~>name InstTerm~>inst~>name)
                      )
                   when( !InPlacePinFig
              printf("*ERROR* Failed to make top pin \"%s\" due to missing pin \"%s\" inside instance \"%s\" \n" Term~>name InstTerm~>name InstTerm~>inst~>name)
             fprintf(Errport "*ERROR* Failed to make top pin \"%s\" due to missing pin \"%s\" inside instance \"%s\" \n" Term~>name InstTerm~>name InstTerm~>inst~>name) 
                       )
                 CellPins = cons(InPlacePinFig~>net~>name CellPins)
                    )
;                ) ; Checks if Instance is protected
              )
       )
     )
    close(warnport)
    close(Errport)
 )
t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( MidPinsInheritPin (CellVue Inst TermName PinName)
  let( (
          InPlacePinFig InPlacePinFigList
          InPlacePinTrf
          TopPinFig
          TopPinNet
          TopPin
          PinLabel PinShape PinList
        )

;     InPlacePinFigList = dbFindTermByName(Inst->master TermName)~>pins~>fig
;    hasty fix by stevemc because it wasn't getting all of the subcell pins
     PinList=dbFindTermByName(Inst->master TermName)~>pins
     foreach(pin PinList
       when(pin->fig
         InPlacePinFigList = list(pin->fig)
       )
     )

  when( !InPlacePinFigList
     InPlacePinFigList = setof(PinShape Inst->master~>shapes PinShape~>pin~>net~>name==TermName)
   )

  InPlacePinFig = car(setof(PinShape InPlacePinFigList !InValidPinLayer(PinShape)))

   when( InPlacePinFig
     InPlacePinTrf = dbTransformBBox(InPlacePinFig~>bBox Inst~>transform)
     TopPinFig = dbCreateRect(CellVue InPlacePinFig~>lpp InPlacePinTrf)
     TopPinNet = dbMakeNet(CellVue PinName)
     TopPin = dbCreatePin(TopPinNet TopPinFig)
     dbSetq( TopPin ( list "top" "bottom" "left" "right" ) accessDir )
     PinLabel = MidPinsCreateLabel(CellVue TopPinFig)

     PinLabel~>parent = TopPinFig

     if( (InPlacePinFig~>PinType == "BusScript") || (InPlacePinFig~>BusPin == "TRUE")
         then
           MidPinsSetPinType(TopPinFig "bus")
         else
           MidPinsSetPinType(TopPinFig "leaf")
        )
      )
TopPinFig
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( InValidPinLayer (PinShape)
  (car(PinShape~>lpp) == "CO" || car(PinShape~>lpp) == "PO" || car(PinShape~>lpp) == "M1")
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
defun( MidPinsScanBusPins (CellVue)
  let( (
          BusPinList
          PinShape
        )

   foreach( PinShape CellVue~>shapes

      when( PinShape~>pin~>net~>name && (PinShape~>PinType == "BusScript")
          BusPinList = cons(PinShape~>pin~>net~>name BusPinList)
          dbReplaceProp(PinShape "BusInPlacePin" "boolean" "TRUE")
        )
    )
BusPinList
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( MidPinsCreateLabel (CellVue PinShape)
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
    PinLabel = dbCreateLabel(CellVue PinShape~>lpp LwLtX+PinWd/2:LwLtY+PinHt/2 LabelName "centerCenter" LabelRot "stick" LabelHt)

    PinLabel
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RefreshPinDatabase ( @key (CellVue (UIGetCellView)) )
 let( (TermPin CellTerms Term)

     CellTerms = CellVue~>terminals

    foreach( Term CellTerms
      unless( (Term~>name == "GND") || (Term~>name == "Vdd")
            foreach( TermPin Term~>pins when(TermPin!=nil dbDeleteObject(TermPin)) )
       )
     )
  t
 )
)
