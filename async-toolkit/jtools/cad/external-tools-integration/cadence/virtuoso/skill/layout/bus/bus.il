; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; global defaults
defchan_modtime = 0 ; store latest modtime of dfII/defchan.il
defchan_patterns = nil ; global list of patterns for DrawTracks
Startloadwires = 0
wires_modtime = makeTable("wires" 0)

(defun BusDefaults ()
  busContacts = t   ; draw the contacts
  busKeepout  = nil ; draw keepout instead of metal
  busTopMetal = nil ; use top metal in both directions
  busContactNum = 1 ; unique number for contact instance names
  busGuideInstName = "buswires" ; global default guideInstName
  busPatterns = nil ; list of additional name->pattern pairs
  busUseDefaultPatterns = t ; draw the standard patterns from the PDK
  bundled_channels = nil ; for bundled channel routing in fat.il
  bundled_bottom_layer = 3 ; bottom layer for bundled channel routing
  bundled_top_layer    = 7 ; top layer for bundled channel routing
  delete_redundant_pins = nil ; DeleteRedundantPins or just warn about them
  t
  )
(BusDefaults)

; return reference to guide instance given instance name
; if inst already defined, use that
(defun GetGuideInst (inst name @key (CV (geGetEditCellView)))
  (cond (inst!=nil inst)
        (name!=nil
         (dbFindAnyInstByName CV name))
        (busGuideInstName!=nil
         (dbFindAnyInstByName CV busGuideInstName))
        )
  )

; simple heuristic to check for guide instances or views
(defun IsGuideInst (inst)
  (let (part)
    part = (car (last (parseString inst->cellName "_")))
    (or part=="buswires"     inst->name=="buswires"
        part=="autobuswires" inst->name=="autobuswires")
    )
  )

(defun EditGuideInstInPlace ()
  (let (cell)
  cell=(GetGuideInst nil busGuideInstName)
  (geDeselectAllFig)
  (geSelectObject cell)
  (leHiEditInPlace)
))

(defun EditAutoGuideInstInPlace ()
  (let (cell)
  cell=(GetGuideInst nil "autobuswires")
  (geDeselectAllFig)
  (geSelectObject cell)
  (leHiEditInPlace)
))

; create a template guide instance
(defun MakeGuideInst (guidename instname @key (CV (geGetEditCellView)))
  (let (instview moveguides)

  (if (HasGuides CV) then
    moveguides = (hiDisplayAppDBox
                      ?name 'GuideOverwriteDbox
                      ?dboxBanner "Warning"
                      ?dboxText "This view has bus guides. Move them into buswires cell?"
                      ?dialogType hicQuestionDialog
                      ?buttonLayout 'YesNo
                      ?defaultButton 1
                     )
  )
  (if !(dbFindAnyInstByName CV instname) then
    instview = (nrOpenCellViewWritable CV~>libName guidename "layout" ?mode "a" )
    (when !instview
      (printf "Cellview %s is not writeable. Instantiating existing cell.\n" guidename)
    )
    inst = (dbCreateInstByMasterName CV CV~>libName guidename "layout" instname (list 0 0) "R0")
    (AnchorInstance inst)
    busGuideInstName = instname
    (when instview && moveguides
      (CopyGuides CV instview)
      (DeleteGuideShapes CV)
    )
    (when instview && !instview->shapes
      (dbCreateRect instview (list "prBoundary" "boundary") (list 0:0 TrackPitch:TrackPitch)) )
  else
    (printf "Instance name %s is already used." instname)
  )
  )
)

; copy paths or inst to prelayout view
(defun CopyBusGuidesToPrelayout (@key (CV (geGetEditCellView)))
  (let (prelayout overwrite)
    prelayout = (nrOpenCellViewWritable CV->libName CV->cellName "prelayout" ?mode "a" )
    (if !prelayout then
      (printf "Couldn't edit prelayout view.")
    else
      (if (HasGuides prelayout) then
        overwrite = (hiDisplayAppDBox
                      ?name 'GuideOverwriteDbox
                      ?dboxBanner "Warning"
                      ?dboxText "Prelayout view already has bus guides. Replace them?"
                      ?dialogType hicQuestionDialog
                      ?buttonLayout 'YesNo
                      ?defaultButton 1
                     )
        (when overwrite
          (DeleteGuides prelayout)
          (CopyGuides CV prelayout)
        )
      else
        (CopyGuides CV prelayout)
      )
    )
  )
)

(defun MoveGuidesToBuswiresCell (@key (CV (geGetEditCellView)))
  (let (wirescell wirescv)
    wirescell=(GetGuideInst nil busGuideInstName)
    (if wirescell then
      wirescv=(dbOpenCellViewByType 
       wirescell->libName wirescell->cellName wirescell->viewName "maskLayout" "a")
      (CopyGuideShapes CV wirescv)
      (DeleteGuideShapes CV)
    else
      (hiDisplayAppDBox
                      ?name 'NoBuswiresCellDbox
                      ?dboxBanner "Error"
                      ?dboxText "No buswires instance found."
                      ?dialogType hicQuestionDialog
                      ?buttonLayout 'Close
                      ?defaultButton 1
                     )
    )
  )
)


; create wires.il
(defun MakeWiresSkill (CellView instname defchans)
  (let (filename file skilltemplate defchantemplate)

    skilltemplate = (sprintf nil "\n   ; template wires skill for %s\n" CellView->cellName)
    skilltemplate = (strcat skilltemplate "\n   ; Add custom channel definitions here\n")
    skilltemplate = (strcat skilltemplate "\n(defun DrawAll (@key (CV (geGetEditCellView)))\n")
    skilltemplate = (strcat skilltemplate "busGuideInstName = \"")
    skilltemplate = (strcat skilltemplate instname)
    skilltemplate = (strcat skilltemplate "\"\n")
    skilltemplate = (strcat skilltemplate "\n   ; Add DefChans to be drawn here\n")
    skilltemplate = (strcat skilltemplate "   ; syntax- (DrawChannels nil YourNewChannel nil \"YourNewChannel\")\n")
    skilltemplate = (strcat skilltemplate "\n(DrawTracks)\n")
    skilltemplate = (strcat skilltemplate ";(DrawDefaultTracks)\n")
    skilltemplate = (strcat skilltemplate "(DrawObstructions)\n")
    skilltemplate = (strcat skilltemplate ")\n")

    defchantemplate = "(ReloadDefChans)\n"

    filename = strcat( "/"  
                      buildString( reverse( cddr( reverse( parseString( CellView~>fileName "/")))) "/") 
                      "/wires.il")
    (if (isFile filename) then
      (printf "File wires.il already exists. Won't overwrite.")
    else
    file = (outfile filename)

    (if !file then
      (printf "File %s is not writeable." filename)
    else
      (when defchans
        (fprintf file "%s" defchantemplate)
      )
      (fprintf file "%s" skilltemplate)
      (close file)
    ))
  )
)

; check for updates to defchan.il and reload if necessary
(defun ReloadDefChans ()
  (let (DFII_dir defchanfile )
  DFII_dir = ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
  defchanfile = (sprintf nil "%s/defchan.il" DFII_dir)
  modtime = (fileTimeModified defchanfile)
  (when modtime > defchan_modtime
    (printf "reloading defchan.il ... ")
    defchan_patterns = nil
    (load defchanfile)
    (printf "defchan.il reloaded.\n")
   )
   defchan_modtime = modtime
  )
)

; redraw everything
(defun RefreshAllBuswires (@key (CV (geGetEditCellView)))
  (DeleteBusWires ?CV CV)
  (DrawWires ?CV CV)
  (DeleteUnusedNets ?CV CV)
)

; redraw all pins in conjunction with inplace pins
(defun RedrawAllPins (@key (CV (geGetEditCellView)))
    (DeletePinsExcludeType "Power" ?CV CV)
    (DeletePinsByPurpose "pin" ?CV CV)
    (DeletePinsByType "Inplace" ?CV CV)
    (RefreshPinDatabase ?CV CV)
    (RefreshAllBuswires ?CV CV)
    (CanonicalizePins ?CV CV)
    (DrawMidLevelPins ?CV CV)
)

; Update layout_pg/layout view after editing bus wires without
; touching other wires.
(defun UpdateBusLayout (@key (CV (geGetEditCellView)))
  (unless CV->viewName=="layout_pg"
          (error "UpdateBusLayout should be run from layout_pg view\n"))
  (RedrawAllPins)
  (DeleteKeepout)
  (CanonicalizeNets)
  (DeleteRedundantPins ?warnOnly !delete_redundant_pins)
  (MakeLayout)
  )

(defun SetBusProp (obj)
  (when obj
    (dbReplaceProp obj "BusScriptObject" "boolean" t)
  ) 
)

(defun IsBusObject (obj)
 (let (isbus)
  isbus = nil
  (when obj
    (when (dbGetPropByName obj "BusScriptObject")->value=="TRUE"
      isbus=t 
    )
  )
  isbus 
 )
)


; draw an individual segment of a path
(defun DrawSegment (name layer width xy0 xy1 beginExt endExt isPin
                         @key (guideInst nil) (CV (geGetEditCellView)))
  (let (nxy0 nxy1 x0 y0 x1 y1 fig rect net label pin)
    nxy0 = (busTransformPoint xy0 guideInst)
    nxy1 = (busTransformPoint xy1 guideInst)
    x0 = (car nxy0)
    y0 = (cadr nxy0)
    x1 = (car nxy1)
    y1 = (cadr nxy1)
    fig = (dbCreatePath CV layer (list nxy0 nxy1) width "varExtendExtend")
    (SetBusProp fig)
    net = (MakeNet CV name)
    fig->beginExt = (if beginExt width/2 0)
    fig->endExt = (if endExt width/2 0)
    (if net!=nil fig->net = net)
    (cond (isPin
          (cond (y0==y1
                rect = (dbCreateRect CV layer list(x0:y0-width/2 x1:y1+width/2)) ; pins should be rectangles
                )
                (x0==x1
                rect = (dbCreateRect CV layer list(x0-width/2:y0 x1+width/2:y1)) ; pins should be rectangles
                )
                (t (error "invalid coordinates for pins %f:%f %f:%f" x0 y0 x1 y1) 
                )
          )
           (dbDeleteObject fig) ; delete the path
           pin = (dbCreatePin net rect) ; create a pin
           (SetBusProp rect)
           (dbReplaceProp rect "PinType" "string" "BusScript")
           pin->accessDir = (list "left" "right" "top" "bottom")
           pin->term->direction = "inputOutput"
           (LabelPin CV rect)
           )
          )
    )
  t
  )


; draw a via (use Nanoroute compatible vias)
(defun DrawVia (name viaList xy width cuts @key (doubleVias t) (verticalVias nil) (positiveVias nil) 
                     (guideInst nil) (stacked nil) (corrected nil) (CV (geGetEditCellView)))
  (let (inst net type extensions params contactOrientation layer1 layer2)

;	when(verticalVias==t println("YES"))

    ;convert to a list if not a list
    (if !listp(viaList) viaList = list(viaList))
    (foreach via viaList
    	(cond (guideInst!=nil contactOrientation = guideInst->orient)
          (guideInst==nil contactOrientation = "R0"))
    	(cond ((and busContacts (via!=nil))
           (if cuts=="" then
             type = via
             (if !doubleVias then
               nil
                else
                 type = (strcat via "_R_H") 
             )
            else type = (strcat via cuts)
           )
           extensions = (BusDefaultViaExtensions via
                         ?cuts cuts ?doubleVias doubleVias ?stacked stacked)
           params = (if extensions 
                        (list (list "layer1XEnclosure" "float" (car extensions))
                              (list "layer1YEnclosure" "float" (cadr extensions))
                              (list "layer2XEnclosure" "float" (caddr extensions))
                              (list "layer2YEnclosure" "float" (cadddr extensions))
                              )
                      nil
                      )
	
           inst = (dbCreateVia CV
                               (techFindViaDefByName TechLib type)
                               (busTransformPoint xy guideInst)
                               contactOrientation
                  )
           when( corrected
           layer2 = evalstring(car(parseString(nth(0 parseString(type "_")) "M")))
           layer1 = evalstring(car(parseString(nth(1 parseString(type "_")) "M")))
           if( evenp(layer1)
            then
              inst~>layer1Enc = 0.04:0.01
              inst~>layer2Enc = 0.04:0.01
            else
              inst~>layer1Enc = 0.04:0.01
              inst~>layer2Enc = 0.04:0.01
            )
           ) 
           (SetBusProp inst)
           net = (MakeNet CV name)
           (cond (net!=nil (dbAddFigToNet inst net)))
           )
        )
    ) ;foreach
    )
  t
  )

; Draw a multisegment path on two perpendicular layers with vias.
; (DrawWire metal67 "a" 0.36 (list 0:0 9.6:0 9.6:9.6 19.2:9.6))
(defun DrawWire (layers name width points
                        @key (isPin nil) (startVias nil) (endVias nil) (doubleVias t) (verticalVias nil) (positiveVias nil) 
                        (guideInst nil) (flipX nil) (flipY nil) (pattern evalstring("e1of4"))
                        (CV (geGetEditCellView)))
  (let (horz_layer vert_layer via viadir last first lastIndex index cuts layer hv dir prevdir templast (types (ClassifyPoints points)) (dirs (ClassifyDirs points)) corrected correctOP)
    corrected = nil
    horz_layer = (car layers)
    vert_layer = (cadr layers)
    via = (caddr layers)
    viadir = (cadddr layers)
    points = (CanonicalizePoints points)
    last = (car points)
    first = t
    lastIndex = (length points)-2
    index = 0
	when(!verticalVias
   cuts = (if (almostEqual (car (cadr points)) (car last)) "_R_V" "_R_H"))
	when(verticalVias
    cuts = (if verticalVias "_R_V" "_R_H"))

    (if startVias (DrawVia name via last width cuts ?doubleVias doubleVias ?verticalVias verticalVias ?positiveVias positiveVias 
                   ?guideInst guideInst ?stacked t ?CV CV))
    (foreach point (cdr points)
             hv = nth(index types)
             if( index > 0
              then 
               prevdir = nth(index-1 dirs)
               dir     = nth(index dirs)
             else
               prevdir = nil
               dir     = nth(index dirs)
             )
             layer = (if (almostEqual (car point) (car last)) vert_layer horz_layer)
             (DrawSegment name layer width last point
                          (index!=0) (index!=lastIndex) isPin ?guideInst guideInst ?CV CV)
	when(!verticalVias
             cuts = (if (and doubleVias viadir) (strcat "_R_" viadir) ""))
	when(verticalVias
             cuts = (if verticalVias "_R_V" "_R_H"))
             templast = last
             when( doubleVias && !verticalVias
                correctOP = DoubleViaCorrection(templast hv dir pattern flipX flipY)
                last = nth(0 correctOP)
                corrected = nth(1 correctOP)
              )
             when( doubleVias &&  verticalVias
                correctOP = VerticalViaCorrection(templast hv prevdir dir pattern flipX flipY positiveVias)
                last = nth(0 correctOP)
                corrected = nth(1 correctOP)
              )
             (if (not first) (DrawVia name via last width cuts
                                      ?doubleVias doubleVias ?guideInst guideInst ?verticalVias verticalVias ?positiveVias positiveVias 
                                      ?stacked nil ?corrected corrected ?CV CV))
             corrected = nil
             first = nil
	when(!verticalVias
            cuts = (if (almostEqual (car point) (car last)) "_R_V" "_R_H"))
	when(verticalVias
             cuts = (if verticalVias "_R_V" "_R_H"))
             last = point
             index = index+1
             )
    (if endVias (DrawVia name via last width cuts ?doubleVias doubleVias ?verticalVias verticalVias ?positiveVias positiveVias 
                         ?guideInst guideInst ?stacked t ?CV CV))
    )
  t
  )

; return list of which directions segments point connect to
(defun ClassifyPoints (points)
  (let (a b types)
    a = (car points)
    b = (cadr points)
    (cond ((almostEqual (car  a) (car  b)) types=(append types (list "EV")))
          ((almostEqual (cadr a) (cadr b)) types=(append types (list "EH"))))
    (for i 1 (length points)-2
         a = (nth i-1 points)
         b = (nth i   points)
         (cond ((almostEqual (car  a) (car  b)) types=(append types (list "VH")))
               ((almostEqual (cadr a) (cadr b)) types=(append types (list "HV"))))
         )
    a = (nth (length points)-2 points)
    b = (nth (length points)-1 points)
    (cond ((almostEqual (car  a) (car  b)) types=(append types (list "VE")))
          ((almostEqual (cadr a) (cadr b)) types=(append types (list "HE")))
          )
    types
    )
  )

; add suffix to a prefix (unless the prefix is empty, in which case result is too)
(defun AddSuffix (prefix suffix)
  (cond ((member suffix PowerNets) || prefix==nil suffix)
        (prefix!="" (sprintf nil "%s%s" prefix suffix))
        (prefix=="" "")
        )
  )

; Draw a channel bundle of wires.
; (DrawChannel metal67 e1of4 "a" (list 0:0 9.6:0 9.6:9.6 19.2:9.6 19.2:19.2 38.4:19.2))
(defun DrawChannel (layers pattern prefix points
                           @key (flipX nil) (flipY nil) (isPin nil)
                           (startVias nil) (endVias nil) (doubleVias t) (verticalVias nil) (positiveVias nil) 
                           (altLayerPatternList nil)
                           (guideInstName nil) (guideInst nil) (shieldM2 t) (drawShield nil)
                           (CV (geGetEditCellView)))
  (let (newpoints node width offset index x y hv
        offsetX offsetY nextOffset curOffset lastLayer curLayer tmpx tmpy
        curPattern nextLayer nextPattern stVias eVias segNum layer_override lastx lasty shieldDrawn
        (types (ClassifyPoints points)))

    ; set guide instance
    guideInst = (GetGuideInst guideInst guideInstName ?CV CV)
    shieldDrawn = nil

    ; go through all nodes in channel
    (foreach node_offset pattern
             node = (car node_offset)
             width = (cadr node_offset)
             offset = (caddr node_offset)
             layer_override = (cadddr node_offset)
             newpoints = nil
             index = 0
             segNum = 0 ; for multiple segment due to changing layers
             lastLayer = curLayer
             (foreach point points
                  x = (car point)
                  y = (cadr point)
                  hv = (nth index types)
                 (if altLayerPatternList then
                  ;altLayerPatternList either a list of patterns for each segments or a single one
                  ;if a single one it will be used for the second segment to the end
                  ;a nil entry in the altLayerPatternList will use the original layer and pattern
                  nextOffset = offset
                  curOffset = offset
                  curLayer = layers
                  (if length(altLayerPatternList) > 1
                  then
                      ;current pattern
                      (if index >= 2 && nth(1 nth(index-2 altLayerPatternList)) 
                      then
                          curLayerPattern = nth(index-2 altLayerPatternList)
                          curLayer =  nth(0 curLayerPattern )
                          curPattern = nth(1 curLayerPattern )
                          (foreach cur_node_offset curPattern
                              (if node == (car cur_node_offset) then
                                  curOffset  = (caddr cur_node_offset)
                              )
                          )
                      )
                      ;next pattern
                      (if index >= 1 && nth(1 nth(index-1 altLayerPatternList)) 
                      then
                          nextLayer = nth(0 nth(index-1 altLayerPatternList))
                          nextPattern = nth(1 nth(index-1 altLayerPatternList))
                          (foreach next_node_offset nextPattern
                              (if node == (car next_node_offset) then
                                  nextOffset  = (caddr next_node_offset)
                              )
                          )
                      )
                  else
                      ;single alternate pattern
                      altLayerPattern = nth(0 altLayerPatternList)
                      altLayer = nth(0 altLayerPattern)
                      altPattern = nth(1 altLayerPattern)
                      (foreach alt_node_offset altPattern
                          (if node == (car alt_node_offset) then
                              altOffset  = (caddr alt_node_offset)
                          )
                      )
                      (if index <= 1
                      then curLayer = layers nextLayer = (if (index == 0) layers altLayer) curOffset = offset nextOffset = altOffset
                      else curLayer = altLayer nextLayer = altLayer curOffset = altOffset nextOffset = altOffset
                      )
                  )
                  (if hv == "EH"  || hv == "HV" then
                      offsetX = nextOffset
                      offsetY = curOffset
                  else 
                      (if hv == "VE" || hv == "HE" then
                          offsetX = curOffset
                          offsetY = curOffset
                      else
                          offsetX = curOffset
                          offsetY = nextOffset
                      )
                  )
                  ;layer info is in the startVias to draw multiple vias
                  stVias = (segNum == 0 && startVias == t)
                  (if (caddr curLayer) != (caddr lastLayer) && length(newpoints) >= 1 then
                      ;switch layers if via list is different
                      tmpx = x + (if flipX -1 1)*(if (and hv!="HE" hv!="EH") offsetX 0.0)
                      tmpy = y + (if flipY -1 1)*(if (and hv!="VE" hv!="EV") offsetY 0.0)
                      newpoints = (append newpoints (list (list ((lastx+tmpx)/2.0) ((lasty+tmpy)/2.0))))
                      (DrawWire lastLayer (AddSuffix prefix node)
                                          width newpoints
                                          ?isPin isPin ?startVias stVias ?endVias nil ?verticalVias verticalVias ?positiveVias positiveVias 
                                          ?doubleVias doubleVias ?guideInst guideInst ?flipX flipX ?flipY flipY ?pattern pattern ?CV CV)
                      newpoints = (list (list ((lastx+tmpx)/2.0) ((lasty+tmpy)/2.0)))
                  )
                  lastLayer = curLayer
                  lastx= x+(if flipX -1 1)*(if (and hv!="HE" hv!="EH") offsetX 0)
                  lasty = y+(if flipY -1 1)*(if (and hv!="VE" hv!="EV") offsetY 0)
                  (if segNum == 0 && startVias && startVias!=t then
                     ;layer info is in the startVias to draw multiple vias
                      DrawVia((AddSuffix prefix node) startVias lastx:lasty 
                               width (if (and hv!="HE" hv!="EH") "_R_V" "_R_H") ?doubleVias doubleVias ?verticalVias verticalVias ?positiveVias positiveVias ?CV CV)
                      stVias = nil
                  )
                  eVias = endVias
                  (if segNum == length(points)-1 && endVias && endVias!=t then
                     ;layer info is in the endVias to draw multiple vias
                      DrawVia((AddSuffix prefix node) endVias lastx:lasty 
                               width (if (and hv!="HE" hv!="EH") "_R_V" "_R_H") ?doubleVias doubleVias ?verticalVias verticalVias ?positiveVias positiveVias ?CV CV)
                      eVias = nil
                  )
                  segNum++
              else
                  ; no alternate pattern
                  offsetX = offset
                  offsetY = offset
                  stVias = startVias
                  eVias = endVias
                  curLayer = layers
              )

              x = x+(if flipX -1 1)*(if (and hv!="HE" hv!="EH") offsetX 0)
              y = y+(if flipY -1 1)*(if (and hv!="VE" hv!="EV") offsetY 0)
              newpoints = (append newpoints (list (list x y)))
              index = index+1
          )
          ; draw wire for this node
          (DrawWire (if layer_override layer_override curLayer)
                    (AddSuffix prefix node) width newpoints
                    ?isPin isPin ?startVias stVias ?endVias eVias ?verticalVias verticalVias ?positiveVias positiveVias 
                    ?doubleVias doubleVias ?guideInst guideInst ?flipX flipX ?flipY flipY ?pattern pattern ?CV CV)

      when( drawShield DrawBusShield(newpoints (if layer_override layer_override curLayer) prefix ?shieldM2 shieldM2 ?CV CV) )
          
    )

  )
  t
  )

; Draw an array of channels.  Must give array_pitch for each segment
; (including "before and "after" segments), or give a single atom for
; all segments except before and after segments, which will be 0.
; 
;(DrawChannelArray metal67 e1of4 "a" 0:3 (list 0 19.2 4.8 4.8 4.8 0)
;                  (list 0.5:2 9.6:0 9.6:9.6 38.4:9.6 38.4:19.2))
(defun DrawChannelArray (layers pattern prefix indices array_pitch points
                           @key (flipX nil) (flipY nil) (isPin nil)
                           (startVias nil) (endVias nil) (doubleVias t) (verticalVias nil) (positiveVias nil) 
                           (guideInstName nil) (guideInst nil)
                           (only_evens nil) (only_odds nil) (drawShield t)
                           (CV (geGetEditCellView)))
  (let (x y o hv xoff yoff newpoints index index_list
        (types (ClassifyPoints points))
        (NP (length points)))

    ; set guide instance
    guideInst = (GetGuideInst guideInst guideInstName ?CV CV)

    ; check usage
    (cond ((and (not (atom array_pitch)) (length array_pitch)!=NP+1)
           (printf "ERROR: in array_pitch for %s.\nMust be either a single number or a list of N+1 pitches for an N point path.\n" prefix)
           nil
           ))

    ; create list of indices
    index_list=(list 0)
    (for i (car indices) (cadr indices)
      (when (mod i 2)==0 && only_evens
        index_list=(append1 index_list i)
      )
      (when (mod i 2)!=0 && only_odds
        index_list=(append1 index_list i)
      )
      (when !only_evens && !only_odds
        index_list=(append1 index_list i)
      )
    )
    ; go through all channels in array
    (foreach i (cdr index_list)
         newpoints = nil
         index = 0
         (foreach point points
                  x = (car point)
                  y = (cadr point)
                  o = i-(car indices)
                  hv = (nth index types)
                  xoff = (cond (hv=="VE" (GetOffset NP index   array_pitch))
                               (hv=="EV" (GetOffset NP index+1 array_pitch))
                               (hv=="VH" (GetOffset NP index   array_pitch))
                               (hv=="HV" (GetOffset NP index+1 array_pitch))
                               (hv=="EH" (GetOffset NP index   array_pitch))
                               (hv=="HE" (GetOffset NP index+1 array_pitch)))
                  x = x+o*xoff*(if flipX -1 1)
                  yoff = (cond (hv=="VE" (GetOffset NP index+1 array_pitch))
                               (hv=="EV" (GetOffset NP index   array_pitch))
                               (hv=="VH" (GetOffset NP index+1 array_pitch))
                               (hv=="HV" (GetOffset NP index   array_pitch))
                               (hv=="EH" (GetOffset NP index+1 array_pitch))
                               (hv=="HE" (GetOffset NP index   array_pitch)))
                  y = y+o*yoff*(if flipY -1 1)
                  newpoints = (append newpoints (list (list x y)))
                  index = index+1
                  )
         
         ; draw channel
         (DrawChannel layers pattern (AddSuffix prefix (sprintf nil "[%d]" i)) newpoints
                      ?flipX flipX ?flipY flipY ?verticalVias verticalVias ?positiveVias positiveVias 
                      ?isPin isPin ?startVias startVias ?endVias endVias ?doubleVias doubleVias
                      ?guideInst guideInst ?drawShield drawShield ?CV CV)
         )
    )
  t
  )

; get the nth array_pitch, or return 0/array_pitch if its an atom
(defun GetOffset (num_points index array_pitch)
  (if (atom array_pitch) then
    (if (or index==0 index==num_points) then 0
        else array_pitch)
    else (nth index array_pitch)
    )
  )

; select by BusPattern
(defun SelectPattern (name @key (CV (geGetEditCellView)))
  (let (prop found)
    found = 0
    (foreach obj CV->shapes
             prop = (GetProp obj "BusPattern" nil)
             (cond ((and obj->objType=="path" prop==name)
                    (geSelectObject obj)
                    found = found+1
                    )
                   )
             )
    found
    )
  )

; select by BusPrefix
(defun SelectPrefix (name @key (CV (geGetEditCellView)))
  (let (prop found)
    found = 0
    (foreach obj CV->shapes
             prop = (GetProp obj "BusPrefix" nil)
             (cond ((and obj->objType=="path" prop==name)
                    (geSelectObject obj)
                    found = found+1
                    )
                   )
             )
    found
    )
  )

; get list of waypoints and layers from (optionally) named shapes
(defun GetPaths (name @key (guideInstName nil) (CV (geGetEditCellView)))
  (let (paths shapes pattern pre isPin startVias endVias flipX flipY pitch double shield rotate vertical positive props)
    paths = nil
    shapes = (geGetSelSet)
    (cond (guideInstName!=nil CV = (GetGuideInst nil guideInstName ?CV CV)->master))
    (cond (shapes==nil shapes = CV->shapes))
    (foreach obj shapes
             pattern   = (GetProp obj "BusPattern" nil)
             pre       = (GetProp obj "BusPrefix" "")
             isPin     = (GetProp obj "BusPin" nil)
             startVias = (GetProp obj "BusStartVias" nil)
             endVias   = (GetProp obj "BusEndVias" nil)
             flipX     = (GetProp obj "BusFlipX" nil)
             flipY     = (GetProp obj "BusFlipY" nil)
             pitch     = (GetProp obj "BusArrayPitch" TrackPitch)
             double    = (GetProp obj "BusDoubleVias" nil)
             shield    = (GetProp obj "DrawBusShields" nil)
             rotate    = (GetProp obj "RotateMetalOrientation" nil)
             vertical  = (GetProp obj "BusVerticalVias" nil)
             positive  = (GetProp obj "PositiveViaShift" nil)
             props = (list pattern obj->lpp pre isPin startVias endVias flipX flipY pitch double shield rotate vertical positive)
             (cond ((and obj->objType=="path" pattern!=nil (or name==nil pattern==name))
                    paths = (cons (list props obj->points) paths))
                   )
             )
    paths
    )
  )

; create bus properties if necessary
(defun CreateBusProps (@key (paths nil))
  (let (pattern prefix pin flipX flipY startVias endVias doubleVias drawShield rotateMetals verticalVias positiveVias )
    paths = (if paths==nil (geGetSelSet) paths)
    (foreach obj paths
             (cond (obj->objType=="path"
                                  pattern = (GetProp obj "BusPattern" "")
                                  prefix = (GetProp obj "BusPrefix" "")
                                  pin = (GetProp obj "BusPin" nil)
                                  flipX = (GetProp obj "BusFlipX" nil)
                                  flipY = (GetProp obj "BusFlipY" nil)
                                  startVias = (GetProp obj "BusStartVias" nil)
                                  endVias = (GetProp obj "BusEndVias" nil)
                                  pitch = (GetProp obj "BusArrayPitch" TrackPitch)
                                  doubleVias = (GetProp obj "BusDoubleVias" nil)
                                  drawShield = (GetProp obj "DrawBusShields" nil)
                                  rotateMetals = (GetProp obj "RotateMetalOrientation" nil)
                                  verticalVias = (GetProp obj "BusVerticalVias" nil)
                                  positiveVias = (GetProp obj "PositiveViaShift" nil)
                                  ; delete properties
                                  (dbDeletePropByName obj "BusPattern")
                                  (dbDeletePropByName obj "BusPrefix")
                                  (dbDeletePropByName obj "BusPin")
                                  (dbDeletePropByName obj "BusFlipX")
                                  (dbDeletePropByName obj "BusFlipY")
                                  (dbDeletePropByName obj "BusStartVias")
                                  (dbDeletePropByName obj "BusEndVias")
                                  (dbDeletePropByName obj "BusArrayPitch")
                                  (dbDeletePropByName obj "BusDoubleVias")
                                  (dbDeletePropByName obj "DrawBusShields")
                                  (dbDeletePropByName obj "RotateMetalOrientation")
                                  (dbDeletePropByName obj "BusVerticalVias")
                                  (dbDeletePropByName obj "PositiveViaShift")
                                  ; set new properties
                                  (dbReplaceProp obj "BusPattern" "string" pattern)
                                  (dbReplaceProp obj "BusPrefix" "string" prefix)
                                  (dbReplaceProp obj "BusPin" "boolean" pin)
                                  (dbReplaceProp obj "BusFlipX" "boolean" flipX)
                                  (dbReplaceProp obj "BusFlipY" "boolean" flipY)
                                  (dbReplaceProp obj "BusStartVias" "boolean" startVias)
                                  (dbReplaceProp obj "BusEndVias" "boolean" endVias)
                                  (dbReplaceProp obj "BusArrayPitch" "list" pitch)
                                  (dbReplaceProp obj "BusDoubleVias" "boolean" doubleVias)
                                  (dbReplaceProp obj "DrawBusShields" "boolean" drawShield)
                                  (dbReplaceProp obj "RotateMetalOrientation" "boolean" rotateMetals)
                                  (dbReplaceProp obj "BusVerticalVias" "boolean" verticalVias)
                                  (dbReplaceProp obj "PositiveViaShift" "boolean" positiveVias)
                                  ))
             )
    )
  t
  )

; set the BusPattern property, create other properties if necessary
(defun SetPattern (pattern @key (paths nil))
  (let (pat)
    paths = (if paths==nil (geGetSelSet) paths)
    (foreach obj paths
      (cond (obj->objType=="path"
                  pat = (if pattern pattern (GetProp obj "BusPattern" "e1of4"))
                  (CreateBusProps ?paths (list obj))
                  (dbReplaceProp obj "BusPattern" "string" pat)
                  )
            ((or obj->objType=="rect" obj->objType=="polygon")
                  (CreateObsProps ?shapes (list obj))
                  )
            )
      )
    t
    )
  )

; eliminate degenerate segments from points
(defun CanonicalizePoints (points)
  (let (newpoints last next point (len (length points)))
    newpoints = (append newpoints (list (nth 0 points))) ; first point
    (for i 1 len-2
         last  = (nth i-1 points)
         point = (nth i   points)
         next  = (nth i+1 points)
         (cond ((and (not (almostEqual (car  last) (car  next)))
                     (not (almostEqual (cadr last) (cadr next))))
                newpoints = (append newpoints (list point))))
         )
    newpoints = (append newpoints (list (nth len-1 points))) ; last point
    )
  )

; move way points
(defun OffsetPoints (points dx dy)
  (let (newpoints p)
    (for i 0 (length points)-1
         p = (nth i points)
         p = (list (car p)+dx (cadr p)+dy)
         newpoints = (append newpoints (list p))
         )
    newpoints
    )
  )

; offset some of the points
(defun OffsetSomePoints (points from to dx dy)
  (let (newpoints apply p)
    (for i 0 (length points)-1
         apply = (and i>=from i<=to)
         p = (nth i points)
         p = (list (car p)+dx*(if apply!=nil 1 0) (cadr p)+dy*(if apply!=nil 1 0))
         newpoints = (append newpoints (list p))
         )
    newpoints
    )
  )

; move paths
(defun OffsetPaths (paths dx dy)
  (let (newpaths props points)
    (foreach path paths
             props = (car path)
             points = (cadr path)
             newpaths = (cons (list props (OffsetPoints points dx dy)) newpaths))
    newpaths
    )
  )

; move only some points of paths
(defun OffsetSomePointsOfPaths (paths from to dx dy)
  (let (newpaths props points)
    (foreach path paths
             props = (car path)
             points = (cadr path)
             newpaths = (cons (list props (OffsetSomePoints points from to dx dy)) newpaths))
    newpaths
    )
  )

; pick which bus prefix to use
(defun ChoosePrefix (prefix path_prefix)
  (cond
   ((and path_prefix!=nil path_prefix!="") path_prefix)
   ((and prefix!=nil prefix!="") prefix)
   ("")
   )
  )

; draw multiple channels
(defun DrawChannels (layers pattern prefix paths
                            @key (guideInstName nil) (guideInst nil) (CV (geGetEditCellView)))
  (let (props points layer isPin startVias endVias doubleVias flipX flipY busShield rotateMetals verticalVias positiveVias 
              patlist prelist pre buspat)

    ; set guide instance
    guideInst = (GetGuideInst guideInst guideInstName ?CV CV)

    ; process guide paths
    (cond ((or paths==nil (atom paths))
           paths = (GetPaths paths ?CV (if guideInst->master guideInst->master CV))))
    (foreach path paths
             props   = (car path)
             points  = (cadr path)
             patlist = (parseString (nth 0 props) " ")
             layer   = (nth 1 props)
             prelist = (parseString (nth 2 props) " ")
             isPin   = (nth 3 props)
             startVias = (nth 4 props)
             endVias = (nth 5 props)
             flipX   = (nth 6 props)
             flipY   = (nth 7 props)
             doubleVias = (nth 9 props)
             busShield = (nth 10 props)
             rotateMetals = (nth 11 props)
             verticalVias = (nth 12 props)
	     positiveVias = (nth 13 props)

             ; iterate over BusPattern/BusPrefix lists
             (foreach pat patlist
                      pre = (ChoosePrefix prefix (car prelist))
                      prelist = (cdr prelist)
                      buspat = (if pattern==nil (MapBusPattern pat) pattern)
                      (when (and buspat pre)
                        (DrawChannel (ChooseLayers layers layer ?rotateMetals rotateMetals)
                                     buspat pre points
                                     ?flipX flipX ?flipY flipY ?isPin isPin
                                     ?startVias startVias ?endVias endVias
                                     ?doubleVias doubleVias ?verticalVias verticalVias ?positiveVias positiveVias  
                                     ?guideInst guideInst ?drawShield busShield ?CV CV)
                        )
                      )
             )
    )
  t
  )

; draw multiple channel arrays
(defun DrawChannelArrays (layers pattern prefix indices array_pitch paths
                                 @key (guideInstName nil) (guideInst nil)
                                      (only_evens nil) (only_odds nil)
                                      (CV (geGetEditCellView)))
  (let (props points layer isPin startVias endVias doubleVias flipX flipY
              pitch busShield rotateMetals verticalVias positiveVias  
              patlist prelist pre buspat lo hi lohi options evens odds)

    ; set guide instance
    guideInst = (GetGuideInst guideInst guideInstName ?CV CV)

    ; look for array bus patterns
    (rexCompile "^\\(.*\\)\\[\\([0-9]+\\)[\\.]*\\([0-9]*\\)\\]$")

    ; process all guide paths
    (cond ((or paths==nil (atom paths))
           paths = (GetPaths paths ?CV (if guideInst->master guideInst->master CV))))
    (foreach path paths
             props  = (car path)
             points = (cadr path)
             patlist = (parseString (nth 0 props) " ")
             layer  = (nth 1 props)
             prelist = (parseString (nth 2 props) " ")
             isPin  = (nth 3 props)
             startVias = (nth 4 props)
             endVias = (nth 5 props)
             flipX  = (nth 6 props)
             flipY  = (nth 7 props)
             pitch  = (if array_pitch==nil (nth 8 props) array_pitch)
             doubleVias = (nth 9 props)
             busShield = (nth 10 props)
             rotateMetals = (nth 11 props)
             verticalVias = (nth 12 props)
	     positiveVias = (nth 13 props)

             ; iterate over BusPattern/BusPrefix lists
             (for n 0 (length patlist)-1
                  pat = (nth n patlist)
                  pre = (if prelist (nth (mod n (length prelist)) prelist) nil)
                  pre = (ChoosePrefix prefix pre)

                  ; default options
                  evens = only_evens
                  odds  = only_odds
                  lohi  = indices

                  ; process pattern options (pattern:opt1:opt2:...)
                  (unless pattern
                    options = (parseString pat ":")
                    pat     = (car options)
                    options = (cdr options)
                    (while options
                      (cond ((car options)=="even" evens=t)
                            ((car options)=="odd"  odds=t)
                            )
                      options = (cdr options)
                      )
                    )

                  ; pick pattern and array range automatically
                  (when (and pattern==nil lohi==nil (rexExecute pat))
                    pat = (rexSubstitute "\\1")
                    lo = (rexSubstitute "\\2")
                    hi = (rexSubstitute "\\3")
                    (cond (hi=="" hi=(atoi lo)-1 lo=0)
                          (t      hi=(atoi hi)   lo=(atoi lo))
                          )
                    lohi = (list lo hi)
                    )
                  buspat = (if pattern==nil (MapBusPattern pat) pattern)

                  ; draw
                  (when (and buspat pre lohi)
                    (DrawChannelArray (ChooseLayers layers layer ?rotateMetals rotateMetals)
                                      buspat pre lohi pitch points
                                      ?flipX flipX ?flipY flipY ?isPin isPin
                                      ?startVias startVias ?endVias endVias
                                      ?doubleVias doubleVias ?verticalVias verticalVias ?positiveVias positiveVias 
                                      ?guideInst guideInst
                                      ?only_evens evens ?only_odds odds ?drawShield busShield ?CV CV)
                    )
                  )
             )
    )
  t
  )

; highlight overlaps of paths on the same metal layer
(defun FindBusOverlaps (@key (CV (geGetEditCellView)))
  (let (any_overlap bbox xy0 xy1 x0 y0 x1 y1 overlaps writable)
    any_overlap=nil
    if( CV then
        writeable=nil;
        if(CV~>mode == "r" then
            writeable=nil
        else
            writable=t
        )
        any_overlap = nil
        if(writable (geDeleteAllMarker CV))
        (foreach shape CV->shapes
                 (cond (((isWiringPath shape) && ( writable || ! any_overlap ) )
                        bbox = shape->bBox
                        xy0 = (car bbox)
                        xy1 = (cadr bbox)
                        x0 = (car xy0)
                        y0 = (cadr xy0)
                        x1 = (car xy1)
                        y1 = (cadr xy1)
                        bbox = (list x0+0.005:y0+0.005 x1-0.005:y1-0.005)
                        overlaps = (dbGetOverlaps CV bbox shape->lpp 0)
                        (foreach obj overlaps
                                 (cond ((and (isWiringPath obj) obj!=shape)
                                        any_overlap = t
                                        if(writable (geCreateMarker obj "warning" "bus" "overlap" ""))))
                                 )
                        )
                    )
                )
           )
    any_overlap
    )
  )

; Create a channel of sub-channels.  Examples:
; chan = (DefChan (list (list prefix channel offset) ...))
; datatail = (DefChan (let (chan channels)
;                       (for i 0 15
;                            chan = (list (sprintf nil ".D[%d]" i) smr_e1of4 TrackPitch*i)
;                            channels = (cons chan channels))
;                       chan = (list ".T" smr_e1of2 TrackPitch*16)
;                       channels = (cons chan channels)
;                       channels))
(defun DefChan (channels)
  (let (newchan newrail prefix pattern offset newname)
    (foreach chan channels
             prefix  = (car chan)
             pattern = (cadr chan)
             offset  = (caddr chan)
             layer_override = (cadddr chan)
             (foreach rail pattern
                      newname = (if (car rail)=="GND" "GND"
                                    (sprintf nil "%s%s" prefix (car rail)))
                      newrail = (list newname (cadr rail)
                                      (caddr rail)+offset layer_override)
                      newchan = (append newchan (list newrail))
                      )
             )
    newchan
  )
)

; Round the offsets in a channel definition (suitable for DefChan) to multiple
; of pitch, and return a new channel definition (suitable for DefChan)
(defun AlignDefChan (pitch channels)
  (mapcar
    (lambda (chan)
      (list (car chan)
            (cadr chan)
            (truncate (caddr chan)/pitch)*pitch ; or MathRoundToNearest?
            (cadddr chan)))
    channels))

; Expand array elements into a defchan
(defun DefChanArray (pattern indices array_pitch)
  (let (chan channels)
    channels = nil
    (for i (car indices) (cadr indices)
         chan = (list (sprintf nil "[%d]" i) pattern i*array_pitch)
         channels = (cons chan channels)
         )
    (DefChan channels)
    )
  )

(defun BuildDefChanList ( oldlist name array type offset increment @key (use_dots t) )
  (let (namelist newlist i dot)
  (if use_dots then dot = "." else dot = "")
  (if array <= 1 then
    namelist = (list (sprintf nil "%s%s" dot name) )
   else
    namelist = (list (sprintf nil "%s%s[0]" dot name) )
    (for i 1 array-1
      namelist = (append1 namelist (sprintf nil "%s%s[%d]" dot name i) )
    )
  )
  i=0
  (foreach newname namelist
    (if !newlist then
      (if !oldlist then
        newlist = (list (list newname type offset) )
       else
        newlist = (append1 oldlist (list newname type offset) )
       )
     else
    i=i+1
    newlist = (append1 newlist (list newname type offset+i*increment) )
    )
  )
  newlist
  ))

; load wires.il for currently edited cellview
(defun LoadWires (@key (CV (geGetEditCellView)))
  (BusDefaults)
  (let (fl port Outfl outfl h txt var1 var2 text CN Cellname lngth Initial k i j finalpath initialpath FinalPath modtime variable1 variable2 Variable2 Match Initialize_wires l FileFound)
	i=0
	l=0
	Match=0
	Cellname=(NameGetCellPathFromCellName CV->cellName)
	piece=parseString( Cellname "/" )
	lngth=length(piece)
	println(Cellname)
	Initial=nth(1 piece)
	finalpath=""
	for(j 6 lngth-1
	FileFound=0
	Initialize_wires=0
	for(i 0 j
	initialpath=nth(i piece)
	finalpath=strcat(finalpath "/" initialpath)
	i=i+1)
	FinalPath=strcat(finalpath "/" "wires.il")
	modtime = (fileTimeModified FinalPath)
	when(modtime!=nil
		when((Startloadwires==0)
		load(FinalPath)
		printf("Loading %s \n" FinalPath)
		wires_modtime[Startloadwires]=list(FinalPath modtime)
		Startloadwires=Startloadwires+1
		)
		when((Startloadwires!=0)
		for(l 0 length(wires_modtime)-1
			variable1=car(wires_modtime[l])
			variable2=cadr(wires_modtime[l])
				when((variable1!=FinalPath)
					FileFound=1
					)
				when((variable1==FinalPath)&&(modtime>variable2)
					load(FinalPath)
					printf("Loading %s \n" FinalPath)
					wires_modtime[l]=list(FinalPath modtime)
					Initialize_wires=1
					)
				when((variable1==FinalPath)&&(modtime<=variable2)
					Initialize_wires=1
					)
			l=l+1
			)

		when( FileFound == 1 && Initialize_wires == 0
			load(FinalPath)
			printf("Loading %s \n" FinalPath)			
			wires_modtime[Startloadwires]=list(FinalPath modtime)
			Startloadwires=Startloadwires+1			
			)
		))
	l=0
	j=j+1
	finalpath=""
	i=0
	)

        ;always load the primary cell's file
        FinalPath=(sprintf nil "%s/wires.il"
                 (NameGetCellPathFromCellName CV->cellName))
        (when (isFile FinalPath) (load FinalPath) )
        )
  )

; load a file given a relative path from the cell directory (i.e. "../wires.il")
(defun LoadRelative (path @key (CV (geGetEditCellView)))
  (let (dir file)
    dir = (NameGetCellPathFromCellName CV->cellName)
    file = (sprintf nil "%s/%s" dir path)
    (load file)
    )
  )

; load then draw wires.il for currently edited cellview
(defun DrawWires (@key (CV (geGetEditCellView)))
  (defun DrawAll (@key (CV (geGetEditCellView))) 
    (printf "Default DrawAll: DrawTracks with paths in current cellview.\n")
    busPatterns = (append busPatterns defchan_patterns)
    (DrawTracks ?CV CV)
    (DrawObstructions ?CV CV)
    t
    )
  (cond ((IsGuideInst CV)
         (printf "Can't draw wires inside the guide instance\n"))
        (t
         (LoadWires ?CV CV)
         (DrawAll ?CV CV)
         (when (dbFindAnyInstByName CV "autobuswires")
           (DrawChannels nil nil nil nil ?guideInstName "autobuswires" ?CV CV)
           )
         )
        )
  t
  )

; fix up pin names by using labels attached to them
(defun FixPinNames (@key (CV (geGetEditCellView)))
  (let ()
    (foreach shape CV->shapes
             (cond ((and shape->objType=="label" shape->parent!=nil)
                    (dbCreatePin (MakeNet CV shape->theLabel) shape->parent)
                    )
                   )
             )
    )
  t
  )

; flatten any subcecells with .wires. in its cell name, then delete guide layers
(defun FlattenWiringSubcells (@key (CV (geGetEditCellView)))
  (let (progress)
    leSetAllLayerVisible(t)

    ; flatten wiring subcells
    (rexCompile "\\.wires\\.")
    progress = t
    (while progress
      progress = nil
      (foreach inst CV->instances
               (cond ((and inst->objType=="inst" (rexExecute inst->cellName))
                      (dbFlattenInst inst 1 nil t t)
                      progress = t
                      )
                     )
               )
      )

    ; delete guide paths
    (foreach shape CV->shapes
             (cond ((and shape->objType=="path"
                         shape->purpose=="bus")
                    (dbDeleteObject shape)
                    )
                   )
             )

    ; slot paths
    (SlotPaths)
    
    ; fix pins
    (FixPinNames)
    )
  t
  )

; chop all shapes with given chop bbox on specified layer
(defun ChopPaths (CV bbox layerName @key (onlyGND nil))
  (let (shapes x0 y0 x1 y1 chop)
    shapes = (dbGetOverlaps CV bbox (list layerName "drawing") 0)
    (foreach shape shapes
             (cond ((and (isMetalDrawing shape->layerName shape->purpose)
                         shape->layerName==layerName
                         shape->objType=="path"
                         (or !onlyGND shape->net->name=="GND")
                         (IsBusObject shape))
                    x0 = (car  (car  bbox))
                    y0 = (cadr (car  bbox))
                    x1 = (car  (cadr bbox))
                    y1 = (cadr (cadr bbox))
                    chop = (list x0:y0 x1:y0 x1:y1 x0:y1)
                    (leChopShape shape chop t t)
                    )
                   )
             )
    )
  t
  )

; chop all paths using "slot" rectangles in specified subcell
(defun SlotPathsSubcell (CV inst)
  (let (transform bbox)
    transform = (geGetInstTransform inst)
    (foreach shape inst->master->shapes
             (cond ((and shape->objType=="rect"
                         (isMetalSlot shape->layerName shape->purpose))
                    bbox = (geTransformUserBBox shape->bBox transform)
                    (ChopPaths CV bbox shape->layerName)
                    )
                   )
             )
    )
  t
  )

; chop all paths using rectangles with "slot" purpose in all subcells
(defun SlotPaths (@key (CV (geGetEditCellView)))
  (let (bbox)

    ; process subcell slot layers
    (foreach inst CV->instances
             (cond (inst->libName!=TechLibName (SlotPathsSubcell CV inst)))
             )

    ; process then delete top level slot layers
    (foreach shape CV->shapes
             (cond ((and shape->objType=="rect"
                         (isMetalSlot shape->layerName shape->purpose))
                    bbox = shape->bBox
                    (ChopPaths CV bbox shape->layerName)
                    (dbDeleteObject shape)
                    )
                   )
             )
    )
  t
  )

; Alternate version of SlotPaths meant to be added to DrawAll.  Looks
; for "buscut" purpose in buswires instance, and only chops GND
; shielding wires.  Can include in DrawAll of wires.il
(defun SlotGND (@key (guideInst nil) (guideInstName nil) (CV (geGetEditCellView)))
  guideInst = (GetGuideInst guideInst guideInstName)
  (foreach shape guideInst->master->shapes
           (when (isMetalSlot shape->layerName shape->purpose)
             (ChopPaths CV shape->bBox shape->layerName ?onlyGND t)
             )
           )
  t
  )

; mark a list of wiring shapes with marker paint
(defun MarkWires (shapes)
  (foreach shape shapes
           (cond ((isWiringPath shape) 
                  (geCreateMarker shape "warning" "bus" "length" "")))
           )
  t
  )

; create BusMark property and set to t
(defun SetBusMark (CV)
  (foreach shape CV->shapes
           (cond ((isWiringPath shape)
                  (dbReplaceProp shape "BusMark" "boolean" t))))
  (rexCompile ".*VIA[1-8][1-8].*")
  (foreach inst CV->instances
           (cond ((or (rexExecute inst->cellName) (isWiringContact inst))
                  (dbReplaceProp inst "BusMark" "boolean" t))))
  t
  )

; create BusMark property and set to t for a list of objects
(defun SetBusMarkForObjects (objects)
  (foreach obj objects (dbReplaceProp obj "BusMark" "boolean" t))
  t
  )

; delete BusMark property
(defun ClearBusMark (CV)
  (foreach shape CV->shapes
           (cond ((isWiringPath shape)
                  (dbDeletePropByName shape "BusMark"))))
  (foreach inst CV->instances
           (cond ((isWiringContact inst)
                  (dbDeletePropByName inst "BusMark"))))
  t
  )

; find subcell pins connected to set of wires
(defun ConnectedSubcellPins (CV objects)
  (let (connected overlaps inst pin layerName bbox)
    (foreach obj objects
             (cond (obj->objType=="path"
                    overlaps = (dbGetOverlaps CV obj->bBox (list obj->layerName "net") 1:1)
                    overlaps = (append overlaps (dbGetOverlaps CV obj->bBox (list obj->layerName "drawing") 1:1))
                    overlaps = (append overlaps (dbGetOverlaps CV obj->bBox (list obj->layerName "pin") 1:1))
                    (foreach overlap overlaps
                             (cond ((atom overlap)==nil
                                    inst = (car overlap)
                                    pin = (cadr overlap)
                                    (cond ((and pin->pin !(isWiringContact inst))
                                           connected = (append connected (list (list inst pin)))
                                           )
                                          )
                                    )
                                   )
                             )
                    )
                   ((isWiringContact obj)
                    (foreach lpp obj->master->lpps
                             layerName = lpp->layerName
                             bbox = (list obj->xy obj->xy)
                             overlaps = (dbGetOverlaps CV bbox (list layerName "net") 1:1)
                             overlaps = (append overlaps (dbGetOverlaps CV bbox (list layerName "drawing") 1:1))
                             overlaps = (append overlaps (dbGetOverlaps CV bbox (list layerName "pin") 1:1))
                             (foreach overlap overlaps
                                      (cond ((atom overlap)==nil
                                             inst = (car overlap)
                                             pin = (cadr overlap)
                                             (cond ((and pin->pin !(isWiringContact inst))
                                                    connected = (append connected (list (list inst pin)))
                                                    )
                                                   )
                                             )
                                            )
                                      )
                             )
                    )
                   )
             )
    connected
    )
  )

; returns all wiring connected to a subcell pin
(defun ConnectedWiringFromSubcellPin (CV inst pin)
  (let (connected overlaps transform bbox)
    transform = (geGetInstTransform inst)
    bbox = (geTransformUserBBox pin->bBox transform)
    overlaps = (dbGetOverlaps CV bbox)
    (foreach overlap overlaps
             (cond ((areOverlappingObjectsConnected pin overlap)
                    connected = (append connected (ConnectedWiring CV overlap)))
                   )
             )
    connected
    )
  )

; exchange L fpr R pins
(defun SwapLeftRightPins (inst pin)
  (let (name term)
    name = pin->net->name
    (cond ((substring name 1 1)=="L"
           name = (strcat "R" (substring name 2))
           term = (dbFindTermByName inst->master name)
           pin = (car term->pins)
           pin->fig)
          (t nil)
          )
    )
  )

; name instances of buffers encountered start at initial inst and pin
(defun NameBuffersRecursive (CV inst pin prefix index)
  (let (wires inst_pins newinst newpin nextpin newname oldinst)
    (cond ((inst==nil) wires = (ConnectedWiring               CV      pin))
          ((inst!=nil) wires = (ConnectedWiringFromSubcellPin CV inst pin))
          )
    (MarkWires wires)
    inst_pins = (ConnectedSubcellPins CV wires)
    (foreach inst_pin inst_pins
             newinst = (car inst_pin)
             newpin = (cadr inst_pin)
             nextpin = (SwapLeftRightPins newinst newpin)
             (cond ((and newinst->viewName=="wiring" newinst!=inst nextpin!=nil)
                    ; give this instance its proper name
                    (printf "%s -> %s[%d]\n" newinst->name prefix index)
                    newname = (sprintf nil "%s[%d]" prefix index)
                    oldinst = (dbFindAnyInstByName CV newname)
                    (cond (oldinst!=nil
                           oldinst->name = (sprintf nil "old_%s" oldinst->name)
                           )
                          )
                    newinst->name = newname
                    ; search for more buffers
                    (NameBuffersRecursive CV newinst nextpin prefix index+1))
                   )
             )
    )
  t
  )

; name instances of buffers encountered starting after initial instName and pinName
; must call SetBusMark before processing a non-conflicting set of these
(defun NameBuffers (instName pinName prefix @key (CV (geGetEditCellView)))
  (let (inst term pin)
    (cond ((instName!=nil) ; start with a subcell pin
           inst = (dbFindAnyInstByName CV instName)
           (cond ((or inst!=nil instName==nil)
                  term = (dbFindTermByName inst->master pinName)
                  pin = (car term->pins)->fig
                  (cond (pin!=nil
                         (NameBuffersRecursive CV inst pin prefix 0))
                        (t
                         (printf "ERROR: can't find pin %s in instance %s\n" pinName instName))
                        )
                  )
                 (t
                  (printf "ERROR: can't find instance %s\n" instName)
                  )
                 )
           )
          ((instName==nil) ; start with top level pin
           term = (dbFindTermByName CV pinName)
           pin = (car term->pins)->fig
           (cond (pin!=nil
                  (NameBuffersRecursive CV nil pin prefix 0))
                 (t
                  (printf "ERROR: can't find pin %s\n" pinName))
                 )
           )
          )
    )
  t
  )

; infer SPEC subtypes from dfII, also list unique wiring-view cell types
(defun ScanSubtypes (@key (CV (geGetEditCellView)))
  (let (file temp type subtypes types last)

    ; find wiring subcells
    (foreach inst CV->instances
             (cond (inst->viewName=="wiring"
                    temp = (parseString inst->cellName ".")
                    type = nil
                    (for i 0 (length temp)-2
                         type = (append type (list (nth i temp)))
                         )
                    type = (buildString type ".")
                    types = (cons inst->cellName types)
                    temp = (sprintf nil "    %s :>\n      %s %s;"
                                    type inst->cellName inst->name)
                    subtypes = (cons temp subtypes)
                    )
                   )
             )

    ; sort and write subtypes
    subtypes = (sort subtypes nil)
    file = (outfile (sprintf nil "%s.subtypes" CV->cellName "w"))
    (foreach subtype subtypes (fprintf file "%s\n" subtype))
    (close file)
    
    ; sort and write types
    types = (sort types nil)
    file = (outfile (sprintf nil "%s.types" CV->cellName "w"))
    last = nil
    (foreach type types 
             (cond (type!=last (fprintf file "%s\n" type)))
             last = type
             )
    (close file)
    )
  t
  )

; quickly scan pins to a userpins.il file
(defun ScanPins (@key (CV (geGetEditCellView)))
  (let (file bbox x0 y0 x1 y1 name str pins)
    (foreach obj CV->shapes 
             (cond (obj->pin
                    bbox = obj->bBox
                    x0 = (car  (car  bbox))
                    y0 = (cadr (car  bbox))
                    x1 = (car  (cadr bbox))
                    y1 = (cadr (cadr bbox))
                    name = obj->net->name
                    str = (sprintf nil "(PinPlace \"%s\" (list %g:%g %g:%g) ?LPP (list \"%s\" \"%s\"))"
                                   name x0 y0 x1 y1 obj->layerName obj->purpose)
                    pins = (cons str pins)
                    )
                   )
             )

    ; sort and write userpins.il
    file = (outfile (sprintf nil "%s/userpins.il"
                             (NameGetCellPathFromCellName CV->cellName) "w"))
    pins = (sort pins nil)
    (foreach str pins (fprintf file "%s\n" str))
    (close file)
    )
  t
  )

; report what _RESET pins in wiring subcells are connected to
(defun ScanReset (@key (CV (geGetEditCellView)))
  (let (file term pin wiring str resets name sub)
    (rexCompile "^standard\\.reset\\.BUF_RESET\\.")
    (SetBusMark CV)
    (foreach inst CV->instances
             (cond (inst->viewName=="wiring"
                    term = (dbFindTermByName inst->master "_RESET")
                    (cond (term
                           name = "_RESET"
                           
                           ; find wiring connected to this instances _RESET pin
                           pin = (car term->pins)->fig
                           wiring = (ConnectedWiringFromSubcellPin CV inst pin)
                           (SetBusMarkForObjects wiring)
                           
                           ; look for outputs of BUF_RESET's
                           inst_pins = (ConnectedSubcellPins CV wiring)
                           (foreach inst_pin inst_pins
                                    sub = (car  inst_pin)
                                    pin = (cadr inst_pin)
                                    (cond ((and (rexExecute sub->master->cellName)
                                                pin->net->name=="_RES")
                                           name = (sprintf nil "%s._RES" sub->name)
                                           )
                                          )
                                    )
                           
                           ; look for top level pins
                           (foreach obj wiring (cond (obj->pin name = obj->net->name)))
                           str = (sprintf nil "%s._RESET = %s" inst->name name)

                           ; append to list of resets found
                           resets = (cons str resets)
                           )
                          )
                    )
                   )
             )
    (ClearBusMark CV)

    ; sort and write userpins.il
    file = (outfile (sprintf nil "%s.resets" CV->cellName "w"))
    resets = (sort resets nil)
    (foreach str resets (fprintf file "%s\n" str))
    (close file)
    )
  t
  )

; check a single object for a power short
(defun CheckObjectPowerShort (obj grid distance)
  (let (x0 y0 x1 y1 gx0 gy0 gx1 gy1 bbox)
    bbox = obj->bBox
    x0 = (car  (car  bbox))
    y0 = (cadr (car  bbox))
    x1 = (car  (cadr bbox))
    y1 = (cadr (cadr bbox))
    gx0 = grid * (round x0/grid)
    gy0 = grid * (round y0/grid)
    gx1 = grid * (round x1/grid)
    gy1 = grid * (round y1/grid)
    (cond ((or (and (isWiringPath obj)
                    (or obj->layerName==(car Metal2LPP)
                        obj->layerName==(car Metal4LPP)
                        obj->layerName==(car Metal6LPP))
                    (or ((abs x0-gx0)<distance) 
                        ((abs x1-gx1)<distance))
                    )
               (and (isWiringPath obj)
                    (or obj->layerName==(car Metal3LPP)
                        obj->layerName==(car Metal5LPP)
                        obj->layerName==(car Metal7LPP))
                    (or ((abs y0-gy0)<distance) 
                        ((abs y1-gy1)<distance))
                    )
               (and (isWiringContact obj)
                    (or ((abs y0-gy0)<distance) 
                        ((abs y1-gy1)<distance)
                        ((abs x0-gx0)<distance) 
                        ((abs x1-gx1)<distance))
                    )
               )
           (geCreateMarker obj "warning" "bus" "power short" "")
           1
           )
          (t 0)
          )
    )
  )

; check for shorts to grid
(defun FindPowerShorts (@key (grid TrackPitch) (distance 0.6) (CV (geGetEditCellView)))
  (let (shorts)
    shorts = 0
    (foreach shape CV->shapes
             shorts = shorts+(CheckObjectPowerShort shape grid distance))
    (foreach inst CV->instances
             shorts = shorts+(CheckObjectPowerShort inst grid distance))
    shorts
    )
  )

; open a list of CellViews named in a file
(defun OpenCellViewsFromFile (filename viewName mode)
  (let (cells file cellName libName temp CellView)
    file = (infile filename)
    (cond (file==nil (printf "ERROR: can't read file %s" filename))
          (t
           (while (gets line file)
             cellName = (StringUtilChompNewline line)
             temp = (parseString cellName ".")
             libName = nil
             (for i 0 (length temp)-3
                  libName = (append libName (list (nth i temp)))
                  )
             libName = (buildString libName ".")
             CellView = (dbOpenCellViewByType libName cellName viewName nil mode)
             (cond (CellView==nil
                    (printf "ERROR: can't open %s %s %s in mode %s\n"
                            libName cellName viewName mode))
                   (t cells = (cons CellView cells))
                   )
             )
           (close file)
           )
          )
    cells
    )
  )



; this is a bit hackish, but the idea is that you can instantiate a cell
; inside the buswires cell that contains a collection of paths from a previous
; subcell. the instance name of the subcell serves as a hierarchical prefix
; to be attached to the names.
; this seemed the best way to reuse a collection of subcell pins which are
; not all on the same layer or contain a mixture of flippings, neither of which
; can be done in a DefChan. 
(defun ProcessBuswiresSubcell ()
 (let (wirescell wirescv subcell prefix newpath pathname newname flipx flipy) 
  wirescell=(GetGuideInst nil busGuideInstName)
  wirescv=(nrOpenCellViewWritable wirescell->libName wirescell->cellName wirescell->viewName ?mode "a")
  subcells=wirescv->instances

  ;clear any previously promoted paths
  (foreach shape wirescv->shapes
    (when (IsBusSubcellPath shape)
      (dbDeleteObject shape)
    )
  )

  (foreach subcell subcells
    prefix=subcell->name
    (foreach shape subcell->master->shapes
      (when shape->objType=="path" && (cadr shape->lpp)=="bus"
        newpath=(dbCopyFig shape wirescv subcell->transform)
        (SetBusSubcellProp newpath)
        pathname=(GetProp newpath "BusPrefix" nil)
        newname=(strcat prefix "." pathname)
        (dbReplaceProp newpath "BusPrefix" "string" newname)

        (when subcell->orient=="MX" || subcell->orient=="R180"
          flipy=(GetProp newpath "BusFlipY" nil)
          (if flipy then
            (dbReplaceProp newpath "BusFlipY" "boolean" nil)
          else
            (dbReplaceProp newpath "BusFlipY" "boolean" t)
          )
        )
        (when subcell->orient=="MY" || subcell->orient=="R180"
          flipx=(GetProp newpath "BusFlipX" nil)
          (if flipx then
            (dbReplaceProp newpath "BusFlipX" "boolean" nil)
          else
            (dbReplaceProp newpath "BusFlipX" "boolean" t)
          )
        )

      )
    )
  )
 )
)


(defun SetBusSubcellProp (obj)
  (when obj
    (dbReplaceProp obj "BusSubcellPath" "boolean" t)
  ) 
)

(defun IsBusSubcellPath (obj)
 (let (isbus)
  isbus = nil
  (when obj
    isbus=(dbGetPropByName obj "BusSubcellPath")
  )
  isbus 
 )
)

;; lot of cases ... logic not optimized !!!
(defun DoubleViaCorrection (point hv dir pattern flipX flipY)
 let( (newpoint corrected up Flip)
  corrected = nil
  Flip = (flipX || flipY) && !(flipX && flipY)
     corrected = t
     if( hv == "HV"
      then
        when( dir == "P" 
         if( prevdir == "P"
          then
            if( Flip 
            then
             newpoint = car(point)-(TrackPitch/24):cadr(point)
            else                                              
             newpoint = car(point)+(TrackPitch/24):cadr(point)
             )
          else
            if( Flip 
            then
             newpoint = car(point)-(TrackPitch/24):cadr(point)
            else                                              
             newpoint = car(point)+(TrackPitch/24):cadr(point)
             )
           )
         )
        when( dir == "N" 
         if( prevdir == "N"
          then
            if( Flip 
            then
             newpoint = car(point)+(TrackPitch/24):cadr(point)
            else                                              
             newpoint = car(point)-(TrackPitch/24):cadr(point)
             )
          else
            if( Flip 
            then  
             newpoint = car(point)+(TrackPitch/24):cadr(point)
            else                                              
             newpoint = car(point)-(TrackPitch/24):cadr(point)
             )
           )
         )
      else
        when( dir == "P" 
         if( prevdir == "P"
          then
            if( Flip 
            then
             newpoint = car(point)+(TrackPitch/24):cadr(point)
            else                                              
             newpoint = car(point)-(TrackPitch/24):cadr(point)
             )
          else
            if( Flip 
            then
             newpoint = car(point)-(TrackPitch/24):cadr(point)
            else                                              
             newpoint = car(point)+(TrackPitch/24):cadr(point)
             )
           )
         )
        when( dir == "N" 
         if( prevdir == "N"
          then
            if( Flip 
            then
             newpoint = car(point)-(TrackPitch/24):cadr(point)
            else                                              
             newpoint = car(point)+(TrackPitch/24):cadr(point)
             )
          else
            if( Flip 
            then
             newpoint = car(point)+(TrackPitch/24):cadr(point)
            else                                             
             newpoint = car(point)-(TrackPitch/24):cadr(point)
             )
           )
         )
      )

  list(newpoint corrected)
 )
)

;; lot of cases ... logic not optimized !!!
(defun VerticalViaCorrection (point hv prevdir dir pattern flipX flipY positiveVias)
 let( (newpoint corrected up Flip)
  corrected = nil
  Flip = (flipX || flipY) && !(flipX && flipY)
	if( !positiveVias
   then
     if( hv == "HV"
      then
        when( dir == "P" 
         if( prevdir == "P"
          then
            if( Flip 
            then
             newpoint = car(point):cadr(point)+(TrackPitch/24)
            else                                              
             newpoint = car(point):cadr(point)-(TrackPitch/24)
             )
          else
            if( Flip 
            then
             newpoint = car(point):cadr(point)-(TrackPitch/24)
            else                                              
             newpoint = car(point):cadr(point)+(TrackPitch/24)
             )
           )
         )
        when( dir == "N" 
         if( prevdir == "N"
          then
            if( Flip 
            then
             newpoint = car(point):cadr(point)-(TrackPitch/24)
            else                                              
             newpoint = car(point):cadr(point)+(TrackPitch/24)
             )
          else
            if( Flip 
            then
             newpoint = car(point):cadr(point)+(TrackPitch/24)
            else                                              
             newpoint = car(point):cadr(point)-(TrackPitch/24)
             )
           )
         )
      else
        when( dir == "P" 
         if( prevdir == "P"
          then
            if( Flip 
            then
             newpoint = car(point):cadr(point)-(TrackPitch/24)
            else                                              
             newpoint = car(point):cadr(point)+(TrackPitch/24)
             )
          else
            if( Flip 
            then
             newpoint = car(point):cadr(point)-(TrackPitch/24)
            else                                              
             newpoint = car(point):cadr(point)+(TrackPitch/24)
             )
           )
         )
        when( dir == "N" 
         if( prevdir == "N"
          then
            if( Flip 
            then
             newpoint = car(point):cadr(point)+(TrackPitch/24)
            else                                              
             newpoint = car(point):cadr(point)-(TrackPitch/24)
             )
          else
            if( Flip 
            then
             newpoint = car(point):cadr(point)+(TrackPitch/24)
            else                                              
             newpoint = car(point):cadr(point)-(TrackPitch/24)
             )
           )
         )
      )
   else
      newpoint = car(point):cadr(point)+(TrackPitch/24)
   )
  list(newpoint corrected)
 )
)

(defun ClassifyDirs (points)
  let( ((types (ClassifyPoints points)) Idx dirs currPt comparePt)

   for( Idx 0 length(points)-1

     when( nth(Idx types) == "VE" || nth(Idx types) == "HE" 
        dirs = cons(nil dirs)
       )

     when( nth(Idx types) == "EH"
       currPt = nth(Idx points)
       comparePt = nth(Idx+1 points)
       if( car(currPt) < car(comparePt)
        then
         dirs = cons("P" dirs)
        else
         dirs = cons("N" dirs)
         )
       )

     when( nth(Idx types) == "EV" 
       currPt = nth(Idx points)
       comparePt = nth(Idx+1 points)
       if( cadr(currPt) < cadr(comparePt)
        then
         dirs = cons("P" dirs)
        else
         dirs = cons("N" dirs)
         )
       )

     when( nth(Idx types) == "HV"
       currPt = nth(Idx points)
       comparePt = nth(Idx+1 points)
       if( cadr(currPt) < cadr(comparePt)
        then
         dirs = cons("P" dirs)
        else
         dirs = cons("N" dirs)
         )
        )

     when( nth(Idx types) == "VH"
       currPt = nth(Idx points)
       comparePt = nth(Idx+1 points)
       if( car(currPt) < car(comparePt)
        then
         dirs = cons("P" dirs)
        else
         dirs = cons("N" dirs)
         )
        )
    )

  reverse(dirs)
  )
)

