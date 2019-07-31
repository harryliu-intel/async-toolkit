; Import LayGen *.gen files into Virtuoso cells
; Copyright Intel 2013
; Andrew Lines

; Laygen dimensions
MfgGrid = 1e-3     ; round dimensions of lables to this manufacturing grid
LaygenUnits = 1e-4 ; scale LayGen coordinates to Virtuoso
UV_dx = -0.035     ; x bloat of uv1/uv2 implant layers relative to diffusion
UV_dy =  0.021     ; y bloat of uv1/uv2 implant layers relative to diffusion

; Other settings
LaygenVersion = (let (id str)
                  id = (ipcBeginProcess "cd $LAYGENWARD; pwd -P")
                  (ipcWait id)
                  str = (ipcReadProcess id)
                  (car (last (parseString str "/\n")))
                  )
LaygenVssName = "vss"
LaygenVccName = "vcc"

; Map Laygen layers to Virtuoso LPPs. Each laygen layer generates a
; list of lppxy, which is (layerName purpose [bloatX] [bloatY]).  Only
; the first lpp of a list gets connectivity.  The rest are used for
; implied layers like transistor implants or complementary metal.
LaygenLayerMap = (makeTable "layerMap" nil)

LaygenLayerMap["nwell"]    = (list (list "nwell" "drawing"))
LaygenLayerMap["wirepoly"] = (list (list "poly"  "drawing"))
LaygenLayerMap["diffcon"]  = (list (list "tcn"   "drawing"))
LaygenLayerMap["vcg"]      = (list (list "vg"    "drawing"))
LaygenLayerMap["vct"]      = (list (list "vt"    "drawing"))
LaygenLayerMap["fti"]      = (list (list "FTI"   "drawing"))

LaygenLayerMap["metal0"]   = (list (list "m0"  "drawing"))
LaygenLayerMap["metalc0"]  = (list (list "m0"  "drawing"))
LaygenLayerMap["metals0"]  = (list (list "m0"  "drawing"))
LaygenLayerMap["via0"]     = (list (list "v0"  "drawing"))
LaygenLayerMap["metal1"]   = (list (list "m1"  "drawing"))
LaygenLayerMap["via1"]     = (list (list "v1"  "drawing"))
LaygenLayerMap["metal2"]   = (list (list "m2"  "drawing"))
LaygenLayerMap["metalc2"]  = (list (list "m2"  "drawing"))

LaygenLayerMap["nv1"]      = (list (list "NV1" "drawing"))
LaygenLayerMap["pv1"]      = (list (list "PV1" "drawing"))
LaygenLayerMap["nv2"]      = (list (list "NV2" "drawing"))
LaygenLayerMap["pv2"]      = (list (list "PV2" "drawing"))
LaygenLayerMap["nv3"]      = (list (list "NV3" "drawing"))
LaygenLayerMap["pv3"]      = (list (list "PV3" "drawing"))
;LaygenLayerMap["nuv1"] = (list (list "ndiff" "drawing")
;                               (list "nv1" "drawing" UV_dx UV_dy))
;LaygenLayerMap["puv1"] = (list (list "pdiff" "drawing")
;                               (list "pv1" "drawing" UV_dx UV_dy))
;LaygenLayerMap["nv2"] = (list (list "ndiff" "drawing")
;                               (list "nv2" "drawing" UV_dx UV_dy))
;LaygenLayerMap["pv2"] = (list (list "pdiff" "drawing")
;                               (list "PV2" "drawing" UV_dx UV_dy))
LaygenLayerMap["slinomn"]    = (list (list "sliNOMN" "drawing"))
LaygenLayerMap["slinomp"]    = (list (list "sliNOMP" "drawing"))
LaygenLayerMap["N"]    = (list (list "ndiff" "drawing"))
LaygenLayerMap["P"]    = (list (list "pdiff" "drawing"))

; Laygen pin direction mapping
LaygenPinMap = (makeTable "pinMap" nil)
LaygenPinMap["In"] = "input"
LaygenPinMap["Out"] = "output"

; which layers can turn into pins (and which should add overlapping pin layer)
LaygenPinLayers = (makeTable "pinLayers" nil)
LaygenPinLayers["metal1"] = t
LaygenPinLayers["metal2"] = t
LaygenPowerLayers = (makeTable "powerLayers" nil)
LaygenPowerLayers["nwell"] = t
LaygenPowerLayers["polycon"] = t
LaygenPowerLayers["metal0"] = t
LaygenPowerLayers["metal2"] =t
LaygenPinPurposeExists = (makeTable "pinPurposeExists" nil)
LaygenPinPurposeExists["polycon"] = t
LaygenPinPurposeExists["metal0"] = t
LaygenPinPurposeExists["metal1"] = t
LaygenPinPurposeExists["metal2"] = t

; map net names
LaygenNetMap = (makeTable "netMap" nil)

LaygenNetMap[LaygenVssName] = GNDNetName
LaygenNetMap[LaygenVccName] = VddNetName

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Top Level Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Import a *.lgf file into a Virtuoso cell
(defun LaygenImport (filename libName cellName @key (viewName "layout"))
  (let (file fields type pins CV)
    file = (infile filename)
    pins = (makeTable "pins" nil)
    pins[LaygenVssName] = "input"
    pins[LaygenVccName] = "input"
    CV = (dbOpenCellViewByType libName cellName viewName "maskLayout" "w")
    (unless file (error "Can't read %s" filename))
    (dbReplaceProp CV "CastCell" "boolean" t)
    (while (gets line file)
      fields = (parseString line " \n")
      type = (car fields)
      fields = (cdr fields)
      (cond (type=="Cell"  (LaygenBbox  CV fields))
            ;(type=="Track" (LaygenTrack CV fields))
            (type=="Wire"  (LaygenWire  CV fields pins))
            (type=="Pin"   (LaygenPin   CV fields pins))
            (type=="Device"
                   ;(LaygenDevice CV (append fields (parseString (gets line file) " \n"))))
		   (LaygenDevice CV fields))
            )
      )
    (close file)

    ; add power hookup shapes
    ;(LaygenFixPowerHookup ?CV CV)

    ; set LaygenVersion property, save and return
    (dbReplaceProp CV "LaygenVersion" "string" LaygenVersion)
    (dbSave CV)
    CV
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; label the pwell for LVS
(defun LaygenLabelPwell (@key (CV (geGetEditCellView)) (netName GNDNetName))
  (let (lpp xy)
    lpp = PWellIsoLPP
    (foreach shape CV->shapes (when shape->lpp==lpp (dbDeleteObject shape)))
    (when CV->prBoundary
      xy = (car (SortVertices CV->prBoundary->points))
      (dbCreateLabel CV lpp xy netName "lowerLeft" "R0" "stick" 0.02)
      (dbCreateRect CV lpp (list xy (car xy)+GridPolyPitch:(cadr xy)+GridDiffPitch))
      )
    )
  t
  )

; label the nwell for LVS
(defun LaygenLabelNwell (@key (CV (geGetEditCellView)) (netName VddNetName))
  (let (lpp xy)
    lpp = NWellLPP
    (foreach shape CV->shapes (when shape->lpp==lpp (dbDeleteObject shape)))
    (when CV->prBoundary
      xy = (car (SortVertices CV->prBoundary->points))
      xy = (car xy):(cadr xy)+GridPitch/2
      (dbCreateLabel CV lpp xy netName "upperLeft" "R0" "stick" 0.02)
      (dbCreateRect CV lpp (list xy (car xy)+GridPolyPitch:(cadr xy)-GridDiffPitch))
      )
    )
  t
  )

; Define PRBoundary of cell
(defun LaygenBbox (CV fields)
  (let (xl yl xh yh coords points)
    vars = (LaygenParseVars (cdr fields))
    coords = vars["bbox"]
    points = (parseString coords ":")
    xl = (LaygenCoord (car points))
    yl = (LaygenCoord (cadr points))
    xh = (LaygenCoord (caddr points))
    yh = (LaygenCoord (cadddr points))
    (when (and xl xh yl yh)
      (dbCreatePRBoundary CV (list xl:yl xl:yh xh:yh xh:yl xl:yl))
      (LaygenLabelPwell ?CV CV)
      )
    )
  t
  )

; Draw a wire or via rectangle on several lpp's, with optional bloating and pin
(defun LaygenWire (CV fields pins)
  (let (vars xl yl xh yh lpps net)
    vars = (LaygenParseVars fields)
    rect = (parseString vars["rect"] ":")
    xl = (Ru_StringToInt (car rect)) * LaygenUnits
    yl = (Ru_StringToInt (cadr rect)) * LaygenUnits
    xh = (Ru_StringToInt (caddr rect)) * LaygenUnits
    yh = (Ru_StringToInt (cadddr rect)) * LaygenUnits
    lpps = LaygenLayerMap[vars["layer"]]
    pin = (if vars["net"] && (vars["net"]==LaygenVssName || vars["net"]==LaygenVccName)
              (if LaygenPowerLayers[vars["layer"]] pins[vars["net"]] nil)
              (if LaygenPinLayers[vars["layer"]]   pins[vars["net"]] nil)
              )
    (LaygenRect CV lpps vars["net"] pin LaygenPinPurposeExists[vars["layer"]] xl yl xh yh)
    )
  t
  )

; Unused
(defun LaygenTrack (CV fields)
  t
  )

; Identify pins
(defun LaygenPin (CV fields pins)
  (let (vars)
    vars = (LaygenParseVars (cdr fields))
    ;pins[vars["name"]] = LaygenPinMap[vars["dir"]]
    pins[(car fields)] = LaygenPinMap[vars["direction"]]
    )
  )

; Draw a transistor
(defun LaygenDevice (CV fields)
  (let (vars );name lpps xl yl xh yh offsets)
    vars = (LaygenParseVars fields)
    name = (car fields)

    x = (atoi vars["x"])
    y = (atoi vars["y"])
    r = (atoi vars["r"])
    w = (LaygenCoord vars["w"])
    l = (LaygenCoord vars["l"])

    xo = 0.054 + x*GridPolyPitch*0.9 ; laygen layout not yet scaled to 60nm poly pitch
    ; This is super hackish. I hope to come back around and compute this
    ; in a more direct fashion
    offsets = (makeTable "yoffsets" nil)
    offsets["N340y1r0"] = 0.119
    offsets["N680y1r0"] = 0.102
    offsets["P340y0r0"] = 0.221
    offsets["P680y0r0"] = 0.238
    offsets["P340y0r1"] = 0.459
    offsets["P680y0r1"] = 0.442
    offsets["N340y0r1"] = 0.527
    offsets["N340y1r1"] = 0.527
    offsets["N680y0r1"] = 0.544
    offsets["N680y1r1"] = 0.544
    yl = offsets[(sprintf nil "%s%sy%dr%d" vars["model"] vars["w"] y r)] - w/2
    xl = xo - 0.044
    xh = xo + 0.044
    yh = yl + w
    lpps = LaygenLayerMap[vars["model"]]
    (printf "Making Device %s x=%d y=%d r=%d xl=%f yl=%f \n" name x y r xl yl)
    (LaygenRect CV lpps nil nil nil xl yl xh yh)
    t
    )
  )

; Convert a LayGen coordinate to Virtuoso
(defun LaygenCoord (str)
  (if str (atoi str) * LaygenUnits nil)
  )

; Parse a bunch of a=b strings into a table
(defun LaygenParseVars (fields)
  (let (table ve)
    table = (makeTable "variables" nil)
    (foreach f fields
             ve = (parseString f "=")
             (when (and ve (length ve)==2) table[(car ve)] = (cadr ve))
             )
    table
    )
  )

; Draw bloat/shrunk rectangles on several layers
(defun LaygenRect (CV lpps net pin pin_purpose xl yl xh yh)
  (let (rect pinrect lpp dx dy fill)
    fill = net && (strncmp net "!" 1)==0
    (foreach lppxy lpps
             lpp = (list (car lppxy) (cadr lppxy))
             dx = (caddr lppxy)
             (unless dx dx=0)
             dy = (cadddr lppxy)
             (unless dy dy=0)
	     ; FIXME. I commented out this "fill" purpose translation
             ;(when fill && (cadr lpp)=="drawing" lpp = (list (car lpp) "fill"))
	     ;(printf "Making rect lpp=%s %s %f,%f %f,%f\n" (car lpp) (cadr lpp) xl yl xh yh)
             rect = (dbCreateRect CV lpp (list xl-dx:yl-dy xh+dx:yh+dy))
             (when net && !fill
               net = (if LaygenNetMap[net] LaygenNetMap[net] net)
               rect->net = (dbFindNetByName CV net)
               (unless rect->net rect->net=(dbCreateNet CV net))
               (when pin
                 pinrect = (if pin_purpose
                               (dbCreateRect CV (list (car lpp) "pin") (list xl-dx:yl-dy xh+dx:yh+dy))
                             rect)
                 (dbCreatePin rect->net pinrect)->term->direction = pin
                 (LaygenLabelPin CV pinrect)
                 )
               net = nil ; only first lpp gets net
               )
             )
    )
  t
  )

; attach a label to a pin
(defun LaygenLabelPin (view fig)
  (let (x0 y0 x1 y1 x y rotation height)
    x0 = (car  (car  fig->bBox))
    y0 = (cadr (car  fig->bBox))
    x1 = (car  (cadr fig->bBox))
    y1 = (cadr (cadr fig->bBox))
    x = (x0+x1)/2
    y = (y0+y1)/2
    x = MfgGrid * (round x/MfgGrid) 
    y = MfgGrid * (round y/MfgGrid)
    (cond (x1-x0>y1-y0 rotation = "R0"  height = y1-y0)
          (t           rotation = "R90" height = x1-x0)
          )
    height = MfgGrid * (round height/MfgGrid/3) ; adjust height
    label = (dbCreateLabel view fig->lpp x:y fig->net->name
                           "centerCenter" rotation "stick" height) ; draw the label
    label->parent = fig ; label moves with figure
    )
  t
  )

; Adds tcn shapes where gcn power hookups are needed
(defun LaygenFixPowerHookup (@key (CV (geGetEditCellView)))
  (let (bbox x0 y0 x1 y1 ya h w gcn tcn tcn_lpps xl xh yl yh)
    bbox = CV->prBoundary->bBox
    x0 = (car (car bbox))
    y0 = (cadr (car bbox))
    x1 = (car (cadr bbox))
    y1 = (cadr (cadr bbox))
    ya = (cadr gridAlignment[2])/2
    h = (y1-y0)/ya
    w = 0.028
    gcn = (list "gcn" "pin")
    tcn = (list "tcn" "drawing")
    tcn_lpps = (list tcn)
    (foreach x (list x0 x1)
             xl = x-w/2
             xh = x+w/2
             yl = 0.021
             yh = 0.147
             (when (dbGetOverlaps CV (list xl:yl xh:yh) gcn 0)
               (LaygenRect CV tcn_lpps GNDNetName "input" t xl yl xh yh)
               )
             yl = 2*ya-0.147
             yh = 2*ya-0.021
             (when (dbGetOverlaps CV (list xl:yl xh:yh) gcn 0)
               (LaygenRect CV tcn_lpps GNDNetName "input" t xl yl xh yh)
               )
             (when nil ; D04 cells don't seem to need tcn for vcc
               yl = 0.252
               yh = 0.42
               (when (dbGetOverlaps CV (list xl:yl xh:yh) gcn 0)
                 (LaygenRect CV tcn_lpps VddNetName "input" t xl yl xh yh)
                 )
               yl = 2*ya-0.42
               yh = 2*ya-0.252
               (when (dbGetOverlaps CV (list xl:yl xh:yh) gcn 0)
                 (LaygenRect CV tcn_lpps VddNetName "input" t xl yl xh yh)
                 )
               )
             )
    (dbSave CV)
    CV
    )
  )

