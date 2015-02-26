; Import LayGen *.gen files into Virtuoso cells
; Copyright Intel 2013
; Andrew Lines

; Laygen dimensions
MfgGrid = 1e-3     ; round dimensions of lables to this manufacturing grid
LaygenUnits = 1e-4 ; scale LayGen coordinates to Virtuoso
UV_dx = -0.035     ; x bloat of uv1/uv2 implant layers relative to diffusion
UV_dy =  0.021     ; y bloat of uv1/uv2 implant layers relative to diffusion

; Other settings
LaygenVersion = "d15q1ww05b_13752" ; sets a LaygenVersion cell property
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
LaygenLayerMap["polycon"]  = (list (list "gcn"   "drawing"))
LaygenLayerMap["viacn"]    = (list (list "vcn"   "drawing"))

LaygenLayerMap["metal0"]   = (list (list "m0"  "drawing"))
LaygenLayerMap["via0"]     = (list (list "v0"  "drawing"))
LaygenLayerMap["metal1"]   = (list (list "m1"  "drawing"))
LaygenLayerMap["via1"]     = (list (list "v1"  "drawing"))
LaygenLayerMap["metal2"]   = (list (list "m2"  "drawing"))

LaygenLayerMap["n"]    = (list (list "ndiff" "drawing"))
LaygenLayerMap["p"]    = (list (list "pdiff" "drawing"))
LaygenLayerMap["nuv1"] = (list (list "ndiff" "drawing")
                               (list "NV1" "drawing" UV_dx UV_dy))
LaygenLayerMap["puv1"] = (list (list "pdiff" "drawing")
                               (list "PV1" "drawing" UV_dx UV_dy))
LaygenLayerMap["nuv2"] = (list (list "ndiff" "drawing")
                               (list "NV2" "drawing" UV_dx UV_dy))
LaygenLayerMap["puv2"] = (list (list "pdiff" "drawing")
                               (list "PV2" "drawing" UV_dx UV_dy))

; Laygen pin direction mapping
LaygenPinMap = (makeTable "pinMap" nil)
LaygenPinMap["In"] = "input"
LaygenPinMap["Out"] = "output"

; which layers can turn into pins (and which should add overlapping pin layer)
LaygenPinLayers = (makeTable "pinLayers" nil)
LaygenPinLayers["nwell"] = t
LaygenPinLayers["metal1"] = t
LaygenPinLayers["metal2"] = t
LaygenPinPurposeExists = (makeTable "pinPurposeExists" nil)
LaygenPinPurposeExists["metal1"] = t
LaygenPinPurposeExists["metal2"] = t

; map net names
LaygenNetMap = (makeTable "netMap" nil)
LaygenNetMap[LaygenVssName] = GNDNetName
LaygenNetMap[LaygenVccName] = VddNetName

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Top Level Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Import a *.gen file into a Virtuoso cell
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
      (cond (type=="Bbox"  (LaygenBbox  CV fields))
            (type=="Track" (LaygenTrack CV fields))
            (type=="Wire"  (LaygenWire  CV fields pins))
            (type=="Pin"   (LaygenPin   CV fields pins))
            (type=="Device"
                   (LaygenDevice CV (append fields (parseString (gets line file) " \n"))))
            )
      )
    (close file)

    ; label pwell, set LaygenVersion property, save and return
    (LaygenRect CV (list (list "pwellSubIso" "id"))
                LaygenVssName pins[LaygenVssName] nil 0 0 GridPolyPitch GridDiffPitch)
    (dbReplaceProp CV "LaygenVersion" "string" LaygenVersion)
    (dbSave CV)
    CV
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define PRBoundary of cell
(defun LaygenBbox (CV fields)
  (let (xl yl xh yh)
    vars = (LaygenParseVars fields)
    xl = (LaygenCoord vars["xl"])
    xh = (LaygenCoord vars["xh"])
    yl = (LaygenCoord vars["yl"])
    yh = (LaygenCoord vars["yh"])
    (when (and xl xh yl yh)
      (dbCreatePRBoundary CV (list xl:yl xl:yh xh:yh xh:yl xl:yl))
      )
    )
  t
  )

; Draw a wire or via rectangle on several lpp's, with optional bloating and pin
(defun LaygenWire (CV fields pins)
  (let (vars xl yl xh yh lpps net)
    vars = (LaygenParseVars fields)
    xl = (LaygenCoord vars["xl"])
    xh = (LaygenCoord vars["xh"])
    yl = (LaygenCoord vars["yl"])
    yh = (LaygenCoord vars["yh"])
    lpps = LaygenLayerMap[vars["layer"]]
    pin = (if LaygenPinLayers[vars["layer"]] pins[vars["net"]] nil)
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
    vars = (LaygenParseVars fields)
    pins[vars["name"]] = LaygenPinMap[vars["dir"]]
    )
  )

; Draw a transistor
(defun LaygenDevice (CV fields)
  (let (vars lpps xl yl xh yh)
    vars = (LaygenParseVars fields)
    xl = (LaygenCoord vars["xl"])
    xh = (LaygenCoord vars["xh"])
    yl = (LaygenCoord vars["yl"])
    yh = (LaygenCoord vars["yh"])
    lpps = LaygenLayerMap[vars["model"]]
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
             (when fill && (cadr lpp)=="drawing" lpp = (list (car lpp) "fill"))
             rect = (dbCreateRect CV lpp (list xl-dx:yl-dy xh+dx:yh+dy))
             (when net && !fill
               net = (if LaygenNetMap[net] LaygenNetMap[net] net)
               rect->net = (dbFindNetByName CV net)
               (unless rect->net rect->net=(dbCreateNet CV net))
               (when pin
                 pinrect = (if pin_purpose
                               (dbCreateRect CV (list (car lpp) "pin") (list xl-dx:yl-dy xh+dx:yh+dy))
                             rect)
                 rect->net = rect->net
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
