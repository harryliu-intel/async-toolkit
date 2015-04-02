; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/bus/bus.il#5 $
; $DateTime: 2006/12/09 12:58:15 $
; $Author: lines $
;
; User must set bundled_channels in wires.il.  The syntax is a list of
; lists.  The inner list starts with the BusPattern name followed by 1
; or more BusPrefix's.  For example:
;
; bundled_channels = (list (list "e1of4" "x" "y") (list "e1of2" "z"))
;
; The [lo..hi] will be expanded.

; Export, Route, Import using CCAR with bundled channels
(defun BundledRoute
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (bottomMetal nil)        ; bottom metal layer to use, or nil to use wires.il
        (topMetal nil)           ; top metal layer to use, or nil to use wires.il
        (routeWires t)           ; route regular wires in parallel with bundled channels
        (SaveQuit nil)           ; automatically save and quit CCAR
        (Background nil)         ; run in foreground or background
        (viaType "c")            ; via type ("c"/"min")
        (subcellView "abstract_edit abstract") ; alternate views of subcells 
        (importView "layout_pg") ; view name to import wiring back to
        (type "fat")             ; routing rul/do file to use from PDK ("fat"/"fat_onepass")
        (retry t)                ; automatic retry
        )
  (let (TEMP FulcrumPDKRoot RouterPDK DoFile RouterDoFile BundledDoFile SaveQuitStr
             LCV rules options session status win flattenCV cmd)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP" )

    ; make flatten view automatically
    (when (or CV->viewName=="floorplan" CV->viewName=="prelayout")
      CV = (MakeFlatten ?CV CV))
    flattenCV = CV

    ; get config settings
    SaveQuit = (or SaveQuit Background)
    FulcrumPDKRoot = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    RouterPDK = (sprintf nil "%s/share/Fulcrum/cell_automation/router" FulcrumPDKRoot)
    DoFile = (sprintf nil "%s/%s.do" RouterPDK type)
    SaveQuitStr = (sprintf nil "-do %s/savequit.do" RouterPDK)
    
    ; load wires.il and set options
    (LoadWires)
    (unless bundled_channels (error "No bundled_channels defined in wires.il\n"))
    (unless bottomMetal bottomMetal = bundled_bottom_layer)
    (unless topMetal    topMetal    = bundled_top_layer)

    ; prepare bundled view
    CV = (CreateBundledView CV routeWires subcellView ?bottomMetal bottomMetal)

    ; write do files
    (when routeWires
      RouterDoFile  = (WriteRouterDoFile CV ?viaType viaType
                                         ?bottomMetal bottomMetal ?topMetal topMetal))
    BundledDoFile = (WriteBundledDoFile CV ?bottomMetal bottomMetal ?topMetal topMetal
                                        ?routeWires routeWires)

    ; options
    LCV = (list CV->libName CV->cellName CV->viewName)
    rules = (sprintf nil "%s/%s.rul" RouterPDK type)
    options = (sprintf nil "-guidir %s -noclean -virtuoso -do %s %s -do %s %s"
                       TEMP BundledDoFile
                       (if routeWires (sprintf nil "-do %s" RouterDoFile) "")
                       DoFile
                       (if SaveQuit SaveQuitStr ""))
    session = (sprintf nil "%s/%s.ses" TEMP CV->cellName)
    (shell (sprintf nil "rm -f %s" session))

    ; export
    status = (iccExportCellview
              ?layoutLCV LCV
              ?background nil
              ?exportDirectory TEMP
              ?rulesFile rules
              ?conductorDepth 32
              ?keepoutDepth 32
              ?pinConnection "strong"
              ?optionList (list "fullConnectivity" "interLayer")
              ?startICC nil)
    (unless status (error "Unable to export to VCAR\n"))

    ; route
    cmd = (sprintf nil "vcar %s/%s.dsn %s %s" TEMP CV->cellName options
                   (if Background "-nog &" ""))
    (printf "%s\n" cmd)
    status = nil
    (while !status
      status = (shell cmd)
      (unless status || retry (error "VCAR failed.  Inspect *.did file.\n"))
      (unless status
        (printf "Unable to run VCAR, retrying...\n")
        (sleep 30)
        )
      )

    ; import from VCAR
    (cond (Background (printf "Running in background, finish with BundledRouteImport\n"))
          (t CV = (BundledRouteImport ?CV flattenCV ?importView importView))
          )
    CV
    )
  )

; Import from VCAR and post-process
(defun BundledRouteImport
  (@key (CV (geGetEditCellView))
        (importView "layout_pg")
        )
  (let (TEMP session status win)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP" )
    session = (sprintf nil "%s/%s.ses" TEMP CV->cellName)

    ; import from CCAR
    status = (iccImportCellview
              ?layoutLCV (list CV->libName CV->cellName "bundled")
              ?iccFile session
              ?background nil)
    (unless status (error "Unable to import from VCAR\n"))
           
    ; post-process the routes
    CV = (FinishBundledImport CV ?importView importView)

    ; change to importView
    win = (hiGetCurrentWindow)
    (unless win (error "Can't change to view %s\n" importView))
    (geOpen ?window win
            ?lib  CV->libName
            ?cell CV->cellName
            ?view CV->viewName
            ?viewType "maskLayout"
            ?mode "a")

    ; draw new bundled bus wires only
    (DrawTracks ?guideInstName "autobuswires")

    ; canonicalize net names
    (CanonicalizeNets ?CV CV)

    ; execute custom function
    (FinishRouterImport)

    ; save importView
    (dbSave CV)
    CV
    )
  )

; write FQCN_bundled.do for CCAR
(defun WriteBundledDoFile (CV @key (bottomMetal 2) (topMetal 7) (routeWires nil))
  (let (name TEMP file nets)

    ; find all matching e rails
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")

    ; write CCAR do file
    name = (sprintf nil "%s/%s_bundled.do" TEMP CV->cellName)
    file = (outfile name "w")
    (unless routeWires (fprintf file "define (class RESET_3W3S)\n")) ; make sure it exists
    (fprintf file "# Bundled channel rules\n")
    (fprintf file "define (class BUNDLED_CHANNEL\n")
    (foreach net (BundledNets CV) (fprintf file "  %s\n" net->name))
    (fprintf file ")\n")
    (fprintf file "rule class BUNDLED_CHANNEL (width 2.28)\n")
    (fprintf file "rule class BUNDLED_CHANNEL (clearance 0.12)\n")
    (fprintf file "rule class BUNDLED_CHANNEL (pin_width_taper off)\n")
    (fprintf file "rule class BUNDLED_CHANNEL (limit_way 0)\n")
    (fprintf file "circuit class BUNDLED_CHANNEL (length 1.1 -1 (type ratio))\n")
    (fprintf file "circuit class BUNDLED_CHANNEL (length 900 -1 (type actual))\n")
    (fprintf file "circuit class BUNDLED_CHANNEL (use_via")
    (for layer bottomMetal topMetal
         (when layer>=3 (fprintf file " M%d_M%d_FAT" layer layer-1)))
    (fprintf file ")\n")
    (close file)
    name
    )
  )

; Prepare Bundled View for Export
(defun CreateBundledView (CV routeWires subcellView @key (bottomMetal 2))
  (let (bundled_nets GND Vdd)

    ; copy to bundled view
    CV = (betterCopyCellView CV CV->libName CV->cellName "bundled" nil nil t)

    ; DrawBundledPins
    (DrawBundledPins CV)

    ; change subcells to subcellView
    (when subcellView!=""
          (foreach inst CV->instances
                   (when inst->libName!=TechLibName
                     (ChangeView inst (parseString subcellView " "))
                     )
                   )
          )

    ; flatten subcells and vias for CCAR
    (FlattenAbstractSubcells CV)

    ; delete labels
    (DeleteLabels ?CV CV)

    ; unconnect individual rails of bundled channels
    (DisconnectBundledPins CV)

    ; delete non-bundled nets
    (when routeWires==nil (DeleteNonBundledNets CV))

    ; delete power supply nets
    GND = (dbFindNetByName CV "GND")
    (when GND (dbDeleteObject GND))
    Vdd = (dbFindNetByName CV "Vdd")
    (when Vdd (dbDeleteObject Vdd))

    ; convert unconnected shapes to keepout
    (ConvertUnconnectedShapesToKeepout CV)

    ; convert preroutes to polygons instead of paths
    (ConvertPreroutedPaths CV)

    ; draw M2 keepout stripes
    (when bottomMetal<=2 (DrawMetal2KeepoutStripes CV))

    ; return bundled CellView
    (dbSave CV)
    CV
    )
  )

; Post-processing after import from CCAR
(defun FinishBundledImport (CV @key (importView "layout_pg"))
  (let (srcCV dstCV pgCV buswires libName cellName viewName
        components baseName win newobj)
    libName  = CV->libName
    cellName = CV->cellName
    viewName = CV->viewName
    win = (hiGetCurrentWindow)

    ; create name of buswires cell
    components = (parseString cellName ".")
    baseName = (cadr (reverse components))
    buswires = (strcat libName ".wires." baseName "_autobuswires")

    ; find relevant views
    dstCV = (dbOpenCellViewByType libName buswires "layout" "maskLayout" "w")
    srcCV = (dbOpenCellViewByType libName cellName "bundled")
    (cond (importView!=CV->viewName
           pgCV  = (betterCopyCellView CV libName cellName importView nil nil t))
          (t pgCV=CV)
          )
    (if (and dstCV srcCV pgCV)==nil (error "Unable to open import views\n"))

    ; draw guide paths to autobuswires cell
    win = (geOpen ?window win ?lib dstCV->libName
                  ?cell dstCV->cellName ?view dstCV->viewName
                  ?viewType "maskLayout" ?mode "a" )
    (DrawBundledChannels srcCV)
    (dbSave srcCV)
    (dbSave dstCV)

    ; import remaining wires to layout_pg view
    win = (geOpen ?window win ?lib libName
                  ?cell cellName ?view "layout_pg"
                  ?viewType "maskLayout" ?mode "a")

    ; delete prerouted contacts
    (DeleteDuplicateContacts srcCV)

    ; copy new wires and vias back to layout_pg
    (CopyPathsAndContactsWithConnectivity srcCV pgCV)

    ; add autobuswires instance
    (dbCreateInst pgCV dstCV "autobuswires" 0:0 "R0")

    ; save and return layout_pg
    (dbSave pgCV)
    pgCV
    )
  )

; delete prerouted contacts in bundled view
(defun DeleteDuplicateContacts (CV)
  (let (net)
    (foreach contact CV->instances
             (when (and contact->libName==TechLibName contact->terminals)
               net = (car contact->terminals)->net
               (when (and net (dbGetPropByName net "BusPrefixes"))
                 (dbDeleteObject contact)
                 )
               )
             )
    )
  t
  )

; shortcut to add autobuswires to current CV
(defun AddAutoBusWires (@key (CV (geGetEditCellView)))
  (AddBusWires ?CV CV ?name "autobuswires")
  )

; enumerate bundled nets
(defun BundledNets (CV)
  (setof net CV->nets (dbGetPropByName net "BusPrefixes")!=nil)
  )

; flatten abstract subcells
(defun FlattenAbstractSubcells (CV)
  (foreach inst CV->instances
           (cond (inst->libName==TechLibName nil)
                 (t
                  (dbFlattenInst inst 1 nil t t)
                  )
                 )
           )
  t
  )

; disconnect shapes that overlap bundled pins
(defun DisconnectBundledPins (CV)
  (let (overlaps)
    (foreach net (BundledNets CV)
     (foreach pin net->pins
      (when (GetProp pin->fig "PinType" nil)=="BundledPin"
            overlaps = (FindMetalOverlaps CV pin->fig->bBox pin->fig->layerName)
            (foreach obj overlaps
                     (when (GetProp obj "PinType" nil)!="BundledPin"
                           (dbReplaceProp obj->net "BundledNet" "boolean" t)
                           obj->lpp = (list (car obj->lpp) "boundary")
                           (when obj->pin (dbDeleteObject obj->pin))
                           )
                     )
            overlaps = (FindContactOverlaps CV pin->fig->bBox pin->fig->layerName)
            (foreach obj overlaps
                     (foreach term obj->terminals term->net=nil)
                     )
            )
      )
     )
    )
  t
  )

; delete nets that aren't bundled
(defun DeleteNonBundledNets (CV)
  (foreach net CV->nets
           (when (dbGetPropByName net "BusPrefixes")==nil
                 (dbDeleteObject net))
           )
  t
  )

; turn unconnected shapes into keepout
(defun ConvertUnconnectedShapesToKeepout (CV)
  (let (layer purpose)
    (foreach shape CV->shapes
             layer   = (car  shape->lpp)
             purpose = (cadr shape->lpp)
             (when (and shape->net==nil (isMetal layer)
                        (or purpose=="drawing" purpose=="net"
                            purpose=="vdd" purpose=="gnd"))
               shape->lpp = (list (car shape->lpp) "boundary")               )
             )
    )
  t
  )

; paint a grid of M2 keepout to force fat wires to be on grid
(defun DrawMetal2KeepoutStripes (CV)
  (let (bBox x0 x1 y0 y1 i0 i1 j0 j1 scratchlpp1 scratchlpp2)
    ; draw stripes
    scratchlpp1=NRegionLPP
    scratchlpp2=PRegionLPP
    bBox = CV->bBox
    x0 = (car (car bBox))
    x1 = (car (cadr bBox))
    y0 = (cadr (car bBox))
    y1 = (cadr (cadr bBox))
    i0 = (round (x0+0.24)/2.88)
    i1 = (round (x1-0.24)/2.88)
    j0 = (round (y0+0.24)/2.88)
    j1 = (round (y1-0.24)/2.88)
    (for i i0 i1 (dbCreateRect CV scratchlpp1 (list 2.88*i-0.18:y0 2.88*i+0.18:y1)))

    ; trim M2 stripes keepout around M2 pins
    (foreach purpose (list "net" "drawing" "pin")
             (leLayerSize CV (list "M2" purpose) DefaultWiringSpacing scratchlpp2)
             )
    (leLayerAndNot CV scratchlpp1 scratchlpp2 Metal2byLPP)
    (foreach shape CV->shapes
             (when shape->lpp==scratchlpp2 (dbDeleteObject shape))
             (when shape->lpp==scratchlpp1 (dbDeleteObject shape))
             )
    )
  t
  )

; Convert prerouted paths to polygons to distinguish them from routed wires
(defun ConvertPreroutedPaths (CV)
  (foreach path CV->shapes
           (when path->objType=="path" (dbConvertPathToPolygon path))
           )
  t
  )

; search for nets of overlapping bundled pin
(defun FindOverlappingBundledShape (CV bbox lpp)
  (let (overlaps overlap)
    overlap = nil
    overlaps = (dbGetOverlaps CV bbox lpp 0)
    (foreach shape overlaps
             (when (GetProp shape "PinType" nil)=="BundledPin"
                   overlap = shape
                   )
             )
    overlap
    )
  )

; draw bundled pin
(defun DrawBundledPin (net bbox lpp pattern flip)
  (let (horz x0 y0 x1 y1 x y flipX flipY rect oshape onet p prefixes patterns small)
    x  = (car (car bbox))
    y  = (cadr (car bbox))
    x1 = (car (cadr bbox))
    y1 = (cadr (cadr bbox))
    x0 = (min x x1)
    x1 = (max x x1)
    y0 = (min y y1)
    y1 = (max y y1)
    flipX = nil
    flipY = nil
    small = nil
    x = (x0+x1)/2
    y = (y0+y1)/2
    horz = (IsLayerHorizontal (car lpp))
    (cond (horz
           y0 = (round y0/2.88-0.5)*2.88 + 0.18
           y1 = (round y1/2.88+0.5)*2.88 - 0.18
           flipY = y-y0>y1-y != flip
           (when (round (x0-0.3)/2.88+0.5)>=(round (x1+0.3)/2.88-0.5) small=t)
           )
          (t
           x0 = (round x0/2.88-0.5)*2.88 + 0.18
           x1 = (round x1/2.88+0.5)*2.88 - 0.18
           flipX = x-x0>x1-x != flip
           (when (round (y0-0.3)/2.88+0.5)>=(round (y1+0.3)/2.88-0.5) small=t)
           )
          )
    (when small (printf "WARNING: pin %s at %g:%g on %s is small or poorly aligned.\n"
                        net->name x y (car lpp)))
    bbox = (list x0:y0 x1:y1)
    oshape = (FindOverlappingBundledShape CV bbox lpp)
    (cond (oshape
           ; combine with overlapping bundled pin
           onet = oshape->net
           oshape->bBox = (BBoxCombine oshape->bBox bbox)
           p = (dbGetPropByName onet "BusPrefixes")
           (cond ((member net->name p->value)==nil
                  (printf "Appending %s to same path as %s\n" net->name (car p->value))
                  prefixes = (cons net->name p->value)
                  p = (dbGetPropByName onet "BusPatterns")
                  patterns = (cons pattern p->value)
                  (dbReplaceProp onet "BusPrefixes" "list" prefixes)
                  (dbReplaceProp onet "BusPatterns" "list" patterns)
                  )
                 )
           )
          (t
           ; create new bundled pin
           rect = (dbCreateRect CV lpp bbox)
           (dbReplaceProp rect "PinType" "string" "BundledPin")
           (cond (horz (dbReplaceProp rect "FlipY" "boolean" flipY))
                 (t    (dbReplaceProp rect "FlipX" "boolean" flipX))
                 )
           (dbCreatePin net rect)
           (cond (horz rect->pin->accessDir = (list "left" "right"))
                 (t rect->pin->accessDir = (list "top" "bottom"))
                 )
           (cond ((dbGetPropByName net "BusPrefixes")==nil
                  bundled_nets = (cons net bundled_nets)))
           (dbReplaceProp net "BusPrefixes" "list" (list net->name))
           (dbReplaceProp net "BusPatterns" "list" (list pattern))
           )
          )
    )
  t
  )

; Expand bundled_channels for debugging
(defun MatchBundledChannelsCore (CV)
  (let (names canonMap canonName patlist pattern i buspattern name net result net_tbl
              railflip)
    ; first pass to find net names
    (foreach entry bundled_channels
      patlist = (car entry)
      patlist = (if (atom patlist) (list patlist) patlist)
      i=0
      (foreach chan (ExpandArrayRanges (cdr entry))
        pattern = (nth (mod i (length patlist)) patlist)
        i=i+1
        buspattern = (MapBusPattern pattern)
        (unless buspattern (error "BusPattern %s unsupported\n" pattern))
        railflip = (BusPatternReference buspattern)
        name = (sprintf nil "%s%s" chan (car railflip))
        names = (cons name names)
        )
      )
    ; query cast to get canonical name mapping
    canonMap = (CanonicalizeNetNames names ?CV CV)
    net_tbl = (makeTable "net added" nil)
    ; second pass
    (foreach entry bundled_channels
      patlist = (car entry)
      patlist = (if (atom patlist) (list patlist) patlist)
      i=0
      (foreach chan (ExpandArrayRanges (cdr entry))
        pattern = (nth (mod i (length patlist)) patlist)
        i=i+1
        buspattern = (MapBusPattern pattern)
        (unless buspattern (error "BusPattern %s unsupported\n" pattern))
        railflip = (BusPatternReference buspattern)
        name = (sprintf nil "%s%s" chan (car railflip))
        canonName = (arrayref canonMap name)
        (unless canonName (error "%s is not a local node\n" name))
        net  = (dbFindNetByName CV canonName)
        (unless net (error "Can't find rail %s\n" canonName))
        e = (list pattern chan buspattern net (cadr railflip))
        (unless net_tbl[net] ; uniquify list
          net_tbl[net] = t
          result = (append result (list e))
          )
        )
      )
    result
    )
  )

; Pick a reference rail to detect flipX/flipY.  Returns the suffix and
; a boolean for if it should be in the top half of a track.
(defun BusPatternReference (pattern)
  (let (suffix flip)
    (foreach rail (reverse pattern)
             (when (caddr rail)!=0 && (car rail)!="GND"
                   suffix=(car rail) flip=(caddr rail)>0)
             )
    (unless suffix suffix=(car (car pattern)))
    (list suffix flip)
    )
  )

; for user feedback
(defun MatchBundledChannels (@key (CV (geGetEditCellView)))
  (LoadWires)
  (foreach entry (MatchBundledChannelsCore CV)
           (printf "Pattern=%s Channel=%s\n" (car entry) (cadr entry))
           )
  t
  )

; convert to bundled pins
(defun DrawBundledPins (CV)
  (let (pattern chan buspattern net cnet transform ubox)
    (foreach entry (MatchBundledChannelsCore CV)

      ; extract channel information
      pattern    = (car entry)
      chan       = (cadr entry)
      buspattern = (caddr entry)
      net        = (cadddr entry)
      flip       = (nth 4 entry)
      (printf "Pattern=%s Channel=%s\n" pattern chan)

      ; create net for channel
      cnet = (MakeNet CV chan)
        
      ; paint fat pins over top-level bundled pins
      (foreach pin net->pins
               (DrawBundledPin cnet pin->fig->bBox pin->fig->lpp pattern flip))
               
      ; paint fat pins over subcell bundled pins
      (foreach instTerm net->instTerms
               inst = instTerm->inst
               (cond (inst->libName!=TechLibName
                      transform = (geGetInstTransform inst)
                      (foreach pin instTerm->term->pins
                               ubox = (geTransformUserBBox
                                       pin->fig->bBox transform)
                               (DrawBundledPin cnet ubox
                                               pin->fig->lpp pattern flip)
                               )
                      )
                     )
               )
      
      ; paint fat pins over prerouted nets
      (foreach fig net->figs
               (DrawBundledPin cnet fig->bBox fig->lpp pattern flip)
               )
      )
    t
    )
  )

; draw all declared bundled channels
(defun DrawBundledChannels (CV)
  (foreach net (BundledNets CV) (DrawBundledChannel CV net))
  t
  )

; draw just one bundled channel by channel name
(defun DrawBundledChannel (CV net)
  (let (selected delpaths paths contacts overlaps
                 layers points xy x y flipX flipY pattern prefix)
    
    ; channel properties
    (printf "Guide Channel=%s\n" net->name)

    ; overlapping patterns and prefixes
    prefix  = (buildString (dbGetPropByName net "BusPrefixes")->value " ")
    pattern = (buildString (dbGetPropByName net "BusPatterns")->value " ")

    ; find paths and contacts for bundled channels
    paths = (setof obj CV->shapes
                   (and obj->objType=="path"
                        obj->net==net)
                   )
    contacts = (setof obj CV->instances
                      (and obj->objType=="inst"
                           obj->libName==TechLibName
                           (BBoxGetWidth  obj->bBox)>PowerGridPitch/2
                           (BBoxGetHeight obj->bBox)>PowerGridPitch/2
                           (or obj->cellName=="M7_M6_FAT"
                               obj->cellName=="M6_M5_FAT"
                               obj->cellName=="M5_M4_FAT"
                               obj->cellName=="M4_M3_FAT"
                               obj->cellName=="M3_M2_FAT"
                               ; NOTE: CCAR doesn't use FAT vias reliably!
                               obj->cellName=="M7_M6c"
                               obj->cellName=="M6_M5c"
                               obj->cellName=="M5_M4c"
                               obj->cellName=="M4_M3c"
                               obj->cellName=="M3_M2c"
                               obj->cellName=="M7_M6min"
                               obj->cellName=="M6_M5min"
                               obj->cellName=="M5_M4min"
                               obj->cellName=="M4_M3min"
                               obj->cellName=="M3_M2min")
                           (car obj->instTerms)->net==net )
                      )

    ; figure out if paths or contacts are flipY or flipX
    (foreach path paths (MarkPath CV path))
    (foreach cont contacts (MarkContact CV cont))

    ; draw paths
    (foreach path paths
             layers = (PickBundledLayers path)
             points = (GetPathPoints path)
             (cond (layers!=nil
                    flipX = (GetProp path "FlipX" nil)
                    flipY = (GetProp path "FlipY" nil)
                    (DrawGuidePath layers pattern prefix points flipX flipY)
                    )
                   )
             )

    ; draw contacts
    (foreach cont contacts (DrawGuideContact cont pattern prefix))

    ; delete source wires
    (foreach path    paths    (dbDeleteObject path))
    (foreach contact contacts (dbDeleteObject contact))
    )
  t
  )

; draw a guide path with bus properties
(defun DrawGuidePath (layer pattern prefix points flipX flipY
                            @key (CV (geGetEditCellView)))
  (let (path)
    obj = (dbCreatePath CV (list layer "bus") points 0.12 "squareFlush")
    (dbReplaceProp obj "BusPattern" "string" pattern)
    (dbReplaceProp obj "BusPrefix" "string" prefix)
    (dbReplaceProp obj "BusPin" "boolean" nil)
    (dbReplaceProp obj "BusFlipX" "boolean" flipX)
    (dbReplaceProp obj "BusFlipY" "boolean" flipY)
    (dbReplaceProp obj "BusStartVias" "boolean" nil)
    (dbReplaceProp obj "BusEndVias" "boolean" nil)
    (dbReplaceProp obj "BusArrayPitch" "float" 2.88)
    (dbReplaceProp obj "BusDoubleVias" "boolean" (RecommendDoubleVias pattern))
    )
  t
  )

; draw an optimized guide path for a contact
(defun DrawGuideContact (cont pattern chan)
  (let (layers xy x y points flipX flipY metalW metalE metalN metalS dx dy)
    layers = (PickBundledLayers cont)
    (cond (layers!=nil
           xy = cont->xy
           x = (car xy)
           y = (cadr xy)
           metalW = (GetProp cont "MetalW" nil)
           metalE = (GetProp cont "MetalE" nil)
           metalN = (GetProp cont "MetalN" nil)
           metalS = (GetProp cont "MetalS" nil)
           flipX  = (GetProp cont "FlipX"  nil)
           flipY  = (GetProp cont "FlipY"  nil)
           (cond ((and metalW!=metalE metalN!=metalS) ; turn via
                  dx = (if metalW -1.14 1.14)
                  dy = (if metalS -1.14 1.14)
                  points = (list x+dx:y x:y x:y+dy)
                  (DrawGuidePath layers pattern chan points flipX flipY)
                  )
                 (t ; stacked via or > 2 spoke
                  points = (list x-1.14:y x:y x:y-1.14)
                  (DrawGuidePath layers pattern chan points flipX flipY)
                  points = (list x+1.14:y x:y x:y+1.14)
                  (DrawGuidePath layers pattern chan points flipX flipY)
                  )
                 )
           )
          )
    )
  t
  )

; pick a suitable bus layer for a particular path or contact
(defun PickBundledLayers (obj)
  (cond (obj->objType=="path" obj->layerName)
        (obj->objType=="inst"
          (cond ((setof lpp obj->master->lpps lpp->layerName=="VIA2")!=nil "M3")
                ((setof lpp obj->master->lpps lpp->layerName=="VIA3")!=nil "M4")
                ((setof lpp obj->master->lpps lpp->layerName=="VIA4")!=nil "M5")
                ((setof lpp obj->master->lpps lpp->layerName=="VIA5")!=nil "M6")
                ((setof lpp obj->master->lpps lpp->layerName=="VIA6")!=nil "M7")
                (t nil)
                )
          )
        )
  )

; copy flip properties from one object to another
(defun CopyFlipProperties (from to)
  (let (flipX flipY)
    flipX = (dbGetPropByName from "FlipX")
    flipY = (dbGetPropByName from "FlipY")
    (if flipX (dbReplaceProp to "FlipX" "boolean" flipX->value))
    (if flipY (dbReplaceProp to "FlipY" "boolean" flipY->value))
    )
  t
  )

; mark if a path needs to be flipped
(defun MarkPath (CV path)
  (let (overlaps flipX flipY pinType)

    ; find overlapping fat pins
    overlaps = (FindMetalOverlaps CV path->bBox path->layerName)

    ; copy their FlipX/FlipY properties
    (foreach overlap overlaps
             (cond ((atom overlap)
                    (CopyFlipProperties overlap path)
                    )
                   )
             )
    )
  t
  )

; mark a contact if it needs to be flipped, and which way metal overlaps connect
(defun MarkContact (CV cont)
  (let (overlaps x0 y0 x1 y1 ox0 oy0 ox1 oy1 mW mE mS mN)
    (foreach lp cont->master->lpps
             overlaps = (FindMetalOverlaps CV cont->bBox lp->layerName)
             x0 = (car (car cont->bBox))
             y0 = (cadr (car cont->bBox))
             x1 = (car (cadr cont->bBox))
             y1 = (cadr (cadr cont->bBox))
             (foreach path overlaps
                      (cond ((atom path)
                             ox0 = (car (car path->bBox))
                             oy0 = (cadr (car path->bBox))
                             ox1 = (car (cadr path->bBox))
                             oy1 = (cadr (cadr path->bBox))
                             (CopyFlipProperties path cont)
                             mW = ox0<x0-1.14
                             mE = ox1>x1+1.14
                             mS = oy0<y0-1.14
                             mN = oy1>y1+1.14
                             (cond (mW
                                    (dbReplaceProp cont "MetalW" "boolean" t)
                                    (TrimPath path cont->xy -1.14:0)
                                    ))
                             (cond (mE
                                    (dbReplaceProp cont "MetalE" "boolean" t)
                                    (TrimPath path cont->xy 1.14:0)
                                    ))
                             (cond (mS
                                    (dbReplaceProp cont "MetalS" "boolean" t)
                                    (TrimPath path cont->xy 0:-1.14)  
                                    ))
                             (cond (mN
                                    (dbReplaceProp cont "MetalN" "boolean" t)
                                    (TrimPath path cont->xy 0:1.14)
                                    ))
                             )
                            )
                      )
             )
    )
  t
  )

; find overlaps on "drawing" "pin" or "net" purposes
(defun FindMetalOverlaps (CV bbox layerName)
  (let (overlaps)
    overlaps = (dbGetOverlaps CV bbox (list layerName "drawing"))
    overlaps = (append overlaps (dbGetOverlaps CV bbox (list layerName "net")))
    overlaps = (append overlaps (dbGetOverlaps CV bbox (list layerName "pin")))
    )
  )

; find overlaps of contacts
(defun FindContactOverlaps (CV bbox layerName)
  (let (overlaps contacts)
    overlaps = (dbGetOverlaps CV bbox (list layerName "drawing") 1)
    (foreach overlap overlaps
             (when (and (atom overlap)==nil (car overlap)->objType=="inst")
               contacts = (cons (car overlap) contacts)
               )
             )
    contacts
    )
  )
  
; detect direction of a metal layer
(defun IsLayerHorizontal (layerName)
  (or layerName=="M3" layerName=="M5" layerName=="M7")
  )

; simplify degenerate paths, assuming only first and last points are valid
(defun GetPathPoints (path)
  (let (points)
    points = (list (car path->points) (nth (length path->points)-1 path->points))
    (unless (or (car (car points))==(car (cadr points))
                (cadr (car points))==(cadr (cadr points)))
      (printf "WARNING: Jogged bundled path on net %s\n" (if path->net path->net->name ""))
      points = (list (car path->points) (cadr path->points))
      )
    points
    )
  )

; trim the ends of a path if it connects to a contact
(defun TrimPath (path xy dxy)
  (let (newpoints)
    (cond (path->objType=="path"
           newpoints = nil
           (foreach point path->points
                    (cond ((and (car point)==(car xy) (cadr point)==(cadr xy))
                           point = (car point)+(car dxy):(cadr point)+(cadr dxy)
                           )
                          )
                    newpoints = (append newpoints (list point))
                    )
           path->points = newpoints
           )
          )
    )
  t
  )

; expand [i..j] in a list of BusPrefix names
(defun ExpandArrayRanges (names)
  (let (expanded a b c left lo hi right prefix n)
    expanded=nil
    (rexCompile "^\\(.*\\)\\[\\([0-9]+\\)\\.\\.\\([0-9]+\\)\\]\\(.*\\)$")
    (foreach name names
             (cond ((rexExecute name)
                    left = (rexSubstitute "\\1")
                    lo = (atoi (rexSubstitute "\\2"))
                    hi = (atoi (rexSubstitute "\\3"))
                    right = (rexSubstitute "\\4")
                    (foreach prefix (ExpandArrayRanges (list left))
                             (foreach suffix (ExpandArrayRanges (list right))
                                      (for i lo hi
                                           n = (sprintf nil "%s[%d]%s" prefix i suffix)
                                           expanded = (append expanded (list n))
                                           )
                                      )
                             )
                    )
                   (t (cdr a)==nil expanded = (append expanded (list name)))
                   )
             )
    expanded
    )
  )



(defun FindMBUFChannels (LegalMBUFs @key (CV (geGetEditCellView)))
 (let (nets iterms filename file goodnets)
  nets=CV->nets
  (foreach net nets
    iterms=net->instTerms
    (when (length iterms)==2
      (when (IsInList (car iterms)->inst->master->cellName LegalMBUFs) &&
            (IsInList (cadr iterms)->inst->master->cellName LegalMBUFs)
        goodnets=(cons net goodnets)
      )
    )
  )
 goodnets
 )
)

(defun ReportMBUFChannels (filename @key (filter_buswires nil))
 (let (file nets newname bufs)
  file=(outfile filename)

  (fprintf file "e1of4s = (list \"e1of4\" ")
  bufs=(list "lib.buffer.half.RBUF_1of4.1000"
             "lib.buffer.half.MBUF_1of4.1000"
             "lib.buffer.half.SBUF_1of4.1000"
       )
  nets=(FindMBUFChannels bufs)
  (foreach net nets
    (when !filter_buswires || (filter_buswires && !(DoesBusWireExist net))
      newname=(reverse (parseString net->name "."))
      (when (car newname)=="e"
        (fprintf file "\"%s\" " (buildString (reverse (cdr newname)) "."))
      )
    )
  )
  (fprintf file ")\n\n")

  (fprintf file "e1of3s = (list \"e1of3\" ")
  bufs=(list "lib.buffer.half.RBUF_1of3.1000"
             "lib.buffer.half.MBUF_1of3.1000"
             "lib.buffer.half.SBUF_1of3.1000"
       )
  nets=(FindMBUFChannels bufs)
  (foreach net nets
    (when !filter_buswires || (filter_buswires && !(DoesBusWireExist net))
      newname=(reverse (parseString net->name "."))
      (when (car newname)=="e"
        (fprintf file "\"%s\" " (buildString (reverse (cdr newname)) "."))
      )
    )
  )
  (fprintf file ")\n\n")

  (fprintf file "e1of2s = (list \"e1of2\" ")
  bufs=(list "lib.buffer.half.RBUF_1of2.1000"
             "lib.buffer.half.MBUF_1of2.1000"
             "lib.buffer.half.SBUF_1of2.1000"
       )
  nets=(FindMBUFChannels bufs)
  (foreach net nets
    (when !filter_buswires || (filter_buswires && !(DoesBusWireExist net))
      newname=(reverse (parseString net->name "."))
      (when (car newname)=="e"
        (fprintf file "\"%s\" " (buildString (reverse (cdr newname)) "."))
      )
    )
  )
  (fprintf file ")\n\n")


  (close file)
 )
)
