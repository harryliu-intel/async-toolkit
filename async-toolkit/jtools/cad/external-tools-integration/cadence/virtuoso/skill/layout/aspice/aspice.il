; Select an instance by its name
(defun SelectInstByName (name)
  (let (CV inst)
    CV = (geGetEditCellView)
    inst = (dbFindAnyInstByName CV name)
    (cond (inst (geSelectObject inst)))
    inst
    )
  )

; Selects a bunch of instaces given names in file
(defun SelectInstancesFromFile (filename @key (CV (geGetEditCellView)) (CAST nil))
  (let (TEMP newfilename file line inst n)

    ; rename instances
    (when CAST
      TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP" )
      newfilename = (sprintf nil "%s/%s.insts" TEMP CV->cellName)
      (shell (sprintf nil "rename --from=cast --to=cadence --type=instance < %s > %s"
                      filename newfilename))
      filename = newfilename
      )

    ; select instances
    (printf "Reading %s\n" filename)
    n=0
    file = (infile filename)
    (while (and file (gets line file))
      line = (substring line 1 strlen(line)-1)
      (unless (SelectInstByName line)
        (printf "Missing instance %s\n" line)
        n=n+1
        )
      )
    (close file)
    (when n (printf "%d missing instances\n" n))
    t
    )
  )

; Prints an aspice.asp file to simulate only selected cells and nets they touch
(defun AspiceSelected (fileName @key (CV (geGetEditCellView)) (tau 50) (power nil) (toGDS2 nil))
  (let (maplist typeMap instMap nodeMap instGds2Map nodeGds2Map
                selected insts nodes n node instances
                rename aspFile nodeName typeName instName)

    ; translate names
    (when toGDS2
        maplist = (TranslateNames "gds2" ?CV CV)
        instGds2Map = (cadr maplist)
        nodeGds2Map = (caddr maplist)
        )
    maplist = (TranslateNames "cast" ?CV CV)
    typeMap = (car maplist)
    instMap = (cadr maplist)
    nodeMap = (caddr maplist)

    ; start asp file
    aspFile = (outfile fileName)
    (fprintf aspFile ".scale_after_delay = 0.5; /* 100 delay equals %dps */\n" tau)
    
    ; find critical nets
    selected = (setof x (geGetSelSet) x->type=="trueInst" && x->libName!=TechLibName)
    insts = (makeTable "instances" nil)
    nodes = (makeTable "nodes" nil)
    (unless power
      nodes[(dbFindNetByName CV GNDNetName)] = t
      nodes[(dbFindNetByName CV VddNetName)] = t
      )
    n=0
    (foreach inst selected
      insts[inst] = t
      (foreach term inst->instTerms
        node = term->net
        (unless nodes[node]
          nodes[node]=t
          nodeName = (if toGDS2 && node->pins==nil
                         nodeGds2Map[node->name]
                         nodeMap[node->name])
          (fprintf aspFile ".critical \"%s\";\n" nodeName)
          n++
          )
        )
      )
    (printf "found %d critical nets...\n" n)

    ; find skipped instances
    n=0
    instances = (setof x CV->instances x->type=="trueInst" && x->libName!=TechLibName) 
    (foreach inst instances
      (unless (or insts[inst] (strncmp inst->cellName "synthesis.shared." 17)==0)
        typeName = typeMap[inst->cellName]
        instName = (if toGDS2 instGds2Map[inst->name] instMap[inst->name])
        (fprintf aspFile ".modify skip \"%s\"   \"%s\";\n" typeName instName)
        (fprintf aspFile ".modify env  \"%s.*\" \"%s\";\n" typeName instName)
        n++
        )
      )
    (printf "found %d non-critical instances\n" n)
    (close aspFile)
    )
  t
  )

; draw M7 power stripes so --extractPower=1 is useful
(defun DrawPowerStripes (@key (CV (geGetEditCellView)))
  (let (x0 y0 x1 y1)
    x0 = (car (car CV->bBox))
    y0 = (cadr (car CV->bBox))
    x1 = (car (cadr CV->bBox))
    y1 = (cadr (cadr CV->bBox))
    x0 = PowerGridPitch*(floor x0/PowerGridPitch)
    x1 = PowerGridPitch*(ceiling x1/PowerGridPitch)
    y0 = (floor y0/PowerGridPitch)
    y1 = (ceiling y1/PowerGridPitch)
    (for y y0 y1
         (DrawChannel BusMetal67 node_6W (if (mod y 2)==0 "GND" "Vdd")
                      (list x0:PowerGridPitch*y x1:PowerGridPitch*y) ?isPin t)
         )
    )
  t
  )

; translate all type, inst, node names in a CellView, return 3 maps
(defun TranslateNames (to @key (CV (geGetEditCellView)))
  (let (typeMap instMap nodeMap
                nodeFileName typeFileName instFileName typeFile instFile nodeFile
                TEMP rename)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP" )
    nodeFileName = (sprintf nil "%s/%s.nodes" TEMP CV->cellName)
    typeFileName = (sprintf nil "%s/%s.types" TEMP CV->cellName)
    instFileName = (sprintf nil "%s/%s.insts" TEMP CV->cellName)
    typeFile = (outfile typeFileName)
    instFile = (outfile instFileName)
    nodeFile = (outfile nodeFileName)

    ; find all types, instances, nodes in this cell
    typeMap = (makeTable "types" nil)
    instMap = (makeTable "instances" nil)
    nodeMap = (makeTable "nodes" nil)
    (foreach inst (setof x CV->instances x->type=="trueInst" && x->libName!=TechLibName) 
             typeMap[inst->cellName]=t
             instMap[inst->name]=t
             )
    (foreach net CV->nets
             nodeMap[net->name]=t
             )
    (foreach name (setof a typeMap t)
             (fprintf typeFile "%s\n" name)
             )
    (foreach name (setof a instMap t)
             (fprintf instFile "%s\n" name)
             )
    (foreach name (setof a nodeMap t)
             (fprintf nodeFile "%s\n" name)
             )
    (close typeFile)
    (close instFile)
    (close nodeFile)
    
    ; rename nodes, types, instances
    rename = (sprintf nil "rename --from=cadence --to=%s" to)
    (shell (sprintf nil "%s --type=cell < %s > %s.renamed"
                    rename typeFileName typeFileName))
    (shell (sprintf nil "%s --type=instance < %s > %s.renamed"
                    rename instFileName instFileName))
    (shell (sprintf nil "%s --type=node < %s > %s.renamed"
                    rename nodeFileName nodeFileName))

    ; read back mapped names
    typeFile = (infile (sprintf nil "%s.renamed" typeFileName))
    instFile = (infile (sprintf nil "%s.renamed" instFileName))
    nodeFile = (infile (sprintf nil "%s.renamed" nodeFileName))
    (foreach name (setof a typeMap t)
             (gets line typeFile)
             line = (StringUtilChompNewline line)
             typeMap[name] = line
             )
    (foreach name (setof a instMap t)
             (gets line instFile)
             line = (StringUtilChompNewline line)
             instMap[name] = line
             )
    (foreach name (setof a nodeMap t)
             (gets line nodeFile)
             line = (StringUtilChompNewline line)
             nodeMap[name] = line
             )
    (close typeFile)
    (close instFile)
    (close nodeFile)
    (list typeMap instMap nodeMap)
    )
  )

; find subcells that connect to a selected wire
(defun WriteConnectedSubcells (netname filename @key (CV (geGetEditCellView)))
  (let (net file found n)
    net = (dbFindNetByName CV netname)
    file = (outfile filename)
    found = (makeTable "instances" nil)
    n=0
    (foreach term net->allInstTerms
             (when term->inst->libName!=TechLibName && !found[term->inst]
                   found[term->inst]=t
                   (fprintf file "%s\n" term->inst->name)
                   n++
                   )
             )
    (close file)
    n
    )
  )

; move shapes and subcell pins to a different net
(defun RenameSelectedNet (to @key (CV (geGetEditCellView)))
  (let (net connected n)
    net = (MakeNet CV to)
    (SelectConnectedWiring)
    n=0

    ; rename wires and contacts
    (foreach obj (geGetSelSet)
             (cond (obj->objType=="path" ||
                    obj->objType=="rect" ||
                    obj->objType=="polygon"
                    (when obj->net!=net
                      obj->net=nil
                      obj->net=net
                      n++)
                    )
                   (obj->libName==TechLibName
                    term = (car obj->instTerms)
                    term->net=nil
                    term->net=net
                    n++)
                   )
             )

    ; rename subcell pins
    connected = (ConnectedSubcellPins CV (geGetSelSet))
    (foreach a connected
             inst = (car a)
             pin = (cadr a)
             (foreach term (setof x inst->instTerms x->name==pin->net->name)
                      term->net=nil
                      term->net=net
                      n++
                      )
             )
    n
    )
  )

; Translate instance names to GDS2
(defun TranslateInstanceNames (@key (CV (geGetEditCellView)))
  (let (x instGds2Map)
    x = (TranslateNames "gds2" ?CV CV)
    instGds2Map = (cadr x)
    (foreach inst (setof a CV->instances
                         (and a->libName!=TechLibName (length a->instTerms)>2))
             inst->name = instGds2Map[inst->name]
             )
    )
  t
  )

; Write CAST file from DFII.
(defun WriteCastSubcells (filename @key (CV (geGetEditCellView)))
  (let (file x typeCastMap instCastMap nodeCastMap
             pinCastMap pinCastMaps portMap castName terms
             nodeName typeName portName portNameNew start)
    (rexCompile "\\(\\]\\[\\)") ; use to translate subcell port names

    x = (TranslateNames "cast" ?CV CV)
    typeCastMap = (car x)
    instCastMap = (cadr x)
    nodeCastMap = (caddr x)

    x = (TranslateNames "gds2" ?CV CV)
    typeGds2Map = (car x)
    instGds2Map = (cadr x)
    nodeGds2Map = (caddr x)

    file = (outfile filename)
    (fprintf file "  subcells {\n")

    ; subcells
    pinCastMaps = (makeTable "pinCastMaps" nil)
    (foreach inst (setof a CV->instances
                         (and a->libName!=TechLibName (length a->instTerms)>2))
             typeName = typeCastMap[inst->cellName]
             (fprintf file "    %s %s()();\n" typeName inst->name)
             pinCastMap = pinCastMaps[inst->master]
             (unless pinCastMap
               pinCastMap = (GetCastPinNameMap typeName)
               pinCastMaps[inst->master]=pinCastMap
               )
             )

    ; nodes
    (foreach net CV->nets
             terms = (list)

             ; find pins
             (when net->pins
               castName = nodeCastMap[net->name]
               terms = (cons castName terms)
               )

             ; find subcell terminals
             (foreach term net->instTerms
                      inst = term->inst
                      pinCastMap = pinCastMaps[inst->master]
                      (when pinCastMap
                        portName = (rexReplace term->name "," -1) ; TODO: better
                        portNameNew = pinCastMap[portName]
                        (when portNameNew portName = portNameNew)
                        terms = (cons (sprintf nil "%s.%s" inst->name portName) terms)
                        )
                      )

             ; print connections
             start = t
             (when (length terms)>1
                   (foreach name terms
                            (fprintf file (if start "    " "\n    ="))
                            start=nil
                            (fprintf file "%s" name)
                            )
                   (fprintf file ";\n")
                   )
             )
    (fprintf file "  }\n")
    (close file)
    )
  t
  )

; utility to figure out the proper CAST name for pins
(defun GetCastPinNameMap (typeName)
  (let (TEMP CAST_PATH cmd file line fields pinMap)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    CAST_PATH = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    cmd = (sprintf nil "cast_query --cast-path=\"%s\" --max-heap-size=%dM --cell=\"%s\" --task=external_nodes=xr:im > \"%s/%s.pins\""
                   CAST_PATH (nrGetMaxHeapSize) typeName TEMP typeName)
    (shell cmd)
    file = (infile (sprintf nil "%s/%s.pins" TEMP typeName))
    pinMap = (makeTable "pins" nil)
    (gets line file)
    (while (gets line file)
      line = (StringUtilChompNewline line)
      fields =(parseString line " ")
      pinMap[(cadr fields)]=(car fields)
      )
    (close file)
    pinMap
    )
  )

; clean TEMP directory
(defun CleanTempDir (@key (temp (ConfigFileGetValue TheCDSConfigTable "TEMP")))
  (shell (sprintf nil "rm -rf %s/*" temp))
  t
  )

; Canonicalize pin names, always preferring port names
(defun NewCanonicalizePins ()
  (setShellEnvVar "NEW_CANONICALNESS_ALGORITHM=true")
  (CleanTempDir) ; avoid incompatible cached files
  (CanonicalizePins)
  (CleanTempDir) ; avoid incompatible cached files
  (setShellEnvVar "NEW_CANONICALNESS_ALGORITHM=false")
  t
  )

; delete connectivity information from shapes, contacts, and optionally subcells
(defun DeleteConnectivity (@key (CV (geGetEditCellView)) (subcells nil))
  (foreach shape CV->shapes shape->net=nil)
  (foreach inst CV->instances
           (when (or subcells inst->libName==TechLibName)
             (foreach term inst->instTerms term->net=nil)
             )
           )
  t
  )

; Unconnect Vdd and GND pins for now
(defun DisconnectPowerPins (@key (CV (geGetEditCellView)))
  (let (Vdd GND)
    Vdd = (dbFindNetByName CV "Vdd")
    GND = (dbFindNetByName CV "GND")
    (dbDeleteObject Vdd)
    (dbDeleteObject GND)
    t
    )
  )

; save a list of inst terms
(defun GetInstanceTerms (netname @key (CV (geGetEditCellView)))
  (let (net l)
    l = (list)
    net = (dbFindNetByName CV netname)
    (foreach term net->allInstTerms
             (when term->inst->libName!=TechLibName
               l = (cons (list term->inst->name term->name) l)
               )
             )
    l
    )
  )
  
; reconnect given a list of inst terms
(defun ConnectInstanceTerms (netname termList @key (CV (geGetEditCellView)))
  (let (net inst term n)
    net = (dbFindNetByName CV netname)
    n=0
    (foreach pair termList
             inst = (dbFindAnyInstByName CV (car pair))
             term = (car (setof x inst->instTerms x->name==(cadr pair)))
             term->net=nil
             term->net=net
             n++
             )
    n
    )
  )
