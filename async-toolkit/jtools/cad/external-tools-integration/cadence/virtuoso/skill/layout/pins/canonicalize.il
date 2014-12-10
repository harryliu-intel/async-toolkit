
(defun CanonicalizeLoadTable (castCell @key (tempdir nil) )
 (let (mapfile table Command)
  (cond (tempdir==nil tempdir = ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ) )
  mapfile = strcat( tempdir "/" castCell ".canon.il" )
  (when !(isFile mapfile) && tempdir!=nil && (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")!=nil
      Command=sprintf(  nil
              "cast2skill --cadence-name --cast-path=%s --cell=%s --output-dir=%s --cadence-name --root-only  --max-heap-size=%dM --64"
              ( ConfigFileGetValue TheCDSConfigTable "CAST_PATH" )
              castCell
              tempdir
              ( nrGetMaxHeapSize )
              )
    (shell Command)
  )
  (if (isFile mapfile) then
    (load mapfile)
    table = (PinCanonicalTable)
  else
    table = nil
    (printf "Cast didn't parse.")
  )
  table
 ) 
)

(defun CanonicalizePins ( @key (cv (geGetEditCellView))
                               (tempdir (ConfigFileGetValue TheCDSConfigTable "TEMP"))
                               (castCell nil))
 (let (table pinname canon allpins pincount renamecount )
  (cond (castCell==nil castCell=cv->cellName))
  table = (CanonicalizeLoadTable castCell ?tempdir tempdir )
  pincount=0
  renamecount=0

  allpins = (PinUtilGetExistingPinFigs cv)
  (when table
    (foreach pin allpins
     pinname = pin->net->name
     (when pinname 
        pincount=pincount+1
        canon = (arrayref table pinname)
        (when canon!=nil && canon!=pinname
          (PinUtilRenamePin cv pin canon)
          (dbReplaceProp pin "CanonicalizedFrom" "string" pinname)
          renamecount=renamecount+1
        )
      )
    )
  )
  (DeleteUnusedNets)
  (printf "Renamed %d of %d pins.\n" renamecount pincount)
 ) 
)

defun( CreatePinShapes ( CellView )
  let((pinShapes label shape net n u Labels)
    Labels=setof( shape CellView~>shapes shape~>objType=="label" )
    n=0
    u=0
    foreach( label Labels
      n=n+1
      if( label~>parent==nil then
        pinShapes=dbGetTrueOverlaps(
          CellView 
          list( label~>xy label~>xy )
          label~>lpp
          1 
        )
        foreach( pinShape pinShapes
          if( pinShape~>objType!="label" && label~>parent == nil then
            u=u+1
            net=dbMakeNet( CellView label~>theLabel )
            dbCreatePin( net pinShape ) 
            label~>parent=pinShape
          )
        )
      )
    )
    printf( "%d/%d pins created from labels\n" u n)
  )
)

; Warn/Delete redundant non-canonical pins (after CanonicalizePins)
(defun DeleteRedundantPins
  (@key (CV (geGetEditCellView))
        (warnOnly nil)
        )
  (let (to_delete (num 0))
    (foreach net CV->nets
             (unless (or (length net->pins)<=1 net->name=="Vdd" net->name=="GND")
               ; find canonicalized pins
               to_delete = nil
               (foreach pin net->pins
                        (when (dbGetPropByName pin->fig "CanonicalizedFrom")
                          to_delete = (cons pin->fig to_delete)
                          )
                        )
               ; keep last pin if all were canonicalized
               (when (length to_delete)==(length net->pins)
                     to_delete = (cdr to_delete)
                     )
               ; delete pins and ROD objects
               (when to_delete
                 num=num+1
                 (printf "WARNING: %s%d redundant pins on net %s\n"
                         (if warnOnly "" "Deleting ") (length to_delete) net->name)
                 (foreach obj to_delete
                          (cond (warnOnly
                                 (geCreateMarker obj "warning" "pins" "redundant pin" "")
                                 )
                                (t
                                 (foreach child obj->children (dbDeleteObject child))
                                 (dbDeleteObject obj)
                                 )
                                )
                          )
                 )
               )
             )
    num
    )
  )
