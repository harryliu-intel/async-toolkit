

(defun SetXAlignmentProp ( cv val )
  (dbReplaceProp cv "XAlignGrid" "float" val)
)

(defun GetXAlignmentProp ( cv )
 (let (align)
  align = (dbGetPropByName cv "XAlignGrid")->value
 )
)

(defun SetYAlignmentProp ( cv val )
  (dbReplaceProp cv "YAlignGrid" "float" val)
)

(defun GetYAlignmentProp ( cv )
 (let (align)
  align = (dbGetPropByName cv "YAlignGrid")->value
 )
)


(defun SetAlignmentUI () 
 (let (cv valx valy xalign maxalign yalign use_subcell str
       layout prelayout floorplan layout_exists prelayout_exists floorplan_exists noedit edit)
  cv = (geGetWindowCellView)
  valx = UISetAlignment_Form->UISetAlignment_Form_XAlign->value
  valy = UISetAlignment_Form->UISetAlignment_Form_YAlign->value
  xalign = valx
  yalign = valy
  ; I'm intentionally not doing anything smart with yalign because we shouldn't ever
  ; be doing funky alignments in that axis

  (if cv->viewName=="layout_pg" then
    hiDisplayAppDBox(
        ?name 'SetAlignmentErrorDialogBox
        ?dboxText "Don't do this in a layout_pg view."
        ?dboxBanner "Wrong view"
        ?dialogType     hicQuestionDialog
        ?dialogStyle    'modal
        ?buttonLayout   'Close
    )
  else
  

  ; check for additonal views to be updated
  floorplan_exists = (dbOpenCellViewByType cv->libName cv->cellName "floorplan" nil "r")
  prelayout_exists = (dbOpenCellViewByType  cv->libName cv->cellName "prelayout" nil "r")
  layout_exists = (dbOpenCellViewByType  cv->libName cv->cellName "layout" nil "r")

  noedit = nil
  (when floorplan_exists
    floorplan = (nrOpenCellViewWritable cv->libName cv->cellName "floorplan")
    (when !floorplan
      noedit = t
      (printf "Couldn't open floorplan view for edit.\n")
      floorplan = (nrOpenCellViewReadable cv->libName cv->cellName "floorplan")
    )
  )
  (when prelayout_exists
    prelayout = (nrOpenCellViewWritable cv->libName cv->cellName "prelayout")
    (when !prelayout
      noedit = t
      (printf "Couldn't open prelayout view for edit.\n")
      prelayout = (nrOpenCellViewReadable cv->libName cv->cellName "prelayout")
    )
  )
  (when layout_exists
    layout = (nrOpenCellViewWritable cv->libName cv->cellName "layout")
    (when !layout
      noedit = t
      (printf "Couldn't open layout view for edit.\n")
      layout = (nrOpenCellViewReadable cv->libName cv->cellName "layout")
    )
  )
  (when noedit
    edit = (hiDisplayAppDBox
                      ?name 'AlignEditDbox
                      ?dboxBanner "Warning"
                      ?dboxText "One or more views could not be edited. P4 Edit and modify them?"
                      ?dialogType hicQuestionDialog
                      ?buttonLayout 'YesNo
                      ?defaultButton 1
                     )
  )
  (when edit
    (when floorplan_exists 
      floorplan = (CDSP4CellView floorplan "edit"))
    (when prelayout_exists 
      prelayout = (CDSP4CellView prelayout "edit"))
    (when layout_exists 
      layout = (CDSP4CellView layout "edit"))
  )

  ;check for larger grid in subcells
  (SetXAlignmentProp cv 0)
  maxalign = (GetMaxSubcellXAlignment cv)
  (when valx<maxalign
    str = (sprintf nil "A subcell has x-alignment of %2.2f. Use its alignment instead?" maxalign)
    use_subcell = (hiDisplayAppDBox
                      ?name 'AlignOverrideDbox
                      ?dboxBanner "Warning"
                      ?dboxText str
                      ?dialogType hicQuestionDialog
                      ?buttonLayout 'YesNo
                      ?defaultButton 1
                     )
    (when use_subcell xalign = maxalign)
  )

  ;update properties
  (when (nrIsCellViewWritable floorplan) 
    (SetXAlignmentProp floorplan xalign)
    (SetYAlignmentProp floorplan yalign)
    (dbSave floorplan)
  )
  (when (nrIsCellViewWritable prelayout)
    (SetXAlignmentProp prelayout xalign)
    (SetYAlignmentProp prelayout yalign)
    (dbSave prelayout)
  )
  (when (nrIsCellViewWritable layout)
    (SetXAlignmentProp layout xalign)
    (SetYAlignmentProp layout yalign)
    (dbSave layout)
  )
  )
 )
)

(defun GetMaxSubcellXAlignment (cv)
 (let (align uniqueInstances uniqueNames cellv)
   (if (GetXAlignmentProp cv) then
     align = (GetXAlignmentProp cv)
   else
     align = MfgGrid
   )

   uniqueInstances = nil
   uniqueNames = nil
   (foreach inst cv->instances
     (when !(IsInList inst->cellName uniqueNames) && inst->libName!=TechLibName
       uniqueNames = (cons inst->cellName uniqueNames )
       uniqueInstances = (cons inst uniqueInstances )
     )
   )
   (foreach inst uniqueInstances
     cellv = (nrOpenCellViewReadable inst->libName inst->cellName inst->viewName)
     (when cellv 
       align = (max align (GetMaxSubcellXAlignment cellv))
     )
   )
   align
 )
)


(defun SetCellAlignment (cv @key (xalign MfgGrid) (yalign GridPitch) )
 (let (maxalign current_xalign)
   (if (nrIsCellViewWritable cv) then
     (SetYAlignmentProp cv yalign)
     current_xalign = (GetXAlignmentProp cv)
     (when !current_xalign
       current_xalign = MfgGrid
     )
     maxalign = (max xalign (GetMaxSubcellXAlignment cv) current_xalign)
     (SetXAlignmentProp cv maxalign)
     (dbSave cv)
   else
     (printf "Couldn't open cell %s for edit.\n" cv->cellName)
   )
 )
)


(defun CheckRoutedAlignment (cv @key (dialog nil))
 (let (uniqueInstances uniqueNames cellv routed (location nil) (locations nil)
          (misalignment nil) (inst_misaligned nil) (sub_misaligned nil) instances)
   uniqueInstances = nil
   uniqueNames = nil
   instances = (setof inst cv->instances inst->libName!=TechLibName) ; exclude vias
   (foreach inst instances
     inst_misaligned = !(IsInstAligned inst)
     misalignment = misalignment || inst_misaligned
     (when inst_misaligned (printf "       in cell %s\n" cv->cellName))  
     (when inst->libName!=TechLibName && inst->orient
       (when !(IsInList inst->orient LegalRotations)
         misalignment = t
         (printf "Instance %s in cell %s has illegal orientation.\n" inst->name cv->cellName)
       )
     )

     location = (list inst->xy inst->cellName inst->orient)
       (when (IsInList location locations)
         misalignment = t
         (printf "Duplicate instance placement of cell %s \n             in cell %s at (%.3f %.3f)\n" 
                                  inst->cellName inst->cellView->cellName (car inst->xy) (cadr inst->xy))
       )
     locations = (cons location locations)

     (when !(IsInList inst->cellName uniqueNames) && inst->libName!=TechLibName && !(IsWiringCell inst)
       uniqueNames = (cons inst->cellName uniqueNames )
       uniqueInstances = (cons inst uniqueInstances )
     )
   )
   (foreach inst uniqueInstances
     routed = (nrIsRoutedCell inst->libName inst->cellName inst->viewName)
     (when !routed && !(IsGlobalsCell inst)
       cellv = (nrOpenCellViewReadable inst->libName inst->cellName inst->viewName)
       sub_misaligned = (CheckRoutedAlignment cellv)
     )
     misalignment = misalignment || sub_misaligned
   )

   (when dialog
     (if misalignment then
       str = (sprintf nil "One or more cells are misaligned. Check CIW.")
     else
       str = (sprintf nil "All cells are correctly aligned.")
     )
     (hiDisplayAppDBox
        ?name 'checkAlignmentDialogBox
        ?dboxText str
        ?dboxBanner "Alignment Status"
        ?dialogType     hicQuestionDialog
        ?dialogStyle    'modal
        ?buttonLayout   'Close
     )
   )
   misalignment
 )
)

(defun fixXAlignment ( @key (cv nil)) 
 (let (xalign instcv inst misaligned inst_moved)
  (when !cv
    cv = (geGetWindowCellView)
  )
  move=0.0
  inst_moved=0
  instances=sort( cv~>instances 
    lambda((cell1 cell2) leftEdge(cell1~>bBox)<leftEdge(cell2~>bBox)))
  foreach( inst instances
    new_move=AlignInstX( cv inst move )
    if( new_move>0.0 then inst_moved=inst_moved+1 )
    if( new_move > move then
      printf("Instance \"%s\" (%s) is moved by %.3f to snap to X alignment.\n" inst~>name inst~>cellName new_move)
      move=new_move
    )
  )
  if( move == 0.0 then
    printf( "No instance moved. \n" )
  else
    printf( "\nTotal %d instances moved.\n" inst_moved )
  )
 )
)


(defun CheckFlatAlignment ( @key (cv nil) (dialog nil))
 (let (xalign instcv instx misaligned)
  (when !cv
    cv = (geGetWindowCellView)
  )

  misaligned = nil
  (foreach inst cv->instances
     (when !(IsInstAligned inst)
       misaligned = (cons inst misaligned)
     )
  )
  (if misaligned then
    str = (sprintf nil "One or more cells are misaligned. Check CIW.")
  else
    str = (sprintf nil "All cells are correctly aligned.")
  )
  (if dialog then
    (hiDisplayAppDBox
        ?name 'checkAlignmentDialogBox
        ?dboxText str
        ?dboxBanner "Alignment Status"
        ?dialogType     hicQuestionDialog
        ?dialogStyle    'modal
        ?buttonLayout   'Close
    )
  else
    (printf "%s\n" str)
  )
 )
)

(defun IsInstAligned (inst)
 (let (instcv xalign yalign instx insty aligned)
   aligned = t
   (unless inst->libName==TechLibName
     instcv = inst->master
     xalign = (GetXAlignmentProp instcv)
     (unless xalign xalign = GridPolyPitch)
     yalign = (GetYAlignmentProp instcv)
     (unless yalign yalign = GridPitch)
     instx = (car inst->xy) || (car inst->mosaic->xy)
     insty = (cadr inst->xy) || (cadr inst->mosaic->xy)
     (unless (IsAligned instx xalign)
       aligned = nil
       (printf "Instance \"%s\" of type %s is misaligned in X\n" inst->name inst->cellName )
       )
     (unless (IsAligned insty yalign)
       aligned = nil
       (printf "Instance \"%s\" of type %s is misaligned in Y\n" inst->name inst->cellName )
       )
     )
   aligned
   )
 )

(defun AlignInstX (cv inst moveX)
 (let (instcv xalign  instx instx_new )
     instcv = (nrOpenCellViewReadable inst->libName inst->cellName inst->viewName)
      xalign = (GetXAlignmentProp instcv)
      (when !xalign 
        xalign = MfgGrid
      )
      instx = round( 1000* ((car inst->xy) || (car inst->mosaic->xy)))/1000.0 
      instx_new = ceiling( round( 1000 * (instx + moveX))/1000.0 /  xalign) *xalign
      moveX = round( 1000* (instx_new - instx))/1000.0
      if( moveX > 0.0 then dbMoveFig( inst cv list(moveX:0 "R0" 1.0)))
  moveX
  )
)

(defun IsAligned (x align)
 (let (aligned)
  (if (modulo (floor (round 1000*x)) (floor (round 1000*align)))==0 then
    aligned = t
  else
    aligned = nil
  ) 
 )
)

(defun CopyAlignmentProps (cv1 cv2)
  (if (GetXAlignmentProp cv1) then
    (SetXAlignmentProp cv2 (GetXAlignmentProp cv1))
  else
    (SetXAlignmentProp cv2 MfgGrid)
  )
  (if (GetYAlignmentProp cv1) then
    (SetYAlignmentProp cv2 (GetYAlignmentProp cv1))
  else
    (SetYAlignmentProp cv2 MfgGrid)
  )
)

(defun CreateInstWithAlignmentProps (cv master name point orient)
 (let (newinst cellv xalign yalign)
   newinst = (dbCreateInst cv master name point orient)
   cellv = (nrOpenCellViewReadable newinst->libName newinst->cellName newinst->viewName)
   xalign = (GetXAlignmentProp cellv)
   yalign = (GetYAlignmentProp cellv)
   (when !xalign
     xalign = MfgGrid)
   (when !yalign
     yalign = GridPitch)
   (SetXAlignmentProp newinst xalign)
   (SetYAlignmentProp newinst yalign)
 )
)

(defun GetMaxXAlignmentOfSet (set)
 (let (maxalign cellv align)
  maxalign = MfgGrid
  (foreach inst set
    cellv = (nrOpenCellViewReadable inst->libName inst->cellName inst->viewName)
    align = (GetXAlignmentProp cellv)
    (when !align
      align = MfgGrid
    )
    maxalign = (max maxalign align)
  )
  maxalign
 )
)

(defun GetMaxYAlignmentOfSet (set)
 (let (maxalign cellv align)
  maxalign = MfgGrid
  (foreach inst set
    cellv = (nrOpenCellViewReadable inst->libName inst->cellName inst->viewName)
    align = (GetYAlignmentProp cellv)
    (when !align
      align = MfgGrid
    )
    maxalign = (max maxalign align)
  )
  maxalign
 )
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions interfacing to the virtuoso alignment dialog

(defun AlignSelectedH ()
 (let (tempdir filename file)
  tempdir = ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
  filename = (sprintf nil "%s/align.replay" tempdir)
  file = (outfile filename)

  (fprintf file "leHiAlign()\n")
  (fprintf file "leAlignSelSetForm->sortObjOption->value = \"Instance Name\"\n")
  (fprintf file "leAlignSelSetForm->direction->value = \"Horizontal\"\n")
  (fprintf file "leAlignSelSetForm->alignSepOption->value = \"Component Space\"\n")
  (fprintf file "leAlignSelSetForm->alignObjOption->value = \"Component Origin\"\n")
  (fprintf file "leAlignSelSetForm->alignSepLabel->value = \"Spacings\"\n")
  (fprintf file "leAlignSelSetForm->alignSeparation->value = \"0\"\n")
  (fprintf file "hiFormDone(leAlignSelSetForm)\n")
  (fprintf file "_leAlignSSNewRefCB\n")

  (close file)

  (hiReplayFile filename)
 )
)

(defun AlignSelectedV ()
 (let (tempdir filename file)
  tempdir = ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
  filename = (sprintf nil "%s/align.replay" tempdir)
  file = (outfile filename)

  (fprintf file "leHiAlign()\n")
  (fprintf file "leAlignSelSetForm->sortObjOption->value = \"Instance Name\"\n")
  (fprintf file "leAlignSelSetForm->direction->value = \"Vertical\"\n")
  (fprintf file "leAlignSelSetForm->alignSepOption->value = \"Component Space\"\n")
  (fprintf file "leAlignSelSetForm->alignObjOption->value = \"Component Origin\"\n")
  (fprintf file "leAlignSelSetForm->alignSepLabel->value = \"Spacings\"\n")
  (fprintf file "leAlignSelSetForm->alignSeparation->value = \"0\"\n")
  (fprintf file "hiFormDone(leAlignSelSetForm)\n")
  (fprintf file "_leAlignSSNewRefCB\n")

  (close file)

  (hiReplayFile filename)
 )
)


(defun OpenAlignForm ()
 (let (tempdir filename file)
  tempdir = ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
  filename = (sprintf nil "%s/align.replay" tempdir)
  file = (outfile filename)

  (fprintf file "leHiAlign()\n")
  (fprintf file "leAlignSelSetForm->sortObjOption->value = \"Instance Name\"\n")
  (fprintf file "leAlignSelSetForm->direction->value = \"Horizontal\"\n")
  (fprintf file "leAlignSelSetForm->alignSepOption->value = \"Component Space\"\n")
  (fprintf file "leAlignSelSetForm->alignObjOption->value = \"Component Origin\"\n")
  (fprintf file "leAlignSelSetForm->alignSepLabel->value = \"Spacings\"\n")
  (fprintf file "leAlignSelSetForm->alignSeparation->value = \"0\"\n")

  (close file)

  (hiReplayFile filename)
 )
)
