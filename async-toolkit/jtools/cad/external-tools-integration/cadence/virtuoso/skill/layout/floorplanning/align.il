; Align cells according to cell properies for MoveAligned

; set alignment properties
(defun SetCellAlignment (GridXY OffsetXY @key (CV (geGetEditCellView)) (overwrite t) (topHalo nil))
  (when overwrite || !(dbGetPropByName CV "XAlignGrid")
        (dbReplaceProp CV "XAlignGrid"   "float" (car  GridXY)))
  (when overwrite || !(dbGetPropByName CV "YAlignGrid")
        (dbReplaceProp CV "YAlignGrid"   "float" (cadr GridXY)))
  (when overwrite || !(dbGetPropByName CV "XAlignOffset")
        (dbReplaceProp CV "XAlignOffset" "float" (car  OffsetXY)))
  (when overwrite || !(dbGetPropByName CV "YAlignOffset")
        (dbReplaceProp CV "YAlignOffset" "float" (cadr OffsetXY)))
  (when overwrite || !(dbGetPropByName CV "topHalo")
        (dbReplaceProp CV "topHalo" "boolean" (if topHalo "TRUE" "FALSE")))
  t
  )

; delete alignment properties
(defun DeleteCellAlignment (@key (CV (geGetEditCellView)))
  (dbDeletePropByName CV "XAlignGrid")
  (dbDeletePropByName CV "YAlignGrid")
  (dbDeletePropByName CV "XAlignOffset")
  (dbDeletePropByName CV "YAlignOffset")
  t
  )

; get cell height and variant or nil if none
(defun GetLibHeightVariant (@key (CV (geGetEditCellView)))
  (let (CopyExact lib h v)
    CopyExact = (dbGetPropByName CV "CopyExact")->value
    (when CopyExact lib=(substring CopyExact 1 3))
    (cond (CopyExact && lib=="g1m"
                     h = (substring CopyExact 13 1)
                     v = (substring CopyExact 12 1)
                     oy = TrackPitch ; G1M cells are offset by one row from origin
                     (list lib h v oy)
                     )
          (CopyExact && lib=="g1i"
                     h = (substring CopyExact 13 1)
                     v = (substring CopyExact 12 1)
                     oy = 0 ; G1I cells are not offset from origin
                     (list lib h v oy)
                     )
          (CopyExact && lib=="i0s"
                     h = (substring CopyExact 13 1)
                     v = (substring CopyExact 12 1)
                     oy = TrackPitch ; I0S cells are offset by one row from origin
                     (list lib h v oy)
                     )
          (CopyExact && lib=="i0m"
                     h = (substring CopyExact 13 1)
                     v = (substring CopyExact 12 1)
                     oy = 0.18 ; I0M cells are offset by one row from origin
                     (list lib h v oy)
                     )
          (CopyExact && (substring CopyExact 1 6)=="x78srm"
                     h = (substring CopyExact 13 1)
                     v = (substring CopyExact 12 1)
                     oy = 0.0 ; X78 sram column cells are not offset
                     (list lib h v oy)
                     )
          (t nil)
          )
    )
  )

; choose default alignment by CellView (TODO: move to PDK)
(defun DefaultCellAlignment (@key (CV (geGetEditCellView)))
  (let (lhv l h v offset align gx gy ox oy CopyExact)
    lhv = (GetLibHeightVariant ?CV CV)
    l   = (car lhv)
    h   = (cadr lhv)
    v   = (caddr lhv)
    oy  = (cadddr lhv)
    CopyExact = (dbGetPropByName CV "CopyExact")->value
    (cond (lhv
           ox=1.5*GridPolyPitch
           gx=GridPolyPitch
           gy=TrackPitch*(if h=="n" && CV->viewName=="floorplan" 1 2)
           (cond (CopyExact && (substring CopyExact 1 5)=="i0szh" gx=GridPolyPitch ox=0.5*GridPolyPitch) ; halo cells
                 (CopyExact && (substring CopyExact 1 3)=="i0m"
                            ox=0.5*GridPolyPitch
                            gy=0.18*(if h=="n" && CV->viewName=="floorplan" 1 2)
                            )
                 (CopyExact && (substring CopyExact 1 6)=="x78srm"
                            ox=0.5*GridPolyPitch
                            gy=0.21*(if h=="n" && CV->viewName=="floorplan" 1 2)
                            )
                 (l=="i0s" && v=="1" gx=  GridPolyPitch ox=0.5*GridPolyPitch)
                 (l=="i0s" && v=="4" gx=4*GridPolyPitch ox=0.5*GridPolyPitch)
                 (l=="i0s" && v=="8" gx=8*GridPolyPitch ox=0.5*GridPolyPitch)
                 (v=="x" gx=GridPolyPitch gy=TrackPitch ox=0.5*GridPolyPitch oy=0) ; halo cells
                 (v=="c" gx=GridPolyPitch gy=TrackPitch ox=0 oy=0) ; halo cells
                 (v=="y" gx=6*GridPolyPitch gy=TrackPitch ox=0 oy=0) ; halo cells
		 ; new 'i' 't' and 'f' halo types in 0.5.1HP2_20ww43.1-
		 ; this alignment might be wrong, but clearly we don't use them
		 (v=="i" gx=GridPolyPitch gy=TrackPitch ox=0 oy=0)
		 (v=="f" gx=GridPolyPitch gy=TrackPitch ox=0 oy=0)
                 (v=="t" gx=GridPolyPitch gy=TrackPitch ox=0 oy=0)
                 (CV->viewName=="floorplan" && (v=="1" || v=="2" || v=="3") ox=0.5*GridPolyPitch)
                 (CV->viewName=="floorplan" && (v=="0" || v=="4" || v=="5") gx=3.0*GridPolyPitch)
                 (v=="1" gx=3*GridPolyPitch ox=0.5*GridPolyPitch)
                 (v=="2" gx=3*GridPolyPitch)
                 (v=="3" gx=3*GridPolyPitch ox=2.5*GridPolyPitch)
                 (v=="0" gx=3*GridPolyPitch)
                 (v=="4" gx=6*GridPolyPitch)
                 (v=="5" gx=6*GridPolyPitch ox=4.5*GridPolyPitch)
                 (v=="h" gx=3*GridPolyPitch) ; custom halo cells
                 (t (error "Illegal standard cell metal variant %s\n" v))
                 )
           align = (list gx:gy ox:oy)
           )
          (TechLibName=="1278.3" align=(list GridPolyPitch:TrackPitch GridPolyPitch/2:TrackPitch)) ; I0S
          (t align=(list GridPolyPitch/2:GridDiffPitch 0.0:0.0)) ; unknown alignment
          )
    align
    )
  )

; get alignment properies
(defun GetCellAlignment (@key (CV (geGetEditCellView)))
  (let (gx gy ox oy align)
    gx = (dbGetPropByName CV "XAlignGrid")->value
    gy = (dbGetPropByName CV "YAlignGrid")->value
    ox = (dbGetPropByName CV "XAlignOffset")->value
    oy = (dbGetPropByName CV "YAlignOffset")->value
    (unless gx && gy && ox && oy
            align = (DefaultCellAlignment ?CV CV)
            (unless gx gx=(car  (car  align)))
            (unless gy gy=(cadr (car  align)))
            (unless ox ox=(car  (cadr align)))
            (unless oy oy=(cadr (cadr align)))
            )
    (list gx:gy ox:oy)
    )
  )

; Cache the default alignment in cell properties
(defun SetDefaultCellAlignment (@key (CV (geGetEditCellView)) (overwrite nil))
  (let (align)
    align = (DefaultCellAlignment ?CV CV)
    (SetCellAlignment (car align) (cadr align) ?CV CV ?overwrite overwrite)
    align
    )
  )

; Set alignment for a routed topHalo cell
(defun SetTopHaloAlignment (@key (CV (geGetEditCellView)) (overwrite t))
  (SetCellAlignment gridAlignment["lego"] 0:0 ?topHalo t ?overwrite overwrite ?CV CV)
  )

; compute GCD of two integers Euclid's Algorithm
(defun GCD (a b)
  (cond (a==0 b)
        (b==0 a)
        (a>b (GCD (mod a b) b))
        (a<b (GCD (mod b a) a))
        (t a)
        )
  )

; compute LCM of integers usign GCD
(defun LCM (a b)
  (cond (a==0 b)
        (b==0 a)
        (t (a / (GCD a b)) * b)
        )
  )

; compute LCD alignment of set of instances
(defun GetLCDAlignment (@key (x 0) (y 0) (instances (geGetSelSet)))
  (let (grid gx gy)
    x=(round x/MfgGrid)
    y=(round y/MfgGrid)
    (foreach inst instances
             grid=(car (GetCellAlignment ?CV inst->master))
             gx=(round (car  grid)/MfgGrid)
             gy=(round (cadr grid)/MfgGrid)
             x=(LCM x gx)
             y=(LCM y gy)
             )
    (list MfgGrid*x MfgGrid*y)
    )
  )

; Move selected cells by LCM while preserving snap alignments.
; Directions are U D L R to move, A to align, C to check alignment.
(defun MoveAligned (dir @key (dx 0) (dy 0) (CV (geGetEditCellView)) (instances (geGetSelSet)))
  (let (align px py ox oy x y gx gy nx ny aligned)
    instances = (setof inst instances
      inst->cellView==CV && !(IsInstanceAnchored inst) &&
      (inst->objType=="inst"))
    (when instances
      align=(GetCellAlignment ?CV CV)
      px=(car (cadr align))
      py=(cadr (cadr align))
      )
    align=(GetLCDAlignment ?instances instances ?x dx ?y dy)
    dx=(car  align)
    dy=(cadr align)
    aligned=t
    (foreach inst instances
      align=(GetCellAlignment ?CV inst->master)
      gx=(car (car align))
      gy=(cadr (car align))
      ox=(car (cadr align))
      oy=(cadr (cadr align))
      xy=(dbTransformPoint ox:oy (list 0:0 inst->orient))
      ox=(car xy)
      oy=(cadr xy)
      ox=ox-px
      oy=oy-py
      x=(car  inst->xy)
      y=(cadr inst->xy)
      nx=(round (x+ox)/gx)*gx-ox
      ny=(round (y+oy)/gy)*gy-oy
      (cond
       (dir=="L" nx=(round (x-dx+ox)/gx)*gx-ox)
       (dir=="R" nx=(round (x+dx+ox)/gx)*gx-ox)
       (dir=="U" ny=(round (y+dy+oy)/gy)*gy-oy)
       (dir=="D" ny=(round (y-dy+oy)/gy)*gy-oy)
       )
      aligned=aligned && (round nx/MfgGrid)==(round x/MfgGrid) && (round ny/MfgGrid)==(round y/MfgGrid)
      (unless dir=="C" (dbMoveFig inst nil (list (list nx-x ny-y) "R0")))
      )
    aligned
    )
  )

; Check if an instance is aligned including that parent grid is an
; integer multiple of instance grid
(defun CheckInstanceAlignment (inst @key (check_x t) (check_y t) (check_parent_grid t))
  (let (topHalo align pgx pgy pox poy gx gy ox oy xy x y nx ny)
    ; parent alignment
    align=(GetCellAlignment ?CV inst->cellView)
    topHalo=(dbGetPropByName inst->cellView "topHalo")->value=="TRUE"
    pgx=(car  (car  align))
    pgy=(cadr (car  align))
    pox=(car  (cadr align))
    poy=(cadr (cadr align))

    ; transformed instance alignment
    align=(GetCellAlignment ?CV inst->master)
    gx=(car (car align))
    gy=(cadr (car align))
    ox=(car (cadr align))
    oy=(cadr (cadr align))
    xy=(dbTransformPoint ox:oy (list 0:0 inst->orient))
    ox=(car xy)
    oy=(cadr xy)
    ox=ox-pox
    oy=oy-poy
    x=(car  inst->xy)
    y=(cadr inst->xy)
    nx=(round (x+ox)/gx)*gx-ox
    ny=(round (y+oy)/gy)*gy-oy

    ; check alignment
    pgx=(round pgx/MfgGrid)
    pgy=(round pgy/MfgGrid)
    gx=(round gx/MfgGrid)
    gy=(round gy/MfgGrid)
    x=(round x/MfgGrid)
    y=(round y/MfgGrid)
    nx=(round nx/MfgGrid)
    ny=(round ny/MfgGrid)
    inst->status=="unplaced" || (!check_x || nx==x && (!check_parent_grid || topHalo || (mod pgx gx)==0)) &&
                                (!check_y || ny==y && (!check_parent_grid || topHalo || (mod pgy gy)==0))
    )
  )

; Return list of misaligned instances
(defun MisalignedInstances (@key (CV (geGetEditCellView)) (check_x t) (check_y t))
  (let (misaligned align gx gy gxi gyi)
    misaligned=nil
    (foreach inst CV->instances
             (when inst->objType=="inst" && (CheckInstanceAlignment inst ?check_x check_x ?check_y check_y)==nil
               misaligned=(cons inst misaligned))
             )
    misaligned
    )
  )

; Select misaligned instances
(defun SelectMisalignedInstances (@key (CV (geGetEditCellView)))
  (let (insts)
    insts = (MisalignedInstances ?CV CV)
    (foreach inst insts (geSelectObject inst))
    (length insts)
    )
  )

; report all subcells with misaligned instances
(defun RecursiveFindMisaligned (@key (CV (geGetEditCellView)))
  misaligned=
    (FindSubcells ?CV CV ?filter (lambda (CV) (MisalignedInstances ?CV CV)!=nil))
  misaligned = (sort misaligned (lambda (a b) (strcmp a->cellName b->cellName)>0))
  t
  )

; open the next subcell in the misaligned list
(defun OpenNextMisaligned ()
  (let (CV)
    (when misaligned
      CV = (car misaligned)
      misaligned = (cdr misaligned)
      (Open CV->cellName CV->viewName)
      (SelectMisalignedInstances)
      )
    )
  t
  )

; Check if prBoundary points are aligned according to top-level alignment directive
; Used by RouteExport for high-level cells
(defun CheckBoundaryAlignment (@key (CV (geGetEditCellView)))
  (let (aligned align gx gy x y nx ny)
    aligned=t
    (when CV->prBoundary
      align=(GetCellAlignment ?CV CV)
      gx=(car  (car  align))
      gy=(cadr (car  align))
      (foreach xy CV->prBoundary->points
               x=(car  xy)
               y=(cadr xy)
               nx=(round x/gx)*gx
               ny=(round y/gy)*gy
               x=(round x/MfgGrid)
               y=(round y/MfgGrid)
               nx=(round nx/MfgGrid)
               ny=(round ny/MfgGrid)
               (unless x==nx && y==ny
                       (printf "misaligned boundary point %g:%g\n" (car xy) (cadr xy))
                       aligned=nil
                       )
               )
      )
    aligned
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Obsolete Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         (printf "Instance %s in cell %s overlaps another at (%.3f %.3f)\n"
                 inst->name cv->cellName (car inst->xy) (cadr inst->xy))
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
      printf("Instance %s (%s) is moved by %.3f to snap to X alignment.\n" inst~>name inst~>cellName new_move)
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
       (printf "Instance %s of type %s is misaligned in X\n" inst->name inst->cellName )
       )
     (unless (IsAligned insty yalign)
       aligned = nil
       (printf "Instance %s of type %s is misaligned in Y\n" inst->name inst->cellName )
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

(defun SetInstAlignmentProps (inst)
  (let (xalign yalign)
    xalign = (GetXAlignmentProp inst->master)
    yalign = (GetYAlignmentProp inst->master)
    (unless xalign xalign = MfgGrid)
    (unless yalign yalign = GridPitch)
    (SetXAlignmentProp inst xalign)
    (SetYAlignmentProp inst yalign)
    inst
    )
  )

(defun CreateInstWithAlignmentProps (cv master name point orient)
  (let (newinst)
    newinst = (dbCreateInst cv master name point orient)
    (SetInstAlignmentProps newinst)
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
