; Trim dangling paths and contacts after routing.
;
; Copyright 2011 Fulcrum Microsystems.  All rights reserved.

; Trim dangling paths and contacts for all nets except Vdd/GND.
(defun TrimDanglingWires 
  (@key (CV (geGetEditCellView))
        (overhang 2*DefaultWiringPitch)
        (nets nil)
        (depth 32)
        )
  (let (Vdd GND progress total)
    GND = (dbFindNetByName CV "GND")
    Vdd = (dbFindNetByName CV "Vdd")
    (unless nets nets = (setof net CV->nets (and net!=GND net!=Vdd)))
    total = 0
    (foreach net nets
      progress = 1
      (while progress>0
        progress = 0
        progress = progress + (MergePaths net)
        progress = progress + (TrimDuplicateContacts net)
        progress = progress + (TrimDanglingPaths net ?CV CV
                                                 ?overhang overhang ?depth depth)
        progress = progress + (TrimDanglingContacts net ?CV CV ?depth depth)
        total = total + progress
        )
      )
    total
    )
  )

; Check if two 2-point paths have the same net, lpp, and width, and
; touch at endpoints and are same direction.
(defun ArePathsMergable (path1 path2)
  (let (a1 b1 a2 b2 vert1 vert2)
    (when (and path1 path2
               path1->objType=="path"
               path2->objType=="path"
               (length path1->points)==2 
               (length path2->points)==2
               path1->lpp==path2->lpp
               path1->net==path2->net
               path1->width==path2->width)
      a1 = (car  path1->points)
      b1 = (cadr path1->points)
      a2 = (car  path2->points)
      b2 = (cadr path2->points)
      vert1 = (car a1)==(car b1)
      vert2 = (car a2)==(car b2)
      (and vert1==vert2 (or a1==a2 a1==b2 b1==a2 b1==b2))
      )
    )
  )

; Merge paths before trimming them.  Only merges 2-point paths to
; generate new 2-point paths.
(defun MergePaths (net)
  (let (n paths mpaths npaths path1)
    n = 0
    (foreach lpp MetalLPPs
      paths = (setof obj net->figs
                     (and obj->objType=="path"
                          obj->lpp==lpp
                          (length obj->points)==2))
      ; NOTE: quadratic algorithm in number of paths on each layer of net
      (while paths
        path1 = (car paths)
        paths = (cdr paths)
        npaths = nil
        (foreach path2 paths
          (cond ((ArePathsMergable path1 path2)
                 mpaths = (leMergeShapes (list path1 path2))
                 (when (length mpaths)==1 n = n+1)
                 (foreach path mpaths
                   (when path!=path1 npaths = (cons path npaths))
                   )
                 )
                (t
                 ; add non-merged shape to npaths
                 npaths = (cons path2 npaths)
                 )
                )
          )
        paths = npaths
        )
      )
    n
    )
  )

; Trim dangling paths on a net.  Delete if it only overlaps one
; contact or path, otherwise leave at least some overhang to meet
; minimum area on stacked vias.
(defun TrimDanglingPaths
  (net @key (CV (geGetEditCellView)) (overhang 2*DefaultWiringPitch) (depth 32))
  (let (progress paths xy0 xy1 x0 y0 x1 y1 bbox overlaps unknown
        xy bxy0 bxy1 bx0 by1 bx1 by1
        nx0 ny0 nx1 xy1 n progress)
    progress = 0
    paths = (setof obj net->figs
              (and obj->objType=="path" (length obj->points)==2
                (isMetalDrawing (car obj->lpp) (cadr obj->lpp))))
    (foreach path paths
      ; create bbox from 2 points of path
      xy0 = (car  path->points)
      xy1 = (cadr path->points)
      x0 = (car  xy0)
      y0 = (cadr xy0)
      x1 = (car  xy1)
      y1 = (cadr xy1)
      bbox = path->bBox

      ; find bbox of overlapping paths and contacts
      overlaps = (append (dbGetOverlaps CV bbox (list path->layerName "drawing") depth)
                         (dbGetOverlaps CV bbox (list path->layerName "pin")     depth))
      bbox = nil
      unknown = nil
      n = 0
      (foreach overlap overlaps
               (cond (overlap==path nil) ; ignore self-overlap
                     ; combine bBox of overlapping paths or rects
                     ((and (atom overlap)
                           (or overlap->objType=="path" overlap->objType=="rect"))
                      bbox = (BBoxCombine bbox overlap->bBox)
                      n = n+1
                      )
                     ; combine xy of overlapping contacts
                     ((and !(atom overlap) (car overlap)->libName==TechLibName)
                      xy = (car overlap)->xy
                      bbox = (BBoxCombine bbox (list xy xy))
                      n = n+1
                      )
                     ; subcell pin uses bBox of entire subcell (not optimal)
                     (!(atom overlap)
                       bbox = (BBoxCombine bbox (car overlap)->bBox)
                       n = n+1
                       )
                     ; unknown overlap disables trimming
                     (t unknown=t)
                     )
               )

      ; trim path to fit inside bbox (bloated by DefaultWiringPitch)
      (cond (unknown nil) ; unknown overlaps, leave path alone
            ; if one or less overlapping path or contact, delete dangling path
            (n<=1
             (dbDeleteObject path)
             progress = progress+1
             )
            ; trim path->points to bbox
            (bbox
             bxy0 = (car  bbox)
             bxy1 = (cadr bbox)
             bx0 = (car  bxy0) - overhang
             by0 = (cadr bxy0) - overhang
             bx1 = (car  bxy1) + overhang
             by1 = (cadr bxy1) + overhang
             nx0 = (min bx1 (max bx0 x0))
             nx1 = (min bx1 (max bx0 x1))
             ny0 = (min by1 (max by0 y0))
             ny1 = (min by1 (max by0 y1))
             (when (or (abs nx0-x0)>=MfgGrid (abs ny0-y0)>=MfgGrid
                       (abs nx1-x1)>=MfgGrid (abs ny1-y1)>=MfgGrid)
               path->points = (list nx0:ny0 nx1:ny1)
               progress = progress+1
               )
             )
            )
      )
    progress
    )
  )


; find contacts on this net
(defun FindContactsOnNet (net)
  (let (contacts)
    contacts = nil
    (foreach instTerm net->instTerms
             (when instTerm->inst->libName==TechLibName
               contacts = (cons instTerm->inst contacts))
             )
    (foreach fig net->figs
             (when fig->libName==TechLibName
               contacts = (cons fig contacts))
             )
    contacts
    )
  )

; Delete duplicate contacts
(defun TrimDuplicateContacts (net)
  (let (contacts duplicates)
    ; find contacts on this net
    contacts = (FindContactsOnNet net)

    ; delete duplicates
    duplicates=nil
    (for i 0 (length contacts)-1
         (for j i+1 (length contacts)-1
              (when (AreViasDuplicate (nth i contacts) (nth j contacts))
                duplicates = (cons (nth j contacts) duplicates)
                )
              )
         )
    (foreach contact duplicates (dbDeleteObject contact))    
    (length duplicates)
    )
  )

; Delete dangling contacts on a net that don't overlap multiple lpp's
(defun TrimDanglingContacts
  (net @key (CV (geGetEditCellView)) (depth 32))
  (let (progress contacts bbox nlayers n overlaps)
    progress = 0

    ; find contacts on this net
    contacts = (FindContactsOnNet net)

    ; process contacts
    (foreach contact contacts

      ; count overlapping paths or contacts on each layer of the contact
      bbox = contact->bBox
      nlayers = 0
      (foreach lpp contact->master->lpps
        (when (isMetal lpp->layerName)
          n = 0
          overlaps = (append (dbGetOverlaps CV bbox (list lpp->layerName "drawing") depth)
                             (dbGetOverlaps CV bbox (list lpp->layerName "pin")     depth))
          (foreach overlap overlaps
            (when (or (atom overlap) (car overlap)!=contact) n=1)
            )
          nlayers = nlayers + n
          )
        )

      ; delete dangling contact if <=1 different layers have overlaps
      (when nlayers<=1
        (dbDeleteObject contact)
        progress = progress+1
        )
      )
    progress
    )
  )
