; move a single instance
(defun MoveInstance ( x y @key (inst nil))
  (when !inst
    inst=(car (geGetSelectedSet))
  )
  (if (IsInstanceAnchored inst) then
    (printf "Not moving anchored instance %s.\n" inst->name)
  else
    inst->xy = x:y
  )
)

; move selected items
(defun MoveSelected ( xOffset yOffset)
  (foreach x (geGetSelSet)
    (cond ((IsInstanceAnchored x)
           (printf "Not moving anchored instance %s.\n" x->name)
           )
          (!x->parent
           (dbMoveFig x nil (list xOffset:yOffset "R0"))
           )
          )
    )
  t
  )

; basic move of selected stuff, while filtering anchors
(defun MoveWithAnchor ()
 (let (selected)
  selected=(geGetSelectedSet)
  (foreach item selected
    (when item->objType=="inst" && (IsInstanceAnchored item)
      (printf "Deselecting anchored instance %s.\n" item->name)
      (geDeselectFig item)
    )
  )
  (leHiMove)
 )
)

; basic stretch of selected stuff, while filtering anchors
(defun StretchWithAnchor ()
 (let (selected)
  selected=(geGetSelectedSet)
  (foreach item selected
    (when item->objType=="inst" && (IsInstanceAnchored item)
      (printf "Deselecting anchored instance %s.\n" item->name)
      (geDeselectFig item)
    )
  )
  (leHiStretch)
 )
)

; click and drag of selected stuff, while filtering anchors
; this is the most common way of inadvertently moving things
(defun SelBoxOrStretchWithAnchor ()
 (let (selected)
  selected=(geGetSelectedSet)
  (foreach item selected
    (when item->objType=="inst" && (IsInstanceAnchored item)
      (printf "Deselecting anchored instance %s.\n" item->name)
      (geDeselectFig item)
    )
  )
  (leSelBoxOrStretch)
 )
)

; basic delete of selected stuff, while filtering protected
(defun DeleteWithProtect ()
 (let (selected)
  selected=(geGetSelectedSet)
  (foreach item selected
    (when item->objType=="inst" && (IsInstanceProtected item)
      (printf "Deselecting protected instance %s.\n" item->name)
      (geDeselectFig item)
    )
    (when item->objType!="inst"
	dbDeleteObject(item)
    )
    (when item->objType=="inst" && !(IsInstanceProtected item)
	dbDeleteObject(item)
    )

  )
;  (leHiDelete)
 )
)

; copy selected cells given an offset
(defun CopySelected (xOffset yOffset)
  SelectedCellList = (geGetSelSet)
  (foreach SelectedCell SelectedCellList
           (dbCopyFig SelectedCell nil (list xOffset:yOffset "R0"))
           )
  t
  )

; find an instance by its name
(defun FindInst (name @key (CV (geGetEditCellView)))
  (dbFindAnyInstByName CV name)
  )

; select an instance by its name
(defun SelectInst (name @key (CV (geGetEditCellView)))
  (let (inst)
    inst = (dbFindAnyInstByName CV name)
    (when inst (geSelectObject inst))
    inst
    )
  )

; find instances by regular expression
(defun FindInstRegExp (rex @key (CV (geGetEditCellView)))
  (let (inst insts)
    (rexCompile rex)
    (foreach inst CV->instances
             (when (rexExecute inst->name) insts = (cons inst insts))
             )
    insts
    )
  )

; select instances by regular expression
(defun SelectInstRegExp (rex @key (CV (geGetEditCellView)))
  (let (inst n)
    n=0
    (rexCompile rex)
    (foreach inst CV->instances
             (when (rexExecute inst->name) n++ (geSelectObject inst))
             )
    n
    )
  )

; select instances filtering by placement status ("locked", "fixed", nil)
(defun SelectPlacementStatus (status @key (CV (geGetEditCellView)))
  (let ((len 0))
    (foreach inst CV->instances
             (when inst->status==status (geSelectObject inst) len++)
             )
    len
    )
  )

; set placement status of select cells
(defun SetPlacementStatus (status @key (CV (geGetEditCellView)))
  (foreach inst (geGetSelSet)
           inst->status=status
           )
  (length (geGetSelSet))
  )

; flip instance upside down
(defun FlipInstY (inst)
  (let (orient xy height)
    height = (RectGetHeight (GetPrbound inst->master)->bBox)
    (cond
          (inst->orient=="R0"
      	    inst->orient = "MX"
      	    inst->xy = (list (car inst->xy) (cadr inst->xy)+height)
	    )
          (inst->orient=="MX"
      	    inst->orient = "R0"
      	    inst->xy = (list (car inst->xy) (cadr inst->xy)-height)
	    )
          (inst->orient=="MY"
      	    inst->orient = "R180"
      	    inst->xy = (list (car inst->xy) (cadr inst->xy)+height)
	    )
          (inst->orient=="R180"
      	    inst->orient = "MY"
      	    inst->xy = (list (car inst->xy) (cadr inst->xy)-height)
	    )
      )
    )
  )

(defun FlipYSelectedInstances ()
  (foreach inst (ss)
    (FlipInstY inst)
    )
  )
