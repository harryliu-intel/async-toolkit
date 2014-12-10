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

