(defun AnchorInstance (inst)
  (when inst
    (unless inst->libName==TechLibName
      (dbReplaceProp inst "Anchored" "boolean" t)
    )
  ) 
)

(defun UnAnchorInstance (inst)
  (when inst
    (unless inst->libName==TechLibName
      (dbReplaceProp inst "Anchored" "boolean" nil)
    )
  ) 
)

(defun IsInstanceAnchored (inst)
 (let (anchored)
  anchored = nil
  (when inst
    (if (dbGetPropByName inst "Anchored")->value=="TRUE" then
      anchored=t
    else
      anchored=nil
    )
  )
  anchored
 )
)

(defun ToggleInstanceAnchor (inst)
  (if (IsInstanceAnchored inst) then
    (UnAnchorInstance inst)
  else
    (AnchorInstance inst)
  )
)

(defun AnchorSelectedInstances ()
 (let (set)
  set=(geGetSelectedSet)
  (foreach item set
    (when item->objType=="inst"
      (AnchorInstance item)
    )
  )
 )
)

(defun UnAnchorSelectedInstances ()
 (let (set)
  set=(geGetSelectedSet)
  (foreach item set
    (when item->objType=="inst"
      (UnAnchorInstance item)
    )
  )
 )
)



;additional properties to protect from deletion

(defun ProtectInstance (inst)
  (when inst
    (unless inst->libName==TechLibName
      (dbReplaceProp inst "Protected" "boolean" t)
    )
  ) 
)

(defun UnProtectInstance (inst)
  (when inst
    (unless inst->libName==TechLibName
      (dbReplaceProp inst "Protected" "boolean" nil)
    )
  ) 
)

(defun IsInstanceProtected (inst)
 (let (protected)
  protected = nil
  (when inst
    (if (dbGetPropByName inst "Protected")->value=="TRUE" then
      protected=t
    else
      protected=nil
    )
  )
  protected
 )
)

(defun ProtectSelectedInstances ()
 (let (set)
  set=(geGetSelectedSet)
  (foreach item set
    (when item->objType=="inst"
      (ProtectInstance item)
    )
  )
 )
)

(defun UnProtectSelectedInstances ()
 (let (set)
  set=(geGetSelectedSet)
  (foreach item set
    (when item->objType=="inst"
      (UnProtectInstance item)
    )
  )
 )
)
