;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 10nm porting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; scale a point from 14nm D04 to 10nm E08
(defun scalePoint (xy)
  (let (x y)
    x = (car xy)
    y = (cadr xy)
    x = (int x/0.070)*0.060 ; poly pitch
    y = (int y/0.399)*0.340 ; row pitch
    x:y
    )
  )

actionScaleTo10 =
(lambda (CV) 
  (unless (GetProp CV "scaled" nil)
    CV->modified=t
    (dbReplaceProp CV "scaled" "boolean" t)
    (foreach inst CV->instances inst->xy = (scalePoint inst->xy))
    (dbSave CV)
    )
  )
