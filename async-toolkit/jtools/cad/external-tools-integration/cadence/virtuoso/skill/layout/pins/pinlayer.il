/**
 * INTEL TOP SECRET
 * Copyright 2002-2012 Intel Corporation. All Rights Reserved.
 * $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/pins/pinlayer.il#0 $
 */

;variety of functions to deal with the change in flow
;whereby we stop streaming "pin" purpose as real metal
;This is in fitting with industry practice, and is now
;required because of Avago's memory layouts.



(defun CoverPinLayerWithDrawing (CV fig)
 (let (pin_bbox)
     (when (cadr fig->lpp)=="pin"
       (dbCreateRect CV (list (car fig->lpp) "drawing") fig->bBox)
     ) 
 )
)

(defun CoverPinLayers (@key (CV (UIGetCellView))) 
   (foreach shape CV->shapes
     (when (cadr shape->lpp)=="pin"
       (CoverPinLayerWithDrawing CV shape)
     )
   )  
)



(defun CheckPinLayer (@key (CV (UIGetCellView)))
 (let (pin_bbox)
   (foreach shape CV->shapes
     (when (cadr shape->lpp)=="pin"
       (printf "pin shape\n")
;this needs to be updated to actually check that the 
;pin layer is covered completely by drawing layer
     )
   )
 )
)

