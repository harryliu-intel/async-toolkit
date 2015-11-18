(defun CreateViaVariant (cv variant point orient)
  (let
    ((paramNameList '("cutWidth"
                      "cutHeight"
                      "cutRows"
                      "cutColumns"
                      "cutSpacing"
                      "layer1Enc"
                      "layer2Enc"
                      "layer1Offset"
                      "layer2Offset"
                      "originOffset"
                      "imp1Enc"
                      "imp2Enc")))
	(dbCreateVia cv variant->viaDef point orient
                 (mapcar 'list paramNameList (cdr variant->params)))))
