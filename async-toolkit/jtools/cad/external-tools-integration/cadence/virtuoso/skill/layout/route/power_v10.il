; Draw v10 to connect subcell m10 pins to m11 power stripe pattern
(defun PowerV10 (inst_name
                 @key (CV (geGetEditCellView))
                      (xp 1.2)          ; horizontal pitch of m11 stripe pattern
                      (pattern (list (list "VSS"  0.166 0.18) ; name, width, offset of m11 stripes
                                     (list "VDD"  0.166 0.42)
                                     (list "V0"   0.166 0.42) ; shares VDD's track
                                     (list "V1"   0.166 0.42) ; shares VDD's track
                                     (list "VSS"  0.166 0.78)
                                     (list "VDDM" 0.166 1.02)
                                     )
                               )
                      )
  (let (inst viaCV net new_net bbox x0 x1 y0 y1 x y w
        col0 col1 m11_name m11_w m11_x def_params viaDef params via)

    ; create empty via overlay instance
    inst = (dbFindAnyInstByName CV "vias")
    (when inst (dbDeleteObject inst))
    viaCV = (ReadCV CV->libName (strcat CV->cellName "_vias") "layout")
    (when viaCV (cdsp4edit ?CV viaCV))
    viaCV = (dbOpenCellViewByType CV->libName (strcat CV->cellName "_vias") "layout" "maskLayout" "w")

    ; get subcell instance with m10 pins
    inst = (dbFindAnyInstByName CV inst_name)
    (unless inst (error "inst_name %s not found\n" inst_name))

    ; add v10 over all m10 pins of inst
    (foreach shape inst->master->shapes
             (when shape->pin && shape->lpp==MetalPin[10]
               net=shape->pin->net
               new_net=(dbMakeNet viaCV net->name)
               bbox=(dbTransformBBox shape->bBox inst->transform)
               x0=(car  (car  bbox))+0.119
               x1=(car  (cadr bbox))-0.119
               y0=(cadr (car  bbox))
               y1=(cadr (cadr bbox))
               y=(y0+y1)/2
               w=y1-y0
               col0=(floor   x0/xp)
               col1=(ceiling x1/xp)
               (foreach m11 pattern
                        m11_name=(car m11)
                        (when net->name==m11_name
                          m11_w=(cadr m11)
                          m11_x=(caddr m11)
                          def_params=(GetViaDefParams 10 w m11_w ?CV viaCV)
                          viaDef=(car  def_params)
                          params=(cadr def_params)
                          (for col col0 col1-1
                               x=col*xp+m11_x
                               (when x>=x0+m11_w/2 && x<=x1-m11_w/2 ; HACK: m10 pins poorly aligned in PADS_V
                                     via = (dbCreateVia viaCV viaDef x:y "R0" params)
                                     via->net = new_net
                                     )
                               )
                          )
                        )
               )
             )

    ; finish
    (dbSave viaCV)
    (cdsp4add ?CV viaCV)
    inst = (dbCreateInst CV viaCV "vias" 0:0 "R0")
    (dbReplaceProp inst "autogen" "boolean" t)
    (dbSave CV)
    viaCV
    )
  )

; get viaDef and params using viaTable (TODO: move to layout/route/nearby.il)
(defun GetViaDefParams (v wb wt @key (CV (geGetEditCellView)))
  (let (tech viaName viaDef params)
    tech = (techGetTechFile CV)
    wb = (round wb/MfgGrid)
    wt = (round wt/MfgGrid)
    viaName = viaTable[(list v wb wt)]
    (unless viaName (error "no viaTable for %d %d %d\n" v wb wt))
    (cond ((atom viaName)
           viaDef = (techFindViaDefByName tech viaName)
           params = nil
           )
          (t
           viaDef = (techFindViaDefByName tech (car viaName))
           params = (cdr viaName)
           viaName = (car viaName)
           )
          )
    (list viaDef params)
    )
  )
