; $Id$

(defun DistortLayer (@key (CV nil) (layer "M7") (shift t) (debug nil))
  (let (y tract dy_M7 twoCutContacts shapes contacts points tmp_y m6layer vOrient hOrient M7Contacts)
    CV = (if CV CV (geGetEditCellView))
    m6layer="M6"
    shapes = (setof shape CV->shapes (car shape->lpp)==layer)
    contacts = (setof contact CV->instances
                      (and contact->libName=="tsmc65"
                           (setof x contact->master->lpps x->layerName==layer))
                      )

    (when debug printf("Distort Metal\n"))
    (foreach shape shapes
             (cond ((or shape->objType=="path" shape->objType=="polygon")
                    shape->points = (DistortPoints shape->points)
                    )
                   (shape->objType=="rect"
                    ; the top and bottom needs to move the same distance to align with path and avoid edge errors
                    tmp_y= (topEdge(shape->bBox) + bottomEdge(shape->bBox))/2
                    dy= cadr( DistortPoint( 0:tmp_y )) - tmp_y
                    shape->bBox = list( leftEdge(shape->bBox):bottomEdge(shape->bBox)+dy rightEdge(shape->bBox):topEdge(shape->bBox)+dy)
                    )
                   )
             )

    ; Find All Via with 0.14 spacing of a vertical M7 path
    (when debug printf("Find MinX %d\n" length(shapes)))
    minXContacts=nil
    foreach( shape shapes
       if( shape->objType=="path" then
           points=listToVector( shape->points )
           for( i 0 length(points)-2
              if(car(points[i])==car(points[i+1]) then 
                instancesWithinRange=  dbProduceOverlapInst( CV 
                                                  list(car(points[i])-0.14:min( cadr(points[i]) cadr(points[i+1]))-0.095
                                                       car(points[i])+0.14:max( cadr(points[i]) cadr(points[i+1]))+0.095)
                            ) 
                instancesOverlappingPath=  dbProduceOverlapInst( CV 
                                                  list(car(points[i])-0.01:min( cadr(points[i]) cadr(points[i+1]))-0.01
                                                       car(points[i])+0.01:max( cadr(points[i]) cadr(points[i+1]))+0.01)
                            ) 

                instances=setof( inst instancesWithinRange member( inst contacts ) && !member( inst instancesOverlappingPath))
                minXContacts=append(minXContacts instances) 
              ) 
           )
       )
    )
    (when debug printf("Find MinX %d\n" length(minXContacts)))
    plusYContacts=nil
    minusYContacts=nil
    contactOverlapShapes=nil
    if(shift then
      ; does a number of things:
      ; 1) makes a list of contacts with m6 pointing up and down (plus and minus respectively) this part
      ;    is used only for double cut vias later
      ; 2) adds to each contact the width of the m6 path, the offset in X if any (to see if the contact
      ;    is aligned to the m6 path, the length of the path overlap at the end point in case that overlap
      ;    is being used to fix drc (a frequent occurance).
      ; I make no attempt to remove the data from the contact but it is only added to M7_M6 contacts anyway.
      ; note that, like minXContacts above, plusYContacts and minusYContacts are global variables
      shapes = (setof shape CV->shapes (car shape->lpp)==m6layer && shape~>objType=="path")
      vOrient=list("R0" "R180" "MX" "MY")
      hOrient=list("R90" "R270" "MXR90" "MYR90")
      ; two cut contacts which are actually oriented vertically
      twoCutContacts=(setof contact contacts
          (contact~>cellName=="M7_M6_2CUT_H" && member(contact~>orient hOrient)) ||
              (contact~>cellName=="M7_M6_2CUT_V") && member(contact~>orient vOrient))

      M7Contacts=list("M7_M6_R0" "M7_M6_H" "M7_M6_V" "M7_M6_R90" 
        "M7_M6_STACK" "M7_M6_STACK_R90" "M7_M6_2CUT_H" "M7_M6_2CUT_V" 
        "M7_M6min" "M7_M6c" )
      when( debug printf("Find XY %d %d %d\n" length(shapes) length(twoCutContacts) length(contacts)))
      foreach( shape shapes
         points=listToVector( shape->points )
         for( i 0 length(points)-2
            ; vertical segments only
            if(car(points[i])==car(points[i+1]) then 
              ext1=shape~>width/2
              ext2=shape~>width/2
              if(i==0 && shape~>pathStyle=="varExtendExtend" ext1=shape~>beginExt)
              if(i==length(points)-2 && shape~>pathStyle=="varExtendExtend" ext2=shape~>endExt)
              if(cadr(points[i]) - cadr(points[i+1]) > 0.18 then
                ; one end
                instancesOverlappingPoint=dbProduceOverlapInst( CV
                                                      list(car(points[i])-0.01:cadr(points[i])-0.01
                                                           car(points[i])+0.01:cadr(points[i])+0.01))
                minusYContacts=append(minusYContacts setof( inst instancesOverlappingPoint member( inst~>cellName M7Contacts )))
                if(shape~>width > 0.0 then
                    foreach( s setof( i instancesOverlappingPoint member( i~>cellName M7Contacts ) ) s~>pathWidth=shape~>width s~>XOffset=car(s~>xy)-car(points[i]) s~>YOverlap=cadr(points[i])+ext1-cadr(s~>xy))
                )
                ; other end
                instancesOverlappingPoint=dbProduceOverlapInst( CV
                                                      list(car(points[i+1])-0.01:cadr(points[i+1])-0.01
                                                           car(points[i+1])+0.01:cadr(points[i+1])+0.01))
                plusYContacts=append(plusYContacts setof( inst instancesOverlappingPoint member( inst~>cellName M7Contacts )))
                if(shape~>width > 0.0 then
                    foreach( s setof( i instancesOverlappingPoint member( i~>cellName M7Contacts ) ) s~>pathWidth=shape~>width s~>XOffset=car(s~>xy)-car(points[i]) s~>YOverlap=cadr(s~>xy)-cadr(points[i+1])-ext2)
                )
              )
              if(cadr(points[i]) - cadr(points[i+1]) < -0.18 then
                ; one end
                instancesOverlappingPoint=dbProduceOverlapInst( CV
                                                      list(car(points[i])-0.01:cadr(points[i])-0.01
                                                           car(points[i])+0.01:cadr(points[i])+0.01))
                plusYContacts=append(plusYContacts setof( inst instancesOverlappingPoint member( inst~>cellName M7Contacts )))
                if(shape~>width > 0.0 then
                    foreach( s setof( i instancesOverlappingPoint member( i~>cellName M7Contacts ) ) s~>pathWidth=shape~>width s~>XOffset=car(s~>xy)-car(points[i]) s~>YOverlap=cadr(s~>xy)-cadr(points[i])-ext1)
                )
                ; other end
                instancesOverlappingPoint=dbProduceOverlapInst( CV
                                                      list(car(points[i+1])-0.01:cadr(points[i+1])-0.01
                                                           car(points[i+1])+0.01:cadr(points[i+1])+0.01))
                minusYContacts=append(minusYContacts setof( inst instancesOverlappingPoint member( inst~>cellName M7Contacts )))
                if(shape~>width > 0.0 then
                    foreach( s setof( i instancesOverlappingPoint member( i~>cellName M7Contacts ) ) s~>pathWidth=shape~>width s~>XOffset=car(s~>xy)-car(points[i]) s~>YOverlap=cadr(points[i+1])+ext2-cadr(s~>xy))
                )
              )
           )
         )
      )
      (when debug printf("Minus Contacts %d, Plus Contacts %d\n" length(minusYContacts) length(plusYContacts)))
    )

    (when debug printf("DistortContacts\n"))
    (foreach contact contacts (DistortContact contact ?shift shift))
    when( debug printf("Done DistortLayer\n"))
  t
  )
)

; distort Y-coordinates of a list a points
(defun DistortPoints (points)
  (when points
    (cons (DistortPoint (car points)) (DistortPoints (cdr points)))
    )
  )

; distort Y-coordinate of one point
(defun DistortPoint (point)
  (let (x y track dy)
    x = round((car point)*1000.0)/1000.0
    y = round((cadr point)*1000.0)/1000.0
    ; the rounding is not always consistently offset from the PG for 1/2 track offsets
    track = (mod (round (y-0.12)/0.24+0.000001) 12)
    track = (mod (track+12) 12)
    dy = (nth track (list 0.02 0.02 0.015 0.01 0.005 0
                          0 -0.005 -0.01 -0.015 -0.02 -0.02))
    x:y+dy
    )
  )


(defun GetDefaultExtensions (contact)
   prog((value)
; used to be only for R0, removed that restriction
;      if( contact~>orient !="R0" then return(nil))
      cond( 
;       there seems to be no way to get this with skill, so this comes from reading the
;       data interactively
;          layer1XDefOverride layer1YDefOverride layer2XDefOverride layer2YDefOverride
;          layer1XEnclosure layer1YEnclosure layer2XEnclosure layer2YEnclosure
;          layer1Direction layer2Direction isOneCut
         (contact->cellName=="M7_M6_R0"
             value=list(0.07 0.07 0.07 0.07 0.01 0.05 0.05 0.01 "lrtb" "lrtb" t)) 
         (contact->cellName=="M7_M6_H"
             value=list(0.07 0.07 0.07 0.07 0.05 0.01 0.05 0.01 "lrtb" "lrtb" t)) 
         (contact->cellName=="M7_M6_V"
             value=list(0.07 0.07 0.07 0.07 0.01 0.05 0.01 0.05 "lrtb" "lrtb" t)) 
         (contact~>cellName=="M7_M6_R90" 
             value=list(0.07 0.07 0.07 0.07 0.05 0.01 0.01 0.05 "lrtb" "lrtb" t)) 
         (contact->cellName=="M7_M6_STACK" 
             value=list(0.07 0.07 0.07 0.07 0.01 0.17 0.05 0.01 "lrtb" "lrtb" t)) 
         (contact->cellName=="M7_M6_STACK_R90" 
             value=list(0.07 0.07 0.07 0.07 0.17 0.01 0.01 0.05 "lrtb" "lrtb" t)) 
         (contact->cellName=="M7_M6_2CUT_H" 
             value=list(0.07 0.07 0.07 0.07 0.04 0.01 0.04 0.01 "lrtb" "lrtb" nil)) 
         (contact->cellName=="M7_M6_2CUT_V" 
             value=list(0.07 0.07 0.07 0.07 0.01 0.04 0.01 0.04 "lrtb" "lrtb" nil)) 
         (contact->cellName=="M7_M6min"
             value=list(0.005 0.005 0.005 0.005 0.005 0.005 0.005 0.005 "lrtb" "lrtb" t)) 
         (contact->cellName=="M7_M6c" 
             value=list(0.01 0.04 0.04 0.01 0.01 0.04 0.04 0.01 "lrtb" "lrtb" t)) 
         (t
             return(nil))
      )
      return(listToVector(value))
   )
)

; distort extensions of single cuts
; not yet used, it is intended to handle rotations better
(defun DistortExtensionsSingle (
    layer1YTopEnclosure layer1YBottomEnclosure layer1XLeftEnclosure layer1XRightEnclosure
      layer2YTopEnclosure layer2YBottomEnclosure layer2XLeftEnclosure layer2XRightEnclosure used_dy)
  (let ()
    if(layer1XLeftEnclosure < 0.01 layer1XLeftEnclosure=0.01)
    if(layer1XRightEnclosure < 0.01 layer1XRightEnclosure=0.01)
    ; move top of M6
    layer1YTopEnclosure=layer1YTopEnclosure-used_dy
    if(layer1YTopEnclosure < 0.0 layer1YTopEnclosure=0.0)
    if(layer1YTopEnclosure < 0.04 && ( layer1XLeftEnclosure < 0.03 || layer1XRightEnclosure < 0.03) layer1YTopEnclosure=0.04)
    ; move bottom of M6
    layer1YBottomEnclosure=layer1YBottomEnclosure+used_dy
    if(layer1YBottomEnclosure < 0.0 layer1YBottomEnclosure=0.0)
    if(layer1YBottomEnclosure < 0.04 && ( layer1XLeftEnclosure < 0.03 || layer1XRightEnclosure < 0.03) layer1YBottomEnclosure=0.04)
  )
  list(layer1YTopEnclosure layer1YBottomEnclosure layer1XLeftEnclosure layer1XRightEnclosure
      layer2YTopEnclosure layer2YBottomEnclosure layer2XLeftEnclosure layer2XRightEnclosure used_dy)
)

; distort and move contact
(defun DistortContact (contact @key (shift t) )
  (prog (x y track dy used_dy dy_contact dy_M7
        layer1XDefOverride layer1YDefOverride layer2XDefOverride layer2YDefOverride
        layer1XEnclosure layer1YEnclosure layer2XEnclosure layer2YEnclosure
        layer1Direction layer2Direction isOneCut
        layer1YTopEnclosure layer1YBottomEnclosure layer1XLeftEnclosure layer1XRightEnclosure
        layer2YTopEnclosure layer2YBottomEnclosure layer2XLeftEnclosure layer2XRightEnclosure
        defaultExtension extensionList
        shiftDistance actualDistance
        )
    layer1XDefOverride=nil
    layer1YDefOverride=nil
    layer2XDefOverride=nil
    layer2YDefOverride=nil
    layer1XEnclosure=nil
    layer1YEnclosure=nil
    layer2XEnclosure=nil
    layer2YEnclosure=nil
    layer1Direction=nil
    layer2Direction=nil
    isOneCut=nil
    x = round((car contact~>xy)*1000.0)/1000.0
    y = round((cadr contact~>xy)*1000.0)/1000.0
    track = (mod (round (y-0.12)/0.24) 12)
    track = (mod (track+12) 12)
    ; be absolutely sure this matches where it is defined elsewhere too!
    dy_contact = (nth track (list 0.01 0.01 0.005 0    0     0 0  0      0    -0.005 -0.01 -0.01))
    dy_M7 = (nth track      (list 0.02 0.02 0.015 0.01 0.005 0 0 -0.005 -0.01 -0.015 -0.02 -0.02))

    if(contact~>OriginalY==nil then
        contact~>OriginalY=cadr(contact~>xy)+dy_M7
    )

    if( dy_M7 == 0.0 then return(nil) )
    used_dy=dy_contact

    defaultExtension=GetDefaultExtensions(contact)

    if( contact->cellName=="M7_M6min"
       used_dy=dy_M7
       used_dy=dy_contact
    )
         
    ; gather the actual contact extensions
    if(defaultExtension != nil then
      layer1XDefOverride = dbGetPropByName(contact "layer1XDefOverride")~>value || defaultExtension[0]
      layer1YDefOverride = dbGetPropByName(contact "layer1YDefOverride")~>value || defaultExtension[1]
      layer2XDefOverride = dbGetPropByName(contact "layer2XDefOverride")~>value || defaultExtension[2]
      layer2YDefOverride = dbGetPropByName(contact "layer2YDefOverride")~>value || defaultExtension[3]
      layer1XEnclosure = dbGetPropByName(contact "layer1XEnclosure")~>value || defaultExtension[4]
      layer1YEnclosure = dbGetPropByName(contact "layer1YEnclosure")~>value || defaultExtension[5]
      layer2XEnclosure = dbGetPropByName(contact "layer2XEnclosure")~>value || defaultExtension[6]
      layer2YEnclosure = dbGetPropByName(contact "layer2YEnclosure")~>value || defaultExtension[7]
      layer1Direction = dbGetPropByName(contact "layer1Direction")~>value || defaultExtension[8]
      ; need to recognize old strings
      if( layer1Direction == "yx" layer1Direction = "lrtb")
      if( layer1Direction == "y" layer1Direction = "tb")
      if( layer1Direction == "x" layer1Direction = "lr")
      layer2Direction = dbGetPropByName(contact "layer2Direction")~>value || defaultExtension[9]
      ; need to recognize old strings
      if( layer2Direction == "yx" layer2Direction = defaultExtension[9])
      if( layer2Direction == "y" layer2Direction = "tb")
      if( layer2Direction == "x" layer2Direction = "lr")
      ; these are the variables actually used!
      isOneCut=defaultExtension[10]
      layer1YTopEnclosure = if( rexMatchp("t" layer1Direction) layer1YEnclosure layer1YDefOverride )
      layer1YBottomEnclosure = if( rexMatchp("b" layer1Direction) layer1YEnclosure layer1YDefOverride )
      layer1XLeftEnclosure = if( rexMatchp("l" layer1Direction) layer1XEnclosure layer1XDefOverride )
      layer1XRightEnclosure = if( rexMatchp("r" layer1Direction) layer1XEnclosure layer1XDefOverride )
      layer2YTopEnclosure = if( rexMatchp("t" layer2Direction) layer2YEnclosure layer2YDefOverride )
      layer2YBottomEnclosure = if( rexMatchp("b" layer2Direction) layer2YEnclosure layer2YDefOverride )
      layer2XLeftEnclosure = if( rexMatchp("l" layer2Direction) layer2XEnclosure layer2XDefOverride )
      layer2XRightEnclosure = if( rexMatchp("r" layer2Direction) layer2XEnclosure layer2XDefOverride )
    )

    if( defaultExtension==nil then
        let( (left right bottom top top6 top7 bottom6 bottom6 shapeM6 shapeM7)
            ; non-parameterized vias, currently. only R0 supported
            ; for these we cannot manipulate the internal extensions, so metal has
            ; to be added instead
            if( contact~>orient == "R0" && ( used_dy != 0 || dy_M7 != 0 ) then
                ; if it moves:
                ; the via is moved and so is the overlap. To guarantee
                ; that the old edge remains coincident with the old metal,
                ; have to create new little rectangles.
                ; assumes only one M7 and one M6 shape
                shapeM6=car(setof( shape contact~>master~>shapes shape~>lpp == Metal6LPP))
                if(shapeM6 then
                    left=car(contact~>xy)+car(car(shapeM6~>bBox))
                    right=car(contact~>xy)+car(cadr(shapeM6~>bBox))
                    bottom=cadr(contact~>xy)+cadr(car(shapeM6~>bBox))
                    top=cadr(contact~>xy)+cadr(cadr(shapeM6~>bBox))
                    if( used_dy > 0 then
                        ; via moved up, fill bottom side for M6
                        bottom6=bottom;
                        top6=bottom+used_dy
                    else
                        ; via moved down, fill top side for M6
                        bottom6=top
                        top6=top+used_dy
                    )
                    if( bottom6 != top6 
                        dbCreateRect(contact~>cellView Metal6LPP list(list(left bottom6) list(right top6)))
                    )
                )
                shapeM7=car(setof( shape contact~>master~>shapes shape~>lpp == Metal7LPP))
                if(shapeM7 then
                    left=car(contact~>xy)+car(car(shapeM7~>bBox))
                    right=car(contact~>xy)+car(cadr(shapeM7~>bBox))
                    bottom=cadr(contact~>xy)+cadr(car(shapeM7~>bBox))
                    top=cadr(contact~>xy)+cadr(cadr(shapeM7~>bBox))
                    if( dy_M7 > 0 then
                        ; fill top side for M7
                        bottom7=top+used_dy
                        top7=top+dy_M7
                    else
                        ; fill bottom side for M7
                        top7=bottom+used_dy
                        bottom7=bottom+dy_M7
                    )
                    ; rectangle fill
                    if( bottom7 != top7
                        dbCreateRect(contact~>cellView Metal7LPP list(list(left bottom7) list(right top7)))
                    )
                )
                ; move the contact
                contact->xy = x:y+used_dy
            )
            ; change this if/when non R0 supported
            if(contact~>orient != "R0" printf("Non parameterized contact %s, orientation %s, not yet supported, ignored\n"
                contact~>cellName contact~>orient))
        )
        return(nil)
    )
    if( isOneCut then
        ; single cut, assume vertical M6 horiz M7, move full M7 change, expand M6, do not touch M7
        ; I know this is controversial, but it works more often then the original idea of using the smaller shift,
        ; at least in my experience doing hundreds of cells
        used_dy=dy_M7
      (cond
        ( contact~>orient == "R0"
          ; R0, this is model for all rotations
          ;      T
          ;   L     R
          ;      B
          ;px=nil
          ;if(x==-2.16 && y==-55.2 then printf("%s dy=%f x=%f y=%f o=%L l=%f r=%f t=%f b=%f\n" contact~>cellName
          ;    used_dy car(contact~>xy) cadr(contact~>xy) contact~>orient
          ;    layer1XLeftEnclosure, layer1XRightEnclosure
          ;    layer1YTopEnclosure layer1YBottomEnclosure) px=t)
          minext1=nil
          minext2=nil
          touchext1=nil
          if(layer1XLeftEnclosure==0.005 && layer1YTopEnclosure==0.005 &&
            layer1XRightEnclosure==0.005 && layer1YBottomEnclosure==0.005 then
;                if(dy_M7 > 0.006 used_dy=0.005 used_dy=0)
;                if(dy_M7 < -0.006 used_dy=-0.005 used_dy=0)
                if(contact~>pathWidth != nil then
                    ; if pathWidth defined, the there is a m6 path under the contact
                    ;if(px printf("%.3f %.3f %.3f %.0f\n" contact~>pathWidth contact~>XOffset contact~>YOverlap contact~>Type))
                    if(contact~>XOffset == 0.0 then
                        ; if the offset is 0, then the contact is horizontally aligned to the path
                        ; then we make the Left/Right overlap match the path width, minimizes DRCs at the ends
                        layer1XLeftEnclosure=(contact~>pathWidth-0.1)/2
                        layer1XRightEnclosure=(contact~>pathWidth-0.1)/2
                        ; if the overlap seems to fix DRC, the adjust contact to match original overlap
                        ; top side
                        if(contact~>YOverlap-0.05 >= 0.04
                            layer1YTopEnclosure=contact~>YOverlap-0.05
                            layer1YTopEnclosure=0.04
                        )
                        ; bottom side
                        if(contact~>YOverlap+0.05 <= -0.04
                            layer1YBottomEnclosure=-contact~>YOverlap-0.05
                            layer1YBottomEnclosure=0.04
                        )
                        touchext1=t
                        ;if(px printf("layer1YTopEnclosure=%.3f layer1YBottomEnclosure=%.3f\n" layer1YTopEnclosure layer1YBottomEnclosure))
                    )
                else
                    ; found that single contacts frequently need to meet minimum area if they do not
                    ; overlap a path, this guarantees min area
                    layer1XLeftEnclosure=0.01
                    layer1XRightEnclosure=0.01
                    layer1YTopEnclosure=0.07
                    layer1YBottomEnclosure=0.07
                )
                minext1=t
            )
          if(! minext1 then
            ; never let the overlap left/right be less than 0.01 if it is not a min overlap contact
            if(layer1XLeftEnclosure < 0.01 layer1XLeftEnclosure=0.01)
            if(layer1XRightEnclosure < 0.01 layer1XRightEnclosure=0.01)
          )
          ; move top of M6, if not already set above
          if( ! touchext1 layer1YTopEnclosure=layer1YTopEnclosure-used_dy)
          ; avoid negatives
          if(layer1YTopEnclosure < 0.0 then layer1YTopEnclosure=0.0)
          if(! minext1 && layer1YTopEnclosure < 0.04 && ( layer1XLeftEnclosure < 0.03 || layer1XRightEnclosure < 0.03) layer1YTopEnclosure=0.04)
          ; move bottom of M6, if not already set above
          if(! touchext1 layer1YBottomEnclosure=layer1YBottomEnclosure+used_dy)
          ; avoid negatives
          if(layer1YBottomEnclosure < 0.0 layer1YBottomEnclosure=0.0)
          if(! minext1 && layer1YBottomEnclosure < 0.04 && ( layer1XLeftEnclosure < 0.03 || layer1XRightEnclosure < 0.03) layer1YBottomEnclosure=0.04)
          ; move the thing
          contact~>xy = x:y+used_dy
          ; set extensions
          dbReplaceProp( contact "layer1YDefOverride" "float" layer1YTopEnclosure) ; top
          dbReplaceProp( contact "layer1YEnclosure" "float" layer1YBottomEnclosure) ; bottom
          dbReplaceProp( contact "layer1XDefOverride" "float" layer1XLeftEnclosure) ; left
          dbReplaceProp( contact "layer1XEnclosure" "float" layer1XRightEnclosure) ; right
          dbReplaceProp( contact "layer1Direction" "string" "br")
          ; obviously this never happens here, but it here for a place holder
          if(used_dy != dy_M7 then
            dbReplaceProp( contact "layer2YDefOverride" "float" layer2YTopEnclosure) ; top
            dbReplaceProp( contact "layer2YEnclosure" "float" layer2YBottomEnclosure) ; bottom
            dbReplaceProp( contact "layer2XDefOverride" "float" layer2XLeftEnclosure) ; left
            dbReplaceProp( contact "layer2XEnclosure" "float" layer2XRightEnclosure) ; right
            dbReplaceProp( contact "layer2Direction" "string" "br")
            )

        return( t )
        )
        ( contact~>orient == "R270"
          ; R270: rotates CW 90deg (single cuts)
          ;       L
          ;    B     T
          ;       R
          ;
          minext1=nil
          minext2=nil
          touchext1=nil
          if(layer1YBottomEnclosure==0.005 && layer1XLeftEnclosure==0.005 &&
            layer1YTopEnclosure==0.005 && layer1XRightEnclosure==0.005 then
;                if(dy_M7 > 0.006 used_dy=0.005 used_dy=0)
;                if(dy_M7 < -0.006 used_dy=-0.005 used_dy=0)
                if(contact~>pathWidth != nil then
                    if(contact~>XOffset == 0.0 then
                        layer1YBottomEnclosure=(contact~>pathWidth-0.1)/2
                        layer1YTopEnclosure=(contact~>pathWidth-0.1)/2
                        if(contact~>YOverlap-0.05 >= 0.04
                            layer1XLeftEnclosure=contact~>YOverlap-0.05
                            layer1XLeftEnclosure=0.04
                        )
                        if(contact~>YOverlap+0.05 <= -0.04
                            layer1XRightEnclosure=-contact~>YOverlap-0.05
                            layer1XRightEnclosure=0.04
                        )
                        touchext1=t
                    )
                else
                    layer1YBottomEnclosure=0.01
                    layer1YTopEnclosure=0.01
                    layer1XLeftEnclosure=0.07
                    layer1XRightEnclosure=0.07
                )
                minext1=t
            )
;          if(layer2YBottomEnclosure==0.005 && layer2XLeftEnclosure==0.005 &&
;            layer2YTopEnclosure==0.005 && layer2XRightEnclosure==0.005 then
;                layer2YBottomEnclosure=0.07
;                layer2YTopEnclosure=0.07
;                layer2XLeftEnclosure=0.01
;                layer2XRightEnclosure=0.01
;                minext2=t
;            )
          if(! minext1 then
            if(layer1YBottomEnclosure < 0.01 layer1YBottomEnclosure=0.01)
            if(layer1YTopEnclosure < 0.01 layer1YTopEnclosure=0.01)
          )
          ; move top of M6
          if( ! touchext1 layer1XLeftEnclosure=layer1XLeftEnclosure-used_dy)
          if(layer1XLeftEnclosure < 0.0 then layer1XLeftEnclosure=0.0)
;          if((member( contact plusYContacts) || member( contact minusYContacts)) && ! minext1 && layer1XLeftEnclosure < 0.04 && ( layer1YBottomEnclosure < 0.03 || layer1YTopEnclosure < 0.03) layer1XLeftEnclosure=0.04)
          if(! minext1 && layer1XLeftEnclosure < 0.04 && ( layer1YBottomEnclosure < 0.03 || layer1YTopEnclosure < 0.03) layer1XLeftEnclosure=0.04)
          ; move bottom of M6
          if(! touchext1 layer1XRightEnclosure=layer1XRightEnclosure+used_dy)
          if(layer1XRightEnclosure < 0.0 layer1XRightEnclosure=0.0)
;          if((member( contact plusYContacts) || member( contact minusYContacts)) && ! minext1 && layer1XRightEnclosure < 0.04 && ( layer1YBottomEnclosure < 0.03 || layer1YTopEnclosure < 0.03) layer1XRightEnclosure=0.04)
          if(! minext1 && layer1XRightEnclosure < 0.04 && ( layer1YBottomEnclosure < 0.03 || layer1YTopEnclosure < 0.03) layer1XRightEnclosure=0.04)
          ; update the contact: note, only origin and M6
          contact~>xy = x:y+used_dy
          dbReplaceProp( contact "layer1YDefOverride" "float" layer1YTopEnclosure) ; top
          dbReplaceProp( contact "layer1YEnclosure" "float" layer1YBottomEnclosure) ; bottom
          dbReplaceProp( contact "layer1XDefOverride" "float" layer1XLeftEnclosure) ; left
          dbReplaceProp( contact "layer1XEnclosure" "float" layer1XRightEnclosure) ; right
          dbReplaceProp( contact "layer1Direction" "string" "br")
;          if(x==912.42 && y==-124.44 printf("%s dy=%f x=%f y=%f o=%L l=%f r=%f t=%f b=%f\n" contact~>cellName
;              used_dy car(contact~>xy) cadr(contact~>xy) contact~>orient
;              layer1XLeftEnclosure, layer1XRightEnclosure
;              layer1YTopEnclosure layer1YBottomEnclosure))

        return( t )
        )
        ( contact~>orient == "R90"
          ; R90: rotates CCW 90deg (single cuts)
          ;        R
          ;     T     B
          ;        L
          minext1=nil
          minext2=nil
          touchext1=nil
          if(layer1YTopEnclosure==0.005 && layer1XRightEnclosure==0.005 &&
            layer1YBottomEnclosure==0.005 && layer1XLeftEnclosure==0.005 then
;                if(dy_M7 > 0.006 used_dy=0.005 used_dy=0)
;                if(dy_M7 < -0.006 used_dy=-0.005 used_dy=0)
                if(contact~>pathWidth != nil then
                    ; if pathWidth defined, the there is a m6 path under the contact
                    ;if(px printf("%.3f %.3f %.3f %.0f\n" contact~>pathWidth contact~>XOffset contact~>YOverlap contact~>Type))
                    if(contact~>XOffset == 0.0 then
                        ; if the offset is 0, then the contact is horizontally aligned to the path
                        ; then we make the Left/Right overlap match the path width, minimizes DRCs at the ends
                        layer1YTopEnclosure=(contact~>pathWidth-0.1)/2
                        layer1YBottomEnclosure=(contact~>pathWidth-0.1)/2
                        ; if the overlap seems to fix DRC, the adjust contact to match original overlap
                        ; top side
                        if(contact~>YOverlap-0.05 >= 0.04
                            layer1XRightEnclosure=contact~>YOverlap-0.05
                            layer1XRightEnclosure=0.04
                        )
                        ; bottom side
                        if(contact~>YOverlap+0.05 <= -0.04
                            layer1XLeftEnclosure=-contact~>YOverlap-0.05
                            layer1XLeftEnclosure=0.04
                        )
                        touchext1=t
                        ;if(px printf("layer1XRightEnclosure=%.3f layer1XLeftEnclosure=%.3f\n" layer1XRightEnclosure layer1XLeftEnclosure))
                    )
                else
                    ; found that single contacts frequently need to meet minimum area if they do not
                    ; overlap a path, this guarantees min area
                    layer1YTopEnclosure=0.01
                    layer1YBottomEnclosure=0.01
                    layer1XRightEnclosure=0.07
                    layer1XLeftEnclosure=0.07
                )
                minext1=t
            )
          if(! minext1 then
            ; never let the overlap left/right be less than 0.01 if it is not a min overlap contact
            if(layer1YTopEnclosure < 0.01 layer1YTopEnclosure=0.01)
            if(layer1YBottomEnclosure < 0.01 layer1YBottomEnclosure=0.01)
          )
          ; move top of M6, if not already set above
          if( ! touchext1 layer1XRightEnclosure=layer1XRightEnclosure-used_dy)
          ; avoid negatives
          if(layer1XRightEnclosure < 0.0 then layer1XRightEnclosure=0.0)
          if(! minext1 && layer1XRightEnclosure < 0.04 && ( layer1YTopEnclosure < 0.03 || layer1YBottomEnclosure < 0.03) layer1XRightEnclosure=0.04)
          ; move bottom of M6, if not already set above
          if(! touchext1 layer1XLeftEnclosure=layer1XLeftEnclosure+used_dy)
          ; avoid negatives
          if(layer1XLeftEnclosure < 0.0 layer1XLeftEnclosure=0.0)
          if(! minext1 && layer1XLeftEnclosure < 0.04 && ( layer1YTopEnclosure < 0.03 || layer1YBottomEnclosure < 0.03) layer1XLeftEnclosure=0.04)
          ; update the contact: note, only origin and M6
          contact~>xy = x:y+used_dy
          dbReplaceProp( contact "layer1YDefOverride" "float" layer1YTopEnclosure) ; top
          dbReplaceProp( contact "layer1YEnclosure" "float" layer1YBottomEnclosure) ; bottom
          dbReplaceProp( contact "layer1XDefOverride" "float" layer1XLeftEnclosure) ; left
          dbReplaceProp( contact "layer1XEnclosure" "float" layer1XRightEnclosure) ; right
          dbReplaceProp( contact "layer1Direction" "string" "br")

        return( t )
        )
        ( contact~>orient == "MX"
          ; MX
          ;     B
          ;  L     R
          ;     T
          minext1=nil
          minext2=nil
          touchext1=nil
          if(layer1XLeftEnclosure==0.005 && layer1YBottomEnclosure==0.005 &&
            layer1XRightEnclosure==0.005 && layer1YTopEnclosure==0.005 then
;                if(dy_M7 > 0.006 used_dy=0.005 used_dy=0)
;                if(dy_M7 < -0.006 used_dy=-0.005 used_dy=0)
                if(contact~>pathWidth != nil then
                    ; if pathWidth defined, the there is a m6 path under the contact
                    ;if(px printf("%.3f %.3f %.3f %.0f\n" contact~>pathWidth contact~>XOffset contact~>YOverlap contact~>Type))
                    if(contact~>XOffset == 0.0 then
                        ; if the offset is 0, then the contact is horizontally aligned to the path
                        ; then we make the Left/Right overlap match the path width, minimizes DRCs at the ends
                        layer1XLeftEnclosure=(contact~>pathWidth-0.1)/2
                        layer1XRightEnclosure=(contact~>pathWidth-0.1)/2
                        ; if the overlap seems to fix DRC, the adjust contact to match original overlap
                        ; top side
                        if(contact~>YOverlap-0.05 >= 0.04
                            layer1YBottomEnclosure=contact~>YOverlap-0.05
                            layer1YBottomEnclosure=0.04
                        )
                        ; bottom side
                        if(contact~>YOverlap+0.05 <= -0.04
                            layer1YTopEnclosure=-contact~>YOverlap-0.05
                            layer1YTopEnclosure=0.04
                        )
                        touchext1=t
                        ;if(px printf("layer1YBottomEnclosure=%.3f layer1YTopEnclosure=%.3f\n" layer1YBottomEnclosure layer1YTopEnclosure))
                    )
                else
                    ; found that single contacts frequently need to meet minimum area if they do not
                    ; overlap a path, this guarantees min area
                    layer1XLeftEnclosure=0.01
                    layer1XRightEnclosure=0.01
                    layer1YBottomEnclosure=0.07
                    layer1YTopEnclosure=0.07
                )
                minext1=t
            )
          if(! minext1 then
            ; never let the overlap left/right be less than 0.01 if it is not a min overlap contact
            if(layer1XLeftEnclosure < 0.01 layer1XLeftEnclosure=0.01)
            if(layer1XRightEnclosure < 0.01 layer1XRightEnclosure=0.01)
          )
          ; move top of M6, if not already set above
          if( ! touchext1 layer1YBottomEnclosure=layer1YBottomEnclosure-used_dy)
          ; avoid negatives
          if(layer1YBottomEnclosure < 0.0 then layer1YBottomEnclosure=0.0)
          if(! minext1 && layer1YBottomEnclosure < 0.04 && ( layer1XLeftEnclosure < 0.03 || layer1XRightEnclosure < 0.03) layer1YBottomEnclosure=0.04)
          ; move bottom of M6, if not already set above
          if(! touchext1 layer1YTopEnclosure=layer1YTopEnclosure+used_dy)
          ; avoid negatives
          if(layer1YTopEnclosure < 0.0 layer1YTopEnclosure=0.0)
          if(! minext1 && layer1YTopEnclosure < 0.04 && ( layer1XLeftEnclosure < 0.03 || layer1XRightEnclosure < 0.03) layer1YTopEnclosure=0.04)
          ;
          contact~>xy = x:y+used_dy
          dbReplaceProp( contact "layer1YDefOverride" "float" layer1YTopEnclosure) ; top
          dbReplaceProp( contact "layer1YEnclosure" "float" layer1YBottomEnclosure) ; bottom
          dbReplaceProp( contact "layer1XDefOverride" "float" layer1XLeftEnclosure) ; left
          dbReplaceProp( contact "layer1XEnclosure" "float" layer1XRightEnclosure) ; right
          dbReplaceProp( contact "layer1Direction" "string" "br")

        return( t )
        )
        ( contact~>orient == "MY"
          ; MY
          ;     T
          ;   R   L
          ;     B
          minext1=nil
          minext2=nil
          touchext1=nil
          if(layer1XRightEnclosure==0.005 && layer1YTopEnclosure==0.005 &&
            layer1XLeftEnclosure==0.005 && layer1YBottomEnclosure==0.005 then
;                if(dy_M7 > 0.006 used_dy=0.005 used_dy=0)
;                if(dy_M7 < -0.006 used_dy=-0.005 used_dy=0)
                if(contact~>pathWidth != nil then
                    ; if pathWidth defined, the there is a m6 path under the contact
                    ;if(px printf("%.3f %.3f %.3f %.0f\n" contact~>pathWidth contact~>XOffset contact~>YOverlap contact~>Type))
                    if(contact~>XOffset == 0.0 then
                        ; if the offset is 0, then the contact is horizontally aligned to the path
                        ; then we make the Left/Right overlap match the path width, minimizes DRCs at the ends
                        layer1XRightEnclosure=(contact~>pathWidth-0.1)/2
                        layer1XLeftEnclosure=(contact~>pathWidth-0.1)/2
                        ; if the overlap seems to fix DRC, the adjust contact to match original overlap
                        ; top side
                        if(contact~>YOverlap-0.05 >= 0.04
                            layer1YTopEnclosure=contact~>YOverlap-0.05
                            layer1YTopEnclosure=0.04
                        )
                        ; bottom side
                        if(contact~>YOverlap+0.05 <= -0.04
                            layer1YBottomEnclosure=-contact~>YOverlap-0.05
                            layer1YBottomEnclosure=0.04
                        )
                        touchext1=t
                        ;if(px printf("layer1YTopEnclosure=%.3f layer1YBottomEnclosure=%.3f\n" layer1YTopEnclosure layer1YBottomEnclosure))
                    )
                else
                    ; found that single contacts frequently need to meet minimum area if they do not
                    ; overlap a path, this guarantees min area
                    layer1XRightEnclosure=0.01
                    layer1XLeftEnclosure=0.01
                    layer1YTopEnclosure=0.07
                    layer1YBottomEnclosure=0.07
                )
                minext1=t
            )
          if(! minext1 then
            ; never let the overlap left/right be less than 0.01 if it is not a min overlap contact
            if(layer1XRightEnclosure < 0.01 layer1XRightEnclosure=0.01)
            if(layer1XLeftEnclosure < 0.01 layer1XLeftEnclosure=0.01)
          )
          ; move top of M6, if not already set above
          if( ! touchext1 layer1YTopEnclosure=layer1YTopEnclosure-used_dy)
          ; avoid negatives
          if(layer1YTopEnclosure < 0.0 then layer1YTopEnclosure=0.0)
          if(! minext1 && layer1YTopEnclosure < 0.04 && ( layer1XRightEnclosure < 0.03 || layer1XLeftEnclosure < 0.03) layer1YTopEnclosure=0.04)
          ; move bottom of M6, if not already set above
          if(! touchext1 layer1YBottomEnclosure=layer1YBottomEnclosure+used_dy)
          ; avoid negatives
          if(layer1YBottomEnclosure < 0.0 layer1YBottomEnclosure=0.0)
          if(! minext1 && layer1YBottomEnclosure < 0.04 && ( layer1XRightEnclosure < 0.03 || layer1XLeftEnclosure < 0.03) layer1YBottomEnclosure=0.04)

          ; update the contact: note, only origin and M6
          contact~>xy = x:y+used_dy
          dbReplaceProp( contact "layer1YDefOverride" "float" layer1YTopEnclosure) ; top
          dbReplaceProp( contact "layer1YEnclosure" "float" layer1YBottomEnclosure) ; bottom
          dbReplaceProp( contact "layer1XDefOverride" "float" layer1XLeftEnclosure) ; left
          dbReplaceProp( contact "layer1XEnclosure" "float" layer1XRightEnclosure) ; right
          dbReplaceProp( contact "layer1Direction" "string" "br")

        return( t )
        )
        ( contact~>orient == "R180"
          ; R180, this is model for all rotations
          ;      B
          ;   R     L
          ;      T
          minext1=nil
          minext2=nil
          touchext1=nil
          if(layer1XRightEnclosure==0.005 && layer1YBottomEnclosure==0.005 &&
            layer1XLeftEnclosure==0.005 && layer1YTopEnclosure==0.005 then
;                if(dy_M7 > 0.006 used_dy=0.005 used_dy=0)
;                if(dy_M7 < -0.006 used_dy=-0.005 used_dy=0)
                if(contact~>pathWidth != nil then
                    ; if pathWidth defined, the there is a m6 path under the contact
                    if(contact~>XOffset == 0.0 then
                        ; if the offset is 0, then the contact is horizontally aligned to the path
                        ; then we make the Right/Left overlap match the path width, minimizes DRCs at the ends
                        layer1XRightEnclosure=(contact~>pathWidth-0.1)/2
                        layer1XLeftEnclosure=(contact~>pathWidth-0.1)/2
                        ; if the overlap seems to fix DRC, the adjust contact to match original overlap
                        ; top side
                        if(contact~>YOverlap-0.05 >= 0.04
                            layer1YBottomEnclosure=contact~>YOverlap-0.05
                            layer1YBottomEnclosure=0.04
                        )
                        ; Bottom side
                        if(contact~>YOverlap+0.05 <= -0.04
                            layer1YTopEnclosure=-contact~>YOverlap-0.05
                            layer1YTopEnclosure=0.04
                        )
                        touchext1=t
                    )
                else
                    ; found that single contacts frequently need to meet minimum area if they do not
                    ; overlap a path, this guarantees min area
                    layer1XRightEnclosure=0.01
                    layer1XLeftEnclosure=0.01
                    layer1YBottomEnclosure=0.07
                    layer1YTopEnclosure=0.07
                )
                minext1=t
            )
          if(! minext1 then
            ; never let the overlap left/Right be less than 0.01 if it is not a min overlap contact
            if(layer1XRightEnclosure < 0.01 layer1XRightEnclosure=0.01)
            if(layer1XLeftEnclosure < 0.01 layer1XLeftEnclosure=0.01)
          )
          ; move top of M6, if not already set above
          if( ! touchext1 layer1YBottomEnclosure=layer1YBottomEnclosure-used_dy)
          ; avoid negatives
          if(layer1YBottomEnclosure < 0.0 then layer1YBottomEnclosure=0.0)
          if(! minext1 && layer1YBottomEnclosure < 0.04 && ( layer1XRightEnclosure < 0.03 || layer1XLeftEnclosure < 0.03) layer1YBottomEnclosure=0.04)
          ; move Bottom of M6, if not already set above
          if(! touchext1 layer1YTopEnclosure=layer1YTopEnclosure+used_dy)
          ; avoid negatives
          if(layer1YTopEnclosure < 0.0 layer1YTopEnclosure=0.0)
          if(! minext1 && layer1YTopEnclosure < 0.04 && ( layer1XRightEnclosure < 0.03 || layer1XLeftEnclosure < 0.03) layer1YTopEnclosure=0.04)
          ; move the thing
          contact~>xy = x:y+used_dy
          ; set extensions
          dbReplaceProp( contact "layer1YDefOverride" "float" layer1YTopEnclosure) ; top
          dbReplaceProp( contact "layer1YEnclosure" "float" layer1YBottomEnclosure) ; bottom
          dbReplaceProp( contact "layer1XDefOverride" "float" layer1XLeftEnclosure) ; left
          dbReplaceProp( contact "layer1XEnclosure" "float" layer1XRightEnclosure) ; right
          dbReplaceProp( contact "layer1Direction" "string" "br")
          ; obviously this never happens here, but it here for a place holder
          if(used_dy != dy_M7 then
            dbReplaceProp( contact "layer2YDefOverride" "float" layer2YTopEnclosure) ; top
            dbReplaceProp( contact "layer2YEnclosure" "float" layer2YBottomEnclosure) ; bottom
            dbReplaceProp( contact "layer2XDefOverride" "float" layer2XLeftEnclosure) ; left
            dbReplaceProp( contact "layer2XEnclosure" "float" layer2XRightEnclosure) ; right
            dbReplaceProp( contact "layer2Direction" "string" "br")
            )

        return( t )
        )
        (t printf("Warning: Unsupported orientation %s in contact %s at %.3f:%.3f\n" contact~>orient contact~>cellName x y))
      )
    )
         
    ; M7_M6_2CUT_V is the only via that is capable to create spacing drc spacing error. 
    ; I would suggest do assura run and replace bad ones with single cut.
    if( contact~>cellName=="M7_M6_2CUT_V" then 
       if( layer1YTopEnclosure<0.04 then layer1YTopEnclosure=0.04)
       if( layer1YBottomEnclosure<0.04 then layer1YBottomEnclosure=0.04)
       if( layer2YTopEnclosure<0.04 then layer2YTopEnclosure=0.04)
       if( layer2YBottomEnclosure<0.04 then layer2YBottomEnclosure=0.04)
    )
    if( contact~>cellName=="M7_M6_2CUT_H" then 
       if( layer1XLeftEnclosure<0.04 then layer1XLeftEnclosure=0.04)
       if( layer1XRightEnclosure<0.04 then layer1XRightEnclosure=0.04)
       if( layer2XLeftEnclosure<0.04 then layer2XLeftEnclosure=0.04)
       if( layer2XRightEnclosure<0.04 then layer2XRightEnclosure=0.04)
    )

    if( member( contact minXContacts ) then
      (cond
        (contact~>orient == "R0" || contact~>orient == "R180" 
          if(layer2XLeftEnclosure>0.04 layer2XLeftEnclosure=0.04)
          if(layer2XRightEnclosure>0.04 layer2XRightEnclosure=0.04))
        (contact~>orient == "R90" || contact~>orient == "R270" 
          if(layer2YTopEnclosure>0.04 layer2YTopEnclosure=0.04)
          if(layer2YBottomEnclosure>0.04 layer2YBottomEnclosure=0.04))
      )
    )

;    has not been tried all that much, and was not 100% successful:
;    do not allow a 'shift' if the contact is within 0.525 microns of a power grid
;    shiftDistance=525
;    actualDistance=(mod (mod (round ( y+used_dy )*1000) 2880)+2880 2880)
;    if(actualDistance <= shiftDistance && actualDistance > shiftDistance-220 used_dy=used_dy-0.01)
;    if(2880-actualDistance <= shiftDistance && 2880-actualDistance > shiftDistance-220 used_dy=used_dy+0.01)
    ; the following are ALL two cut contacts
    (cond
      (contact~>orient == "R270"
        ; left is top, top is right, right is bottom, bottom is left (double vias)

        layer2XLeftEnclosure = layer2XLeftEnclosure - used_dy + dy_M7 
        if(layer2XLeftEnclosure < 0.04 && layer2YBottomEnclosure < 0.03 layer2XLeftEnclosure=0.04)
        if(layer2XLeftEnclosure < 0.0 layer2XLeftEnclosure=0.0)
        layer2XRightEnclosure = layer2XRightEnclosure + used_dy - dy_M7 
        if(layer2XRightEnclosure < 0.04 && layer2YBottomEnclosure < 0.03 layer2XRightEnclosure=0.04)
        if(layer2XRightEnclosure < 0.0 layer2XRightEnclosure=0.0)

        if( contact~>cellName=="M7_M6_2CUT_H" then 
           if( layer2XLeftEnclosure>0.04 then
            used_dy=used_dy+layer2XLeftEnclosure-0.04
            layer2XLeftEnclosure=0.04
           )
           if( layer2XRightEnclosure>0.04 then
            used_dy=used_dy-layer2XRightEnclosure+0.04
            layer2XRightEnclosure=0.04
           )
        )
        ; left is top side
        layer1XLeftEnclosure = layer1XLeftEnclosure-used_dy
        if(layer1XLeftEnclosure < 0.0 layer1XLeftEnclosure=0.0)
        if(layer1XLeftEnclosure < 0.04 && (layer1YBottomEnclosure < 0.03 || layer1YTopEnclosure < 0.03) layer1XLeftEnclosure=0.04)
        ; right is bottom side
        layer1XRightEnclosure = layer1XRightEnclosure+used_dy 
        if(layer1XRightEnclosure < 0.0 layer1XRightEnclosure=0.0)
        if(layer1XRightEnclosure < 0.04 && (layer1YBottomEnclosure < 0.03 || layer1YTopEnclosure < 0.03) layer1XRightEnclosure=0.04)

        ; do not put too much extension if there is M6 already there (was needed to fix AREA)
        if(member( contact plusYContacts) || member( contact minusYContacts) then
            if(layer1XRightEnclosure > 0.04 layer1XRightEnclosure=0.04)
            if(layer1XLeftEnclosure > 0.04 layer1XLeftEnclosure=0.04)
        )
        ; if there seems to be room on M6, tweek the contact
        (cond
          (member( contact plusYContacts) && contact~>cellName=="M7_M6_2CUT_H" && track > 2 used_dy=used_dy+0.005)
          (member( contact minusYContacts) && contact~>cellName=="M7_M6_2CUT_H" && track < 9 used_dy=used_dy-0.005)
;          (member( contact plusYContacts) && contact~>cellName=="M7_M6_2CUT_H" && track > 2 printf("Plus %L %.3f %d\n" contact~>xy used_dy track) used_dy=used_dy+0.005)
;          (member( contact minusYContacts) && contact~>cellName=="M7_M6_2CUT_H" && track < 9 printf("Minus %L %.3f %d\n" contact~>xy used_dy track) used_dy=used_dy-0.005)
;          (contact~>cellName=="M7_M6_2CUT_H" printf("No change %L %.3f %d\n" contact~>xy used_dy track))
        )
      )
      (contact~>orient == "R0"
        layer2YTopEnclosure = layer2YTopEnclosure - used_dy + dy_M7 
        if(layer2YTopEnclosure < 0.04 && layer2XLeftEnclosure < 0.03 layer2YTopEnclosure=0.04)
        if(layer2YTopEnclosure < 0.0 layer2YTopEnclosure=0.0)
        layer2YBottomEnclosure = layer2YBottomEnclosure + used_dy - dy_M7 
        if(layer2YBottomEnclosure < 0.04 && layer2XLeftEnclosure < 0.03 layer2YBottomEnclosure=0.04)
        if(layer2YBottomEnclosure < 0.0 layer2YBottomEnclosure=0.0)

        if( contact~>cellName=="M7_M6_2CUT_V" then 
           if( layer2YTopEnclosure>0.04 then
            used_dy=used_dy+layer2YTopEnclosure-0.04
            layer2YTopEnclosure=0.04
           )
           if( layer2YBottomEnclosure>0.04 then
            used_dy=used_dy-layer2YBottomEnclosure+0.04
            layer2YBottomEnclosure=0.04
           )
        )
        ; Top side
        layer1YTopEnclosure = layer1YTopEnclosure-used_dy
        if(layer1YTopEnclosure < 0.0 layer1YTopEnclosure=0.0)
        if(layer1YTopEnclosure < 0.04 && (layer1XLeftEnclosure < 0.03 || layer1XRightEnclosure < 0.03) layer1YTopEnclosure=0.04)
        ; Bottom side
        layer1YBottomEnclosure = layer1YBottomEnclosure+used_dy 
        if(layer1YBottomEnclosure < 0.0 layer1YBottomEnclosure=0.0)
        if(layer1YBottomEnclosure < 0.04 && (layer1XLeftEnclosure < 0.03 || layer1XRightEnclosure < 0.03) layer1YBottomEnclosure=0.04)

        ; do not put too much extension if there is M6 already there (was needed to fix AREA)
        if(member( contact plusYContacts) || member( contact minusYContacts) then
            if(layer1YBottomEnclosure > 0.04 layer1YBottomEnclosure=0.04)
            if(layer1YTopEnclosure > 0.04 layer1YTopEnclosure=0.04)
        )
        ; if there seems to be room on M6, tweek the contact
        (cond
          (member( contact plusYContacts) && contact~>cellName=="M7_M6_2CUT_V" && track > 2 used_dy=used_dy+0.005)
          (member( contact minusYContacts) && contact~>cellName=="M7_M6_2CUT_V" && track < 9 used_dy=used_dy-0.005)
;          (member( contact plusYContacts) && contact~>cellName=="M7_M6_2CUT_H" && track > 2 printf("Plus %L %.3f %d\n" contact~>xy used_dy track) used_dy=used_dy+0.005)
;          (member( contact minusYContacts) && contact~>cellName=="M7_M6_2CUT_H" && track < 9 printf("Minus %L %.3f %d\n" contact~>xy used_dy track) used_dy=used_dy-0.005)
;          (contact~>cellName=="M7_M6_2CUT_H" printf("No change %L %.3f %d\n" contact~>xy used_dy track))
        )
;          if(x == 9.84 && y == 399.78 printf("1 %s dy=%L x=%L y=%f o=%L l=%L r=%L t=%L b=%L\n" contact~>cellName
;              used_dy car(contact~>xy) cadr(contact~>xy) contact~>orient
;              layer1YTopEnclosure layer1YBottomEnclosure
;              layer1XRightEnclosure layer1XLeftEnclosure))
      )
      (t printf("Warning: Unsupported orientation %s in contact %s at %.3f:%.3f\n" contact~>orient contact~>cellName x y))
    )

;          if(x == 1105.08 && y == -437.28 printf("3 %s dy=%f x=%f y=%f o=%L l=%f r=%f t=%f b=%f\n" contact~>cellName
;              used_dy car(contact~>xy) cadr(contact~>xy) contact~>orient
;              layer2XLeftEnclosure, layer2XRightEnclosure
;              layer2YTopEnclosure layer2YBottomEnclosure))
    contact~>xy = x:y+used_dy
    dbReplaceProp( contact "layer1YDefOverride" "float" layer1YTopEnclosure) ; top
    dbReplaceProp( contact "layer1YEnclosure" "float" layer1YBottomEnclosure) ; bottom
    dbReplaceProp( contact "layer1XDefOverride" "float" layer1XLeftEnclosure) ; left
    dbReplaceProp( contact "layer1XEnclosure" "float" layer1XRightEnclosure) ; right
    dbReplaceProp( contact "layer1Direction" "string" "br")
;
    dbReplaceProp( contact "layer2YDefOverride" "float" layer2YTopEnclosure) ; top
    dbReplaceProp( contact "layer2YEnclosure" "float" layer2YBottomEnclosure) ; bottom
    dbReplaceProp( contact "layer2XDefOverride" "float" layer2XLeftEnclosure) ; left
    dbReplaceProp( contact "layer2XEnclosure" "float" layer2XRightEnclosure) ; right
    dbReplaceProp( contact "layer2Direction" "string" "br")

    return( t )
  )
)

; create distorted views from layout
; if the layout_pg exists, distort it, replace its subcells, 
; then copy to distort view and delete the grid.
; if only layout exists, just distort that and replace its subells.
; viewname is the output view name and/or output_pg prefix
; layer is the layer to be shifted. For Alta it is ALWAYS M7
; verbose is not all that effective, but it echos the cell being processed
; shift should always be true now. It shifts the double contacts if M6 says it is ok by 0.005
; layout_only is really for debugging, will distort layout view even if layout_pg exists it will not distort it
(defun DistortCell (cellname @key
    (viewname "distort")
    (layer "M7")
    (verbose nil)
    (shift t)
    (layout_only nil)
    (clean nil)
    (debug nil))
  (let (layoutcv layout_pgcv distortcv distort_pgcv)
    if(debug verbose=t)
    if(verbose printf("%s\n" cellname))
    layoutcv = (nrOpenCellViewReadableByName cellname "layout")
    layout_pgcv=nil
    if(layout_only == nil layout_pgcv = (nrOpenCellViewReadableByName cellname "layout_pg"))
    distortcv = (nrOpenCellViewWritableByName cellname viewname)
    distort_pgcv = nil
    (when !layout_pgcv && verbose
      (printf "No layout_pg view found. Using layout view.\n")
    )
    (when !layoutcv
      (error "No layout view found.\n")
    )
    (if !distortcv then
;      (error (sprintf nil "%s %s view not writable.\n" cellname viewname))
       (printf "%s %s view not writable.\n" cellname viewname)
    else
      (if layout_pgcv != nil then
        distort_pgcv = (nrOpenCellViewWritableByName cellname (strcat viewname "_pg"))
        (if distort_pgcv == nil then
;          (error (sprintf nil "%s %s_pg view not writable.\n" cellname viewname))
          (printf "%s %s_pg view not writable.\n" cellname viewname)
        else
          (dbSave layout_pgcv distort_pgcv)
          (DistortLayer ?CV distort_pgcv ?layer layer ?shift shift ?debug debug)
          (ReplaceSubcells distort_pgcv "layout" viewname ?verbose nil)
          (ReplaceSubcells distort_pgcv "layout_pg" (strcat viewname "_pg") ?verbose nil)
          (DistortAddM78PowerGrid distort_pgcv viewname)
          when( clean DistortClean(distort_pgcv ?verbose verbose ))
          (dbSave distort_pgcv)
          (dbSave distort_pgcv distortcv)
          (DeletePowerGridInstance distortcv)
          (dbSave distortcv) 
        )
      )

      (when layoutcv && !layout_pgcv
        distortcv = (nrOpenCellViewWritableByName cellname viewname)
        (when !distortcv
          (error (sprintf nil "%s view not writable.\n" viewname))
        )
        (dbSave layoutcv distortcv)
        (DistortLayer ?CV distortcv ?layer layer ?shift shift ?debug debug)
        (ReplaceSubcells distortcv "layout" viewname ?verbose nil)
        (ReplaceSubcells distortcv "layout_pg" (strcat viewname "_pg") ?verbose nil )
        (DistortAddM78PowerGrid distortcv viewname)
        when( clean DistortClean(distortcv ?verbose verbose ))
        (dbSave distortcv)
      )
      if(layoutcv dbPurge(layoutcv))
      if(layout_pgcv dbPurge(layout_pgcv))
      if(distortcv dbPurge(distortcv))
      if(distort_pgcv dbPurge(distort_pgcv))
    ) 
    t
  )
)

; just grab the cell name from current window
(defun DistortCV ()
  (DistortCell (geGetWindowCellView)->cellName)
)

; this is to fix a distort_pg view/distort view with m8grid instantiated
(defun DistortPGFix ( cellname @key (verbose t) (viewname "distort_pg") )
   (let (m8grid cv inst dpcv dcv)
      (if verbose printf("%s\n" cellname))
      cv = (nrOpenCellViewReadableByName cellname "layout_pg")
      m8grid = "globals.wires.POWER_GRID_M78"
      if(cv then 
        dpcv = (nrOpenCellViewWritableByName cellname strcat( viewname "_pg") )
        (foreach inst dpcv->instances
          (when inst->objType=="inst" && inst~>cellName == m8grid
            (dbDeleteObject inst)
          )
          (when inst->objType=="mosaicInst" && inst~>master~>cellName == m8grid
            (dbDeleteObject inst~>mosaic)
          )
        )
        dcv = (nrOpenCellViewWritableByName cellname viewname )
        (DistortAddM78PowerGrid  dpcv viewname )
        dbSave(dpcv)
        dbSave(dpcv dcv)
        (DeletePowerGridInstance dcv)
        (dbSave dcv)
        (dbPurge dpcv)
        (dbPurge dcv)
      else
        printf("Cannot open %s %s\n" cellname "layout_pg")
      )
      t
    )
)

; there are two potential cases as I see it. one is the normal flow where
; we have a POWER_GRID_TIEOFF cell. in this case, we will copy that cellview
; to a distorted view of its own and instantiate M78 wherever there is grid 
; reaching up to m7. 
; there are also layout(pg) views that were done by hand and merely instantiate 
; mosaics of power grid tiles. in this case, we will directly instantiate matching 
; mosaics of m78 in the layout(pg) view on top of the old grids. 
(defun DistortAddM78PowerGrid (cv distortviewname)
  (let (gridcv distortgridcv has_pg pginst m7grids m8grid xy orient num rows columns uX uY cellname)
    m7grids = (list "globals.wires.POWER_GRID_M34567"
                  "globals.wires.POWER_GRID_M4567"
                  "globals.wires.POWER_GRID_M567"
                  "globals.wires.POWER_GRID_M67"
                  "globals.wires.POWER_GRID_M34567_FULL"
            )
    m8grid = "globals.wires.POWER_GRID_M78"

    ;find the power grid cell
    has_pg=nil
    (foreach inst cv->instances
      (when (IsPowerGridCell inst)
        pginst = inst
        has_pg=t
        gridcv = (nrOpenCellViewReadableByName inst->cellName "layout")
        distortgridcv = (nrOpenCellViewWritableByName inst->cellName distortviewname)
        (when !distortgridcv
          (error (sprintf nil "Can't open distort cell %s for write.\n" inst->cellName))
        )
        (if !gridcv then
          (printf "Can't open cell %s layout for read, instance view %s\n" inst->cellName inst~>viewName)
          has_pg=nil;
;          dbDeleteObject(pginst)
        else
          (dbSave gridcv distortgridcv)
        )
      )
    )

    ;add m78 to the distort view of the power grid cell and instantiate it
    (when has_pg
       num=0
      ; delete M78 grid, no mosaics in PG?
      (foreach inst setof( i distortgridcv->instances i~>cellName==m8grid)
        (dbDeleteObject inst)
      )
      ; put M78 grid back
      (foreach inst distortgridcv->instances
        (when inst->objType=="inst"
          cellname = inst->cellName
        )
        (when inst->objType=="mosaicInst"
          cellname = inst->master->cellName
        )
        (when (IsInList cellname m7grids)
          (when inst->objType=="inst"
            (dbCreateInstByMasterName distortgridcv "globals" m8grid
              "layout" (sprintf nil "grid_%d" num) inst~>xy inst~>orient)
            num=num+1
          )
          (when inst->objType=="mosaicInst"
            (dbCreateSimpleMosaicByMasterName distortgridcv "globals" m8grid
              "layout" (sprintf nil "grid_%d" num) inst~>mosaic~>xy
                "R0" inst~>mosaic~>rows inst~>mosaic~>columns inst~>mosaic~>uY inst~>mosaic~>uX)
            num=num+1       
          )
        )
      )
      (dbSave distortgridcv)
      (dbClose distortgridcv)
      ;replace the old layout view with distort
      (dbCreateInstByMasterName cv pginst->libName pginst->cellName
        distortviewname (sprintf nil "distort_%s" pginst->name) pginst->xy pginst->orient)
      (dbDeleteObject pginst)
    )

    ;if no grid cell, assume that mosaics are instantiated at the toplevel
    (when !has_pg
      num=0
      (foreach inst setof( i cv->instances i~>cellName==m8grid)
        (if inst->objType=="mosaicInst" then
          (dbDeleteObject inst~>mosaic)
        else
          (dbDeleteObject inst)
        )
      )
      (foreach inst cv->instances
        (when inst->objType=="inst"
          cellname = inst->cellName
        )
        (when inst->objType=="mosaicInst"
          cellname = inst->master->cellName
        )

        (when (IsInList cellname m7grids)
          (when inst->objType=="inst"
            (dbCreateInstByMasterName cv "globals" m8grid
              "layout" (sprintf nil "grid_%d" num) inst~>xy inst~>orient)
            num=num+1
          )
          (when inst->objType=="mosaicInst"
            (dbCreateSimpleMosaicByMasterName cv "globals" m8grid
              "layout" (sprintf nil "grid_%d" num) inst~>mosaic~>xy
                "R0" inst~>mosaic~>rows inst~>mosaic~>columns inst~>mosaic~>uY inst~>mosaic~>uX)
            num=num+1       
          )
        )
      )
    )
  )
)

; this function is a minimal set of stuff from RouteClean
(defun DistortClean ( CellView @key (verbose nil) )
    (let (
          ( DirectiveTable
            ( CellInfoGetTableForCellName
              "null"
              ( sprintf 
                nil
                "%s/share/Fulcrum"
                ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
              ) )

          ( Area
            (when nil
              ( enterBox ?prompts ( list  "Select area to clean" ) ) ) )
          )

      when(verbose printf("DistortClean\n"))
      (cond (
             ( PinUtilFindPRBoundShape BoundaryLPP CellView )
      (let (
            ( MetalLPPs
                        ( append
                              ( list Metal6LPP )
                              ( list Metal7LPP ) ) ) )

        
        ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
        ( NotchesFillNotchesOnCellView CellView

              ( append
                    ( mapcar
                      (lambda ( MetalLPP )
                        ( rexCompile "M" )
                        ( list ( rexReplace ( car MetalLPP ) "area" 0 )
                               MetalLPP ) )
                      MetalLPPs )
                ( append
                      ( mapcar
                        (lambda ( MetalLPP )
                          ( rexCompile "M" )
                          ( list ( rexReplace ( car MetalLPP ) "notch" 0 )  MetalLPP ) )
                        MetalLPPs )
                      ( mapcar
                        (lambda ( MetalLPP )
                          ( rexCompile "M" )
                          ( list ( rexReplace ( car MetalLPP ) "extension" 0 )  MetalLPP ) )
                        MetalLPPs ) ) )

            ( append 
              ( mapcar 
                (lambda ( MetalLPP )
                  ( car MetalLPP ) )
                MetalLPPs )
            ( append
              ( setof 
                Set
                ;( list "AREA" "NOTCH" "EXTENSION" )
                ( list "AREA" )
                ( stringp Set ) )
              ( append
                (if nil ; UIRouteClean_Form->UIRouteClean_Form_OnGrid->value
                    ( list
                      "M6_AREA_KEEPIN"
                      "M7_AREA_KEEPIN" ) )
                (if  ( equal 
                       ( VersionGetMajorVersion )
                       "4" )
                    ( list "VERSION_4X" ) ) ) ) )

          ( sprintf 
            nil
            "%s/share/Fulcrum/notch/fill_notches.assura.rules.all" 
            ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
          ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
          ?Area Area
          ?NoPCells nil
          )
        ; Why are we deleting Dummy?
        ;( foreach LPP
        ;          ( mapcar
        ;            (lambda ( LPP )
        ;              ( list ( car LPP ) "dummy" ) )
        ;            MetalLPPs )
        ;          ( foreach Shape ( PinUtilGetAllShapesOnLPP CellView LPP )
        ;                    ( dbDeleteObject Shape ) ) )
        ) )
       (
        ( printf "No prBoundary shape!!!\n" ) )

        ) ) )

(defun Distort2CutCell ( cellname viewname @key (verbose nil))
    (let (cv)
        cv = (nrOpenCellViewWritableByName cellname viewname)
        if(cv then
            Distort2Cut( cv ?verbose verbose)
            dbSave(cv)
        else
            printf("Error: Cannot open %s %s for write\n" cellname viewname)
        )
    )
)

; do both at once... Most of the time it seems best to do M6 first
(defun Distort2Cut ( CellView @key (verbose nil))
    DistortMXClean( CellView ?verbose verbose ?layer "M7" ?start t)
    DistortMXClean( CellView ?verbose verbose ?layer "M6" ?start nil)
    ; third pass to pick up remaining un-fixable contacts
    DistortMXClean( CellView ?verbose verbose ?layer "M7" ?start nil)
)

(defun DistortMXClean ( CellView @key
                                    (verbose nil)
                                    (layer "M7") ; we do actually run this on M6 AND M7
                                    (start t))
    (let ( x y twocut height ovelaps ucnt dcnt ccnt fix mincontact)
        twocut=(setof contact CellView~>instances
            (contact~>cellName=="M7_M6_2CUT_H" && member(contact~>orient list("R90" "R270" "MXR90" "MYR90"))) ||
                (contact~>cellName=="M7_M6_2CUT_V" && member(contact~>orient list("R0" "R180" "MX" "MY"))))
        ucnt=0
        dcnt=0
        ccnt=0
        mincontact=nil
        foreach( cut twocut
            if(start==t then
                cut~>Up="F"
                cut~>Dn="F"
            )
            x=car(cut~>xy)
            y=cadr(cut~>xy)
            height=0.38/2 ; two cut height, ignore ACTUAL height... mostly ok due to other changes
            ; selected layer, drawing purpose, top only
            ; check that it is NOT connected above because connected above does not need to move down
            ; use 0.03 instead of 0.01 because sometimes another cut lies slighly offset just below
            overlaps=dbGetOverlaps(CellView list( list( x-0.03 y+height+0.03) list(x+0.03 y+height-0.03)) list(layer "drawing"))
            fix=nil
            if( overlaps == nil then
                if(layer == "M7" then
                    ; check pg only for M7 because it is horizontal
                    (let (grid delta)
                        ; find nearest power grid and see if it is close enough to create a violation
                        grid = round(y/2.88)*2.88
                        delta=abs(grid-y)-0.2-0.19-0.1 ; 0.2=1/2 PG, 0.19=1/2 contact 0.1=spacing
                        if(delta < 0 && y-grid < 0 then
                            ucnt=ucnt+1
                            fix=t
                            if(delta > -0.021 then
                                when( verbose printf("DG %.3f %.3f %s\n" x y cut~>cellName))
                                y=y+delta
                                cut~>xy=list(x y)
                                cut~>Dn="T"
                            )
                        )
                    )
                )
                ; if we moved due to pg, no point in looking for other things
                if(! fix then
                    ; x +- 0.06 to look for shapes not quite aligned
                    overlaps=dbGetTrueOverlaps(CellView list( list( x-0.06 y+height+0.095) list(x+0.06 y+height+0.095)) list(layer "drawing") 1 )
                    foreach( ov overlaps
                        when( verbose printf("D1 %.3f %.3f %s %L %L %L\n" x y cut~>cellName ov~>objType ov~>bBox ov~>lpp))
                        if(! fix ucnt=ucnt+1); count only once
                        fix=t
                    )
                    if(fix then
                        y=y-0.005
                        cut~>xy=list(x y)
                        cut~>Dn="T"
                    )
                    ; do it twice
                    if(fix then
                        fix=nil
                        overlaps=dbGetTrueOverlaps(CellView list( list( x-0.06 y+height+0.095) list(x+0.06 y+height+0.095)) list(layer "drawing") 1 )
                        foreach( ov overlaps
                            when(verbose printf("D2 %.3f %.3f %s %L %L %L\n" x y cut~>cellName ov~>objType ov~>bBox ov~>lpp))
                            fix=t
                        )
                        if(fix then
                            y=y-0.005
                            cut~>xy=list(x y)
                        )
                    )
                    ; do it third time, sometimes have to move 0.015
                    if(fix then
                        fix=nil
                        overlaps=dbGetTrueOverlaps(CellView list( list( x-0.06 y+height+0.095) list(x+0.06 y+height+0.095)) list(layer "drawing") 1 )
                        foreach( ov overlaps
                            when(verbose printf("D3 %.3f %.3f %s %L %L %L\n" x y cut~>cellName ov~>objType ov~>bBox ov~>lpp))
                            fix=t
                        )
                        if(fix then
                            y=y-0.005
                            cut~>xy=list(x y)
                        )
                    )
                )
            )
            ; now do the same in the other direction
            overlaps=dbGetOverlaps(CellView list( list( x-0.03 y-height-0.03) list(x+0.03 y-height+0.03)) list(layer "drawing"))
            fix=nil
            if( overlaps == nil then
                if(layer == "M7" then
                    ; check pg only for M7 because it is horizontal
                    (let (grid delta)
                        ; find nearest power grid and see if it is close enough to create a violation
                        grid = round(y/2.88)*2.88
                        delta=abs(grid-y)-0.2-0.19-0.1 ; 0.2=1/2 PG, 0.19=1/2 contact 0.1=spacing
                        if(delta < 0 && y-grid > 0 then
                            when( verbose printf("UG %.3f %.3f %s\n" x y cut~>cellName))
                            dcnt=dcnt+1
                            fix=t
                            if(delta > -0.021 then
                                y=y-delta
                                cut~>xy=list(x y)
                                cut~>Up="T"
                            )
                        )
                    )
                )
                if( ! fix then
                    overlaps=dbGetTrueOverlaps(CellView list( list( x-0.06 y-height-0.095) list(x+0.06 y-height-0.095)) list(layer "drawing") 1 )
                    foreach( ov overlaps
                        when(verbose printf("U1 %.3f %.3f %s %L %L %L\n" x y cut~>cellName ov~>objType ov~>bBox ov~>lpp))
                        if(! fix dcnt=dcnt+1)
                        fix=t
                    )
                    if(fix then
                        y=y+0.005
                        cut~>xy=list(x y)
                        cut~>Up="T"
                    )
                    ; do it twice
                    ; but there ought to be a way to use the first set of overlap's coordinates to tell how much to shift, the problem
                    ; is that sometimes it is an instance (like another contact) and sometimes it is a shape
                    if( fix then
                        fix=nil
                        overlaps=dbGetTrueOverlaps(CellView list( list( x-0.06 y-height-0.095) list(x+0.06 y-height-0.095)) list(layer "drawing") 1 )
                        foreach( ov overlaps
                            when( verbose printf("U2 %.3f %.3f %s %L %L %L\n" x y cut~>cellName ov~>objType ov~>bBox ov~>lpp))
                            fix=t
                        )
                        if(fix then
                            y=y+0.005
                            cut~>xy=list(x y)
                        )
                    )
                    ; do it third time
                    if( fix then
                        fix=nil
                        overlaps=dbGetTrueOverlaps(CellView list( list( x-0.06 y-height-0.095) list(x+0.06 y-height-0.095)) list(layer "drawing") 1 )
                        foreach( ov overlaps
                            when( verbose printf("U3 %.3f %.3f %s %L %L %L\n" x y cut~>cellName ov~>objType ov~>bBox ov~>lpp))
                            fix=t
                        )
                        if(fix then
                            y=y+0.005
                            cut~>xy=list(x y)
                        )
                    )
                )
            )
            ; if we had to move both ways, this is un-fixable as a 2cut
            if(cut~>Up=="T" && cut~>Dn=="T" then
                if(mincontact == nil mincontact=dbOpenCellViewByType( TechLibName "M7_M6min" "layout" "maskLayout" "r"))
                cut~>master=mincontact
                ; change extensions and align with m7
                if(cut~>OriginalY != nil cut~>xy=(list x cut~>OriginalY))
                cut~>orient="R0" ; the extensions below assume R0
                dbReplaceProp( cut "layer1YDefOverride" "float" 0.05) ; top
                dbReplaceProp( cut "layer1YEnclosure" "float" 0.05) ; bottom
                dbReplaceProp( cut "layer1XDefOverride" "float" 0.01) ; left
                dbReplaceProp( cut "layer1XEnclosure" "float" 0.01) ; right
                dbReplaceProp( cut "layer1Direction" "string" "br")
                dbReplaceProp( cut "layer2YDefOverride" "float" 0.01) ; top
                dbReplaceProp( cut "layer2YEnclosure" "float" 0.01) ; bottom
                dbReplaceProp( cut "layer2XDefOverride" "float" 0.04) ; left
                dbReplaceProp( cut "layer2XEnclosure" "float" 0.04) ; right
                dbReplaceProp( cut "layer2Direction" "string" "br")
                printf("Change Two Cut to One Cut %.3f %.3f %s\n" x y cut~>cellName)
                ccnt=ccnt+1
            )
        )
        when(verbose printf("Count %s UP %d DN %d\n" layer dcnt ucnt))
        when(ccnt > 0 printf("Info: %d Contacts changed to single cut\n" ccnt))
    )
)




(defun MakeDistort
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        )
  (let (layoutCV tiehilo)
    (when CV->viewName=="distort_pg"
          layoutCV = (dbCopyCellView CV CV->libName CV->cellName "distort" nil nil t)
          (when !layoutCV (error "Unable to write layout view\n"))
          tiehilo = (dbFindAnyInstByName layoutCV "distort_tiehilo")
          (when tiehilo (dbDeleteObject tiehilo))
          tiehilo = (dbFindMosaicByName layoutCV "distort_tiehilo")
          (when tiehilo (dbDeleteObject tiehilo))
          (dbSave layoutCV)
          )
    layoutCV
    )
  )
