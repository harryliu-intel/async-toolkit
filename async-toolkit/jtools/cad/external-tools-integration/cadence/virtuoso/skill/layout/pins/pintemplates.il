

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Generic power pins function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

procedure( DrawPowerPins( x y layer )
  PinPlace( "GND" list( x:y x+GridWidth/2:y+GridWidth/2 )
	?LPP layer )
  PinPlace( "Vdd" list( x:y+GridPitch/2
			 x+GridWidth/2:y+GridWidth/2+GridPitch/2 )
	?LPP layer )
)

procedure( DrawM3_PowerPins( x y )
  DrawPowerPins( x y Metal3pinLPP )
)
procedure( DrawM5_PowerPins( x y )
  DrawPowerPins( x y Metal5pinLPP )
)
procedure( DrawM7_PowerPins( x y )
  DrawPowerPins( x y Metal7pinLPP )
)



;leaf cell template
; LeafTilePowerTemplate in pdkinfo describes the metals in a
; half-tile, assuming that the next track above mirrors. this allows
; either split- or solid- stripes.
(defun DrawLeafM3Power ( @key (cv (UIGetCellView)) )
 (let (track i x1 x2 template stripe1 stripe2 stripe1y stripe2y
         fig1 fig2)
  (DelLeafM3Power ?cv cv)
  track=0.0
  i=0
  x1=(car (car (GetPrbound cv)->bBox))
  x2=(car (cadr (GetPrbound cv)->bBox))
  (while track < (cadr (cadr (GetPrbound cv)->bBox))
    template=LeafTilePowerTemplate
    
    (if (mod i 2)==0 then
      stripe1=(car template)
      stripe2=(cadr template)
      stripe1y=track+(nth 2 stripe1)
      stripe2y=track+(nth 2 stripe2)
    else
      stripe2=(car template)
      stripe1=(cadr template)
      stripe1y=track+(nth 2 stripe2)
      stripe2y=track+(nth 2 stripe1)
    )

    fig1=(dbCreateRect cv
              (nth 1 stripe1)
              (list x1:stripe1y x2:stripe1y+PowerGridWireWidth) )
    SetLeafM3PowerProp(fig1)
    (dbCreatePin (dbFindNetByName cv (nth 0 stripe1)) fig1)
    fig2=(dbCreateRect cv
              (nth 1 stripe2)
              (list x1:stripe2y x2:stripe2y+PowerGridWireWidth) )
    SetLeafM3PowerProp(fig2)
    (dbCreatePin (dbFindNetByName cv (nth 0 stripe2)) fig2)

    (when i==0
      (dbCreateLabel cv 
              (nth 1 (car template))
              (list x1+0.03 track+(nth 2 (car template))+PowerGridWireWidth/2)
              (nth 0 (car template))
              "centerLeft" "R0" "stick" 0.03)
      (dbCreateLabel cv 
              (nth 1 (cadr template))
              (list x1+0.03 track+(nth 2 (cadr template))+PowerGridWireWidth/2)
              (nth 0 (cadr template))
              "centerLeft" "R0" "stick" 0.03)
    )

    track=track+PowerGridPitch
    i=i+1
  )

 )
)

defun( DelLeafM3Power ( @key (cv (UIGetCellView)) )
  let( (Shape)

   foreach( Shape cv~>shapes

     when( Shape~>LeafM3Power == "TRUE"
        dbDeleteObject(Shape)
      )

     when( ( Shape~>theLabel == "GND" && cadr(Shape~>lpp) == "gnd" ) || ( Shape~>theLabel == "Vdd" && cadr(Shape~>lpp) == "vdd" )
        dbDeleteObject(Shape)
      )
    )

  )
t
)

defun( SetLeafM3PowerProp (Fig)

   dbReplaceProp(Fig "LeafM3Power" "boolean" "TRUE")

t
)
