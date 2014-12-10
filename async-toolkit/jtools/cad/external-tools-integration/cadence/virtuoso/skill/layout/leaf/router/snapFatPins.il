; Copyright 2007 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( SnapFatPinsGetClosestTrack ( CellView pin )
  let((wirePitch wireSpacing wirePitchOffset fig x1 x2 y1 y2 height track trackGood yy1 yy2 m3Shapes track_inc)
    wirePitch=DefaultWirePitch*MicronsPerMeter
    wireSpacing=DefaultWireSpacing*MicronsPerMeter
    wirePitchOffset=0.06
    foreach( fig pin~>pins~>fig
      x1=leftEdge(fig~>bBox)
      x2=rightEdge(fig~>bBox)
      y1=bottomEdge(fig~>bBox)
      y2=topEdge(fig~>bBox)
      height=y2-y1
      yy1=y1
      track=floor((y1-wirePitchOffset)/wirePitch)
      track_inc=-1
      trackGood=t
      while( (trackGood && y1-yy1<2.88 && yy1-max(y1 0)<2.88)
        yy1=track*wirePitch+wirePitchOffset
        yy2=yy1+height
        m3Shapes=dbGetTrueOverlaps(CellView 
                        list(list(x1-wireSpacing+0.001 yy1-wireSpacing+0.001)
                             list(x2+wireSpacing-0.001 yy2+wireSpacing-0.001))
                        PinLPP
                        0)
        trackGood=setof( shape m3Shapes shape~>objType!="label" && !member(shape pin~>pins~>fig)) || yy2<0.0
        if(track<=0 then track_inc=1)
        track=track+track_inc      
      )

      if(!trackGood then
        dbMoveFig(fig CellView list( 0.0:yy1-y1 "R0" 1.0 ) )                       
      )
    )
  )
)

defun( SnapFatPins ( CellView )
; find all Fat Pins and those go outside of PrBound and fix them.
  let((pins pin defaultPinWidth)
    pins=setof( pin CellView~>terminals pin~>net~>name!="Vdd" && pin~>net~>name!="GND")
    defaultPinWidth=CellInfoLookupParametrized( DirectiveTable "layer_wirewidth" "layer" PinLPP)*UserUnitsPerMeter 
    pins=setof( pin pins car(pin~>pins~>fig)~>bBox && (topEdge(car(pin~>pins~>fig)~>bBox)-bottomEdge(car(pin~>pins~>fig)~>bBox)>defaultPinWidth+0.001 || topEdge(car(pin~>pins~>fig)~>bBox)<0.0))
    foreach(pin pins
      SnapFatPinsGetClosestTrack( CellView pin )
    )

  )
)
