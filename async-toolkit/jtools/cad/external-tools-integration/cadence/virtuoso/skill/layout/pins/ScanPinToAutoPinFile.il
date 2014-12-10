; Copyright 2008 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( glcFindPinTemplate ( CellName
                           @key
                           (PinGlobalWirePitch DefaultWiringPitch)
                           (PinGlobalWireWidth DefaultWiringWidth)
                           (PinGlobalWireSpacing 0.12)
                           (PowerGridTrackOffset -1) 
                           (AutoPinFileName "autopin.il")
                           (templateViewName "layout")
                         )
  prog((fin cellRootName templateCellName libName BakPinFileName CellView)
    AutoPinTemplateFound=0
    ; AutoPinTemplateFound is a global variable used also in snapFatPins
    BakPinFileName=strcat(AutoPinFileName "_bak")
    fin=infile(BakPinFileName)
    if( fin then close(fin) AutoPinTemplateFound=1 return(nil))
    rexCompile(".[0-9]+$")
    cellRootName=rexReplace(CellName "" 0)
    templateCellName=strcat( cellRootName ".pintemplate" )
    libName=car( NameParseCellName( CellName ))
    CellView=nrOpenCellViewReadable( libName templateCellName templateViewName )
    if( CellView==nil then return(nil))
    system(sprintf(nil "mv -f %s %s" AutoPinFileName BakPinFileName))
    AutoPinTemplateFound=1
    glcScanPinsToAutoPin( CellView 
                   ?PinGlobalWirePitch PinGlobalWirePitch
                   ?PinGlobalWireWidth PinGlobalWireWidth
                   ?PinGlobalWireSpacing PinGlobalWireSpacing
                   ?PowerGridTrackOffset PowerGridTrackOffset 
                   ?AutoPinFileName AutoPinFileName )
  )
)


defun( glcScanPinsToAutoPin ( CellView
                           @key
                           (PinGlobalWirePitch DefaultWiringPitch)
                           (PinGlobalWireWidth DefaultWiringWidth)
                           (PinGlobalWireSpacing 0.12)
                           (PinGlobalWireOffset 0.12)
                           (PowerGridTrackOffset -1) 
                           (AutoPinFileName "autopin.il")
                         )
  prog((pins pin fout prbound x1 x2 y1 y2 fig xMin xMax pinData pinOrient height)
    fout=outfile(AutoPinFileName)
    if( fout==nil then
      printf("Unable to open file %s for write.\n" AutoPinFileName)
      return(nil)
    )
    fprintf( fout "( defvar PinGlobalWirePitch %.4e )\n" PinGlobalWirePitch/UserUnitsPerMeter )
    fprintf( fout "( defvar PinGlobalWireWidth %.4e )\n" PinGlobalWireWidth/UserUnitsPerMeter )
    fprintf( fout "( defvar PinGlobalWireSpacing %.4e )\n" PinGlobalWireSpacing/UserUnitsPerMeter )
    fprintf( fout "( PinPlacePowerGrid \"Vdd\" %.0f %.0f %.4e )\n" 
                     PowerGridTrackOffset+PowerGridPitch/PinGlobalWirePitch 
                     PowerGridPitch/PinGlobalWirePitch*2 
                     PowerGridWireWidth/UserUnitsPerMeter)
    fprintf( fout "( PinPlacePowerGrid \"GND\" %.0f %.0f %.4e )\n" 
                     PowerGridTrackOffset+0.0 
                     PowerGridPitch/PinGlobalWirePitch*2 
                     PowerGridWireWidth/UserUnitsPerMeter)
    prbound=car(setof( lpp CellView~>lpps lpp~>layerName=="prBoundary" && lpp~>purpose=="drawing"))
    xMin=leftEdge(car(prbound~>shapes)~>bBox)
    xMax=rightEdge(car(prbound~>shapes)~>bBox)
    foreach( fig prbound~>shapes      
      if(leftEdge(fig~>bBox)<xMin then xMin=leftEdge(fig~>bBox))
      if(rightEdge(fig~>bBox)>xMax then xMax=rightEdge(fig~>bBox))
    )
    
    pins=setof( pin CellView~>terminals pin~>net~>name!=GNDNetName && pin~>net~>name!=VddNetName)
    foreach( pin pins 
      pinData=makeTable(`pinData nil)
      pinWidth=makeTable(`pinWidth "")
      foreach( fig pin~>pins~>fig
;        printf("%s\n" pin~>net~>name)
        cond(
          (fig~>objType=="rect" || fig~>objType=="polgon"
            bBox=fig~>bBox 
            y1=bottomEdge(bBox)
            y2=topEdge(bBox)
            x1=leftEdge(bBox)
            x2=rightEdge(bBox)
            track=floor((y2-PinGlobalWireOffset)/PinGlobalWirePitch)
            ; metal3 track offset is 0.12
            height=y2-y1
            if( abs(height-PinGlobalWireWidth)<0.001 then 
              pinWidth[track]="" 
            else
              pinWidth[track]=sprintf(nil "%.4e" height/UserUnitsPerMeter)
            )
            cond( 
              (x1-xMin<PinGlobalWirePitch && xMax-x2<PinGlobalWirePitch
                 pinData[track]="PinPlaceHorizontalStrut")
              (x1-xMin<PinGlobalWirePitch 
                 if(pinData[track]=="PinPlaceRight" then 
                   pinData[track]="PinPlaceLeftRightPin"
                 else 
                   pinData[track]="PinPlaceLeft")
              )
              (xMax-x2<PinGlobalWirePitch 
                 if(pinData[track]=="PinPlaceLeft" then 
                   pinData[track]="PinPlaceLeftRightPin"
                 else 
                   pinData[track]="PinPlaceRight")
              )
              (t
                 if(pinData[track]==nil then pinData[track]="none")
              )
            )            
          )
          (t printf("Unknow type %s\n" fig~>objType))
        )
      )
      foreach( track pinData
        if(pinData[track]!="none" then
          fprintf(fout "( %s \"%s\" %d %s )\n" pinData[track] pin~>net~>name track pinWidth[track])
        else
          printf("Error pin %s found.\n" pin~>net~>name)
        )
      )
    )
  close(fout)
  )
)
