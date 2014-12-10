; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


  
defun( nrDeleteLayers ( CellView layerLLP )
  let((num_deleted)
    num_deleted=0
    foreach( lpp CellView~>lpps
      if( lpp~>layerName==car(layerLLP)  && lpp~>purpose==cadr(layerLLP) then
         foreach( shape lpp~>shapes 
           dbDeleteObject( shape ) 
           num_deleted=num_deleted+1 )
      )
    ) 
  num_deleted
  )
)

defun( nrDeleteInstances ( CellView libName cellName viewName )
  let((num_deleted)
    num_deleted=0
    foreach( inst CellView~>instances
      if( inst~>libName==libName && inst~>cellName==cellName && inst~>viewName==viewName then
          dbDeleteObject( inst )
          num_deleted=num_deleted+1 
      )
    )
  num_deleted
  )
) 

; does not really belong here, but it does not 'belong' anywhere.
(defun nrGetMaxHeapSize ()
  (cond ((and (boundp 'maxHeapSize) maxHeapSize!=nil) maxHeapSize)
        ((boundp 'UIOptions_Form)
         UIOptions_Form->UIOptions_Form_MaxHeapSize->value)
        (t
         (shell "uname -m > /tmp/uname-machine")
         mnfile = (infile "/tmp/uname-machine")
         (gets machine mnfile)
         (close mnfile)
         (deleteFile "/tmp/uname-machine")
         (if machine=="x86_64\n" 4096 1800)
         )
        )
  )

; this function should be combined with nrGetMaxHeapSize
(defun nrGetJavaArguments ()
  (cond ((and (boundp 'javaArguments) javaArguments!=nil) javaArguments)
        ((boundp 'UIOptions_Form)
          UIOptions_Form->UIOptions_Form_JavaArguments->value)
        (t "")))

; moved from a UI function skill to be allowed in nograph mode
defun( FindInplacedPins (cellView)
  let((powerNetNames InplacedPinsList terminal pinShapes net pin overlapShape obj SetshapeSet )
    powerNetNames=list("Vdd" "GND" "Vss" "gnd")
    InplacedPinsList=nil  
    foreach( terminal cellView~>terminals
      pinShapes=(setof f terminal~>pins~>fig f!=nil) ; why are there nil figs?
      net=terminal~>net
      if( !member( net~>name powerNetNames ) then
        foreach( pin pinShapes
          overlapShapeSet=append( dbGetOverlaps( cellView pin~>bBox list(pin~>layerName "drawing") 1:20 ) 
                           dbGetOverlaps( cellView pin~>bBox list(pin~>layerName "net") 1:20 ) )
          overlapShapeSet=setof( obj overlapShapeSet car(obj)~>libName!="tsmc65" )

          foreach( shapeSet overlapShapeSet
            inst=car(shapeSet)
            shape=cadr(shapeSet)
;            if(shape~>net then printf("Found top leve pin %s overlaping %s\n" net~>name inst~>name))
          )
          if( overlapShapeSet then 
            InplacedPinsList=cons( net~>name InplacedPinsList )            
          )
        )
      )
    )
    if( InplacedPinsList then printf("Found %d inplaced pins \n" length(InplacedPinsList)))
    InplacedPinsList
  )
) 


;
; delete tool summary file
;
defun( nrDeleteToolSum ( LogFileName )
  let( (SumFileName Command)
    SumFileName = substring( LogFileName 1 ( strlen( LogFileName ) - 3 ))
    SumFileName = strcat( SumFileName "sum" )
    Command = sprintf( nil "rm -f %s" SumFileName )
    shell( Command )
    printf( "Delete old summary file %s\n" SumFileName)
  )
)
