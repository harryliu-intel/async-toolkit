; custom skill functions

; return list of array indices from an instance name
(defun GetIndices ( name )
  ind = (list)
  (foreach substring (parseString name "[]")
           (if (sscanf substring "%d" i) == 1 then ind = (append1 ind i))
           )
  ind
  )

; return lower left bounding box over a list of instances
(defun LowerLeftOfInstances (instances)
  (let (x y minX minY)
    (foreach inst instances
             x = (car  inst->xy)
             y = (cadr inst->xy)
             (when minX==nil || x<minX minX=x)
             (when minY==nil || y<minY minY=y)
             )
    (when minX && minY minX:minY)
    )
  )

; Aligns an array of cells using transformation:
; (x,y) = Sum_i {(dx_i, dy_i) * index_i}
; normal     example (ArrayCells (list 4.8:0 0:9.6))
; transposed example (ArrayCells (list 0:9.6 4.8:0))
;
; ArrayCells( list( 15:0 0:38.4 ) ?WildCardString "slk_ex\\[.*\\].mb\\[.*\\]")
;
(defun ArrayCells ( matrix @key ( CV (geGetEditCellView))
                                ( WildCardString nil ) 
                                )
  (let (x y dx dy xDiff yDiff oldXY newXY pcre SelectedCellList name indices)

    ; choose instances to array
    (cond (WildCardString && WildCardString!=""
           pcre = (pcreCompile WildCardString)
           SelectedCellList = (setof x CV->instances (pcreExecute pcre x->name))
           )
          (t
           SelectedCellList = (setof x (geGetSelSet) x->objType=="inst" && x->cellView==CV)
           )
          )

    ; find original lower left xy of cells
    oldXY =  (LowerLeftOfInstances SelectedCellList)

    ; Process instances
    (foreach SelectedCell SelectedCellList 

      ; parse indices from instance name
      name = SelectedCell->name
      indices = (GetIndices name)

      ; get current coordinates
      x = (car  SelectedCell->xy)
      y = (cadr SelectedCell->xy)

      ; compute offset
      xDiff = -x
      yDiff = -y
      (foreach xy matrix
        dx = (car xy)
        dy = (cadr xy)
        index = (car indices)
        indices = (cdr indices)
        (cond ((and index!=nil (atom dx) (atom dy))
               xDiff = xDiff + dx * index
               yDiff = yDiff + dy * index
               )
              ((and index!=nil (nth index xy)!=nil)
               xDiff = xDiff + (car  (nth index xy))
               yDiff = yDiff + (cadr (nth index xy))
               )
              )
        )

      ; move instance
      (dbMoveFig SelectedCell CV (list xDiff:yDiff "R0"))
      )

    ; adjust based on differnce of lower left
    newXY = (LowerLeftOfInstances SelectedCellList)
    (foreach SelectedCell SelectedCellList
      (dbMoveFig SelectedCell CV (list (car oldXY)-(car newXY):(cadr oldXY)-(cadr newXY) "R0"))
      )
    )
  t
  )

; Wraparound GUI
procedure( TR_FloorplanArray( )

        let( (  TR_FloorplanArray 
                TR_FloorplanArray_xOffset
                TR_FloorplanArray_yOffset )

        TR_FloorplanArray_Matrix = hiCreateStringField( 
                ?name 'TR_FloorplanArray_Matrix 
                ?prompt "Matrix List" 
		?defValue "0:2.88*8"
                ?editable t )

        TR_FloorplanArray_WorkOnSelectedField = hiCreateToggleField( 
                ?name 'TR_FloorplanArray_WorkOnSelectedField
                ?prompt "Manual Select" 
		?choices list( list( `WorkOnSelected "Yes" ) )
		?defValue list( t )
                ?callback list( "TR_FloorplanArray_WorkOnSelectedField_CB( hiGetCurrentForm() )" )
                )

        TR_FloorplanArray_WildCardStringField = hiCreateStringField( 
                ?name 'TR_FloorplanArray_WildCardStringField
		?defValue nil
                ?editable nil 
		)

        TR_FloorplanArray_Form = hiCreateAppForm( 
                ?name 'TR_FloorplanArray_Form
                ?fields  list( 	
				list( TR_FloorplanArray_Matrix 200:0 200:30 100 )
				list( TR_FloorplanArray_WorkOnSelectedField 0:30 150:30 100 )
				list( TR_FloorplanArray_WildCardStringField 200:30 200:30 0 )
				)
                ?callback "ArrayCells(  evalstring( sprintf( nil \"list(%s)\" TR_FloorplanArray_Form->TR_FloorplanArray_Matrix->value ))
					?WildCardString TR_FloorplanArray_WildCardStringField->value 
					)" 
        )

        hiDisplayForm( TR_FloorplanArray_Form )

        ); end let

); end procedure

; ===========================================================================
; TR_FloorplanArray_WorkOnSelectedField_CB( hiGetCurrentForm() )
; ===========================================================================
procedure( TR_FloorplanArray_WorkOnSelectedField_CB( theForm )

	if( !theForm->TR_FloorplanArray_WorkOnSelectedField->WorkOnSelected->value
		then
			theForm->TR_FloorplanArray_WildCardStringField->editable = t
			theForm->TR_FloorplanArray_WildCardStringField->value = ""
		else
			theForm->TR_FloorplanArray_WildCardStringField->editable = nil
			theForm->TR_FloorplanArray_WildCardStringField->value = theForm->TR_FloorplanArray_WildCardStringField->defValue
	)			


); end procedure







; handy binds
hiSetBindKey( "Layout"  "<key>KP_4"        "(MoveSelected -0.01 0)")
hiSetBindKey( "Layout"  "<key>KP_6"        "(MoveSelected  0.01 0)")
hiSetBindKey( "Layout"  "<key>KP_2"        "(MoveSelected 0 -0.01)")
hiSetBindKey( "Layout"  "<key>KP_8"        "(MoveSelected 0  0.01)")

hiSetBindKey( "Layout"  "<shift>KP_4"        "(MoveSelected -0.48 0)")
hiSetBindKey( "Layout"  "<shift>KP_6"        "(MoveSelected  0.48 0)")
hiSetBindKey( "Layout"  "<shift>KP_2"        "(MoveSelected 0 -0.48)")
hiSetBindKey( "Layout"  "<shift>KP_8"        "(MoveSelected 0  0.48)")

; Align pcells on minimum grid
(defun AlignPCells ()

  SelectedCellList = geGetSelSet( )
  
  (foreach SelectedCell SelectedCellList 
          
           (if SelectedCell->objType == "inst" then

               xGrid = 0.01
               yGrid = 0.01
               
               xCord = (car SelectedCell -> xy )
               yCord = (cadr SelectedCell -> xy )
               
               ; Move to negative x direction
               xDiff1 = (( xCord / xGrid ) - floor( xCord / xGrid )) * xGrid
               
               ; Move to negative y direction
               yDiff1 = (( yCord / yGrid ) - floor( yCord / yGrid )) * yGrid 
               
               ; Move to positive x direction
               xDiff2 = (( xCord / xGrid ) - floor( ( xCord / xGrid ) + 1 )) * xGrid

               ; Move to positive y direction
               yDiff2 = (( yCord / yGrid ) - floor( ( yCord / yGrid ) + 1 )) * yGrid 

               ; Find the closest
               (if ((abs xDiff1) > (abs xDiff2))
                   then
                 xDiff = xDiff2
                 else
                 xDiff = xDiff1
                 )
               (if ((abs yDiff1) > (abs yDiff2))
                   then
                 yDiff = yDiff2
                 else
                 yDiff = yDiff1
                 )
	
               dbMoveFig( SelectedCell, nil, list(-xDiff:-yDiff "R0"))
               )
           )
  )

; bind it
hiSetBindKey( "Layout"  "<key>KP_Enter" "(AlignPCells)")
