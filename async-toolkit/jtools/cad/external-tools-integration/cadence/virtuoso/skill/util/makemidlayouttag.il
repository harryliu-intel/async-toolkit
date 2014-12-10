; skill to do layout_tag cell
; AAG
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/util/makelayouttag.il#5 $
; $DateTime: 2012/04/26 09:34:35 $

(defun MakeMidLayoutTag ( CellView @key (orientation "MXR90") (TargetViewName "layout_tag") )
    let( (point trans inst rcv box x y valuex1 valueex1 valuey1 valueey1 valueyy1 valueeyy1 valuex2 valueex2 valuey2 valueey2 valueyy2 valueeyy2 prbbboxx1 prbbboxy1 prbbboxx2 prbbboxy2 leftx lefttx lefty leftty leftyy lefttyy rightx righttx righty rightty rightyy righttyy numofrows numofcols INST)
        trans=list(0:0 orientation 1.0)
        makePloc( CellView~>libName CellView~>cellName CellView~>viewName sprintf(nil "%s.ploc" CellView~>cellName) )
        rcv=nrOpenCellViewWritable(
            CellView~>libName
            CellView~>cellName
            TargetViewName ?mode "w" )
        if( rcv != nil then
            foreach( label setof( l CellView~>shapes l~>objType=="label" )
                point = dbTransformPoint( label~>xy trans)
                dbCreateLabel(
                    rcv
                    label~>lpp
                    point
                    label~>theLabel
                    "centerCenter"
                    "R0"
                    label~>font label~>height )
                ; interchange x and y
                x = car(point)
                y = cadr(point)
                dbCreateRect(rcv, label~>lpp list(list(x-0.005 y-0.005) list(x+0.005 y+0.005)))
            )
            inst = dbCreateInst( rcv CellView "inst_layout" 0:0 orientation )
            if( inst == nil
                printf( "Cannot create inst\n" ))

	if((GetPrbound(CellView)==nil) then
		println("Error: Missing PrBound in the cellview ")
	else
	prbbboxx1 = caar(GetPrbound(CellView)->bBox)
	prbbboxy1 = cadar(GetPrbound(CellView)->bBox)
	prbbboxx2 = caadr(GetPrbound(CellView)->bBox)
	prbbboxy2 = cadadr(GetPrbound(CellView)->bBox)
	if(negativep(prbbboxx1)==t then
	valueex1 = round((prbbboxx1/3.12)-0.5)
	lefttx = (3.12*valueex1)-3.12
	else if(prbbboxx1==0 then
	lefttx = -3.12
	else
	valueex1 = round((prbbboxx1/3.12)-0.5)
	lefttx = (3.12*valueex1)-3.12))
	if(negativep(prbbboxy1)==t then
	valueey1 = round((prbbboxy1/3.12)-0.5)
	leftty = (3.12*valueey1)-3.12
	valueeyy1 = round((prbbboxy1/8.8)-0.5)
	lefttyy = (8.8*valueeyy1)-8.8
	else if(prbbboxy1==0 then
	leftty = -3.12
	lefttyy = -8.8
	else
	valueey1 = round((prbbboxy1/3.12)-0.5)
	leftty = (3.12*valueey1)-3.12
	valueeyy1 = round((prbbboxy1/8.8)-0.5)
	lefttyy = (8.8*valueeyy1)-8.8))
	if(negativep(prbbboxx2)==t then
	valueex2 = round((prbbboxx2/3.12)+0.5)
	righttx = (3.12*valueex2)+3.12
	else
	valueex2 = round((prbbboxx2/3.12)+0.5)
	righttx = (3.12*valueex2)+3.12)
	if(negativep(prbbboxy2)==t then
	valueey2 = round((prbbboxy2/3.12)+0.5)
	rightty = (3.12*valueey2)+3.12
	valueeyy2 = round((prbbboxy2/8.8)+0.5)
	righttyy = (8.8*valueeyy2)+8.8
	else
	valueey2 = round((prbbboxy2/3.12)+0.5)
	rightty = (3.12*valueey2)+3.12
	valueeyy2 = round((prbbboxy2/8.8)+0.5)
	righttyy = (8.8*valueeyy2)+8.8)

	if(negativep(prbbboxy1)==t then
	valuex1 = round((prbbboxy1/3.12)-0.5)
	leftx = (3.12*valuex1)-3.12
	else if(prbbboxy1==0 then
	leftx = -3.12
	else
	valuex1 = round((prbbboxy1/3.12)+0.5)
	leftx = (3.12*valuex1)-3.12))
	if(negativep(prbbboxx1)==t then
	valuey1 = round((prbbboxx1/3.12)-0.5)
	lefty = (3.12*valuey1)-3.12
	valueyy1 = round((prbbboxx1/8.8)-0.5)
	leftyy = (8.8*valueyy1)-8.8
	else if(prbbboxx1==0 then
	lefty = -3.12
	leftyy = -8.8
	else
	valuey1 = round((prbbboxx1/3.12)+0.5)
	lefty = (3.12*valuey1)-3.12
	valueyy1 = round((prbbboxx1/8.8)+0.5)
	leftyy = (8.8*valueyy1)-8.8))
	if(negativep(prbbboxy2)==t then
	valuex2 = round((prbbboxy2/3.12)-0.5)
	rightx = (3.12*valuex2)+3.12
	else
	valuex2 = round((prbbboxy2/3.12)+0.5)
	rightx = (3.12*valuex2)+3.12)
	if(negativep(prbbboxx2)==t then
	valuey2 = round((prbbboxx2/3.12)-0.5)
	righty = (3.12*valuey2)+3.12
	valueyy2 = round((prbbboxx2/8.8)-0.5)
	rightyy = (8.8*valueyy2)+8.8
	else
	valuey2 = round((prbbboxx2/3.12)+0.5)
	righty = (3.12*valuey2)+3.12
	valueyy2 = round((prbbboxx2/8.8)+0.5)
	rightyy = (8.8*valueyy2)+8.8)
	dbCreateRect(CellView "y1" list(lefttx:leftty righttx:rightty)); adds a y1 shape to the layout view
	numofrows = ceiling((rightx-leftx)/3.12)+3
	numofcols = ceiling((rightyy-leftyy)/8.8)+3
        gridCV = (nrOpenCellViewReadable "globals.avago" "globals.avago.wires.POWER_GRID_M89_CONV" "layout")
	FillTag(rcv ?CV CellView)
          (dbCreateSimpleMosaic rcv gridCV "taggrid" (leftx-3.12):(leftyy-8.8) "R0" numofcols numofrows 8.8 3.12)
	foreach(INST rcv->instances
		when((INST~>name=="taggrid")
		  INST~>uX=3.12
                ))
	)

            dbSave( rcv )
            dbClose( rcv )
        else
            printf( "Cannot open cell\n" )
        )
        t
    )
)

(defun FillTag
  (tagView @key (CV (geGetEditCellView)) ; select CV
        (overhang t) ; should power grid overhang by 1 tile
        )
  (let (Components RComponents type_sub polyCellName
        inst prb PowerGridSize PGS prbObjPoints prb1
        x1box x2box y1box y2box x1 y1 x2 x2 xy
        AssuraLayerMappings AssuraRuleFile AssuraRunLog ErrorStr
        polyCV ReplaceCellNameTable
        count gridName gridCV rows cols leafcell leafbbox 
        filltag XdirGridSize PolyGridSize PolyColSize filltagname LName CName)

    ; generated cell or file names
    filltagname = strcat( CV~>viewName "_" "filltag")
    LName = CV~>libName 
    CName = CV~>cellName
    Components = (parseString CV->cellName ".")
    RComponents = (reverse Components)
    (when (length RComponents)<3 (error "Cell name violates CAST conventions\n"))
    type_sub = (sprintf nil "%s_%s" (cadr RComponents) (car RComponents))
    polyCellName = (CV->cellName)

    ; delete old poly grid instance if it exists
    inst = (dbFindAnyInstByName CV "filltag")
    (when inst (dbDeleteObject inst))

    ; get prBoundary boundary shapes 
	foreach(SHAPE CV->shapes
		when((SHAPE~>layerName=="y1")
    prbObjPoints = SHAPE~>bBox))
    prb1 = (dbCreateRect CV list("prBoundary" "drawing") prbObjPoints)
    copyprb1 = (setof copyprb1 CV~>shapes (car copyprb1->lpp) == "prBoundary"&&(cadr copyprb1->lpp) == "drawing")

    ; checks for prBoundary drawing, if only one exists, if vertices are on 3.12u grid
    (cond ((length copyprb1)==1 (letseq
                ((copyprb (car copyprb1)))
                vertprb = (setof vertprb copyprb->points (CheckOffGrid vertprb))
                vertprbrec = (setof vertprbrec copyprb->bBox (CheckOffGrid vertprbrec))
                (when vertprb (error "Not All prBoundary Vertices Are On 3.12u Grid: %L" vertprb))
                (when vertprbrec (error "Not All prBoundary Vertices Are On 3.12u Grid: %L" vertprbrec))
                prb =  (dbCopyShape copyprb CV )
                prb->lpp=(list "prBoundary" "boundary")))
           (t (error "ERROR: Check prBoundary layer. Either no prBoundary or more than one exists.")))

    ;create prBoundary leaf boxes for fill block
	leafbboxx1 = GetPrbound(CellView)->points
	dbCreatePolygon(CV list("prBoundary" "leaf") leafbboxx1)
 
    ; minimum sized power grid y-axis
    PowerGridSize = 2*PowerGridPitch

    ; minimum sized poly x-axis grid
    PolyGridSize = 2*0.13

    ; number of polyfill columns per powergrid
    OneCol = 1
    TwoCol = 2
    ThreeCol = 3
    PolyColSize = PowerGridSize/PolyGridSize

    ; run layer processing
    AssuraLayerMappings = (list
                           (list "filltag" (list "M7" "block"))
                           )
    AssuraRuleFile = (sprintf nil "%s/share/Fulcrum/cell_automation/%s"
                              (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                              "filltag.rul")
    AssuraRunLog = (sprintf nil "%s/%s.filltag.log"
                            (ConfigFileGetValue TheCDSConfigTable "TEMP")
                            CV->cellName)
    ErrorStr = (AssuraRunAssuraLayerProcessor
                CV CV->libName polyCellName filltagname
                AssuraRuleFile
                (ConfigFileGetValue TheCDSConfigTable "TEMP")
                AssuraLayerMappings
                nil
                ?AssuraRunLog AssuraRunLog
                ?LeaveMess nil 
                )
    polyCV = (nrOpenCellViewWritable CV->libName polyCellName filltagname)

    ; define poly cell names
    ReplaceCellNameTable=makeTable("a" "")
    ReplaceCellNameTable["M7"]="globals.wires.POLY_FILL_S23"

    ; replace block shapes with poly mosaics
    count=0
    (foreach lpp polyCV->lpps
      gridName = ReplaceCellNameTable[lpp->layerName]
      (when gridName!="" && lpp->purpose=="block"
        gridCV = (nrOpenCellViewReadable "globals" gridName "layout")
        (foreach shape lpp->shapes
          x1 = (leftEdge   shape->bBox)
          y1 = (bottomEdge shape->bBox)
          x2 = (rightEdge  shape->bBox)
          y2 = (topEdge    shape->bBox)
          cols = (round (x2-x1)/PolyGridSize)
          rows = (round (y2-y1)/PowerGridSize)
          XdirGridSize = PolyGridSize
          (cond (cols >= ThreeCol (let ()
           	   gridCV = (nrOpenCellViewReadable "globals" "globals.wires.POLY_FILL_S78" "layout")
                   cols = (round cols/ThreeCol)
                   XdirGridSize = 3*PolyGridSize))

                (cols >= ThreeCol (let ()
                   gridCV = (nrOpenCellViewReadable "globals" "globals.wires.POLY_FILL_S78" "layout")
                   cols = (round cols/ThreeCol)
                   XdirGridSize = 3*PolyGridSize))

                (cols >= TwoCol (let ()
                   gridCV = (nrOpenCellViewReadable "globals" "globals.wires.POLY_FILL_S52" "layout")
                   cols = (round cols/TwoCol)
                   XdirGridSize = 2*PolyGridSize))

                (cols >= OneCol (let ()
                   gridCV = (nrOpenCellViewReadable "globals" "globals.wires.POLY_FILL_S26" "layout")
                   cols = (round cols/OneCol)
                   XdirGridSize = PolyGridSize))
           )
           
           (dbCreateSimpleMosaic polyCV gridCV
                                (sprintf nil "I%d" count) x1:y1 "R0"
                                rows cols PowerGridSize XdirGridSize)

          count = count+1
          (dbDeleteObject shape)
          )
        )
      )

    ; copy prBoundary object to poly fill view
    (dbCreatePRBoundary polyCV CV->prBoundary->points)

    ; delete temporary prBoundary shape
    (dbDeleteObject prb)

    ; delete temporary leaf prBoundary shapes 
    leafprb = (setof leafprb (wcv)~>shapes (car leafprb->lpp) == "prBoundary"&&(cadr leafprb->lpp) == "leaf")
    (foreach templeafprb leafprb
       (dbDeleteObject templeafprb))

    ; instantiate poly grid 

	foreach(SHAPE CV->shapes
		when((SHAPE~>layerName=="y1")||(SHAPE~>layerName=="prBoundary")
		  dbDeleteObject(SHAPE)
                ))

	filler = (dbCreateInst tagView polyCV "filltag" 0:0 "MXR90")
    (AnchorInstance filler)
	dbFlattenInst(filler 1)	
	;ddDeleteObj(ddGetObj(LName CName filltagname))		
    ; save and return
    (dbSave polyCV)
    polyCVID = polyCV
   (dbClose polyCV)
    ddDeleteObj(ddGetObj(LName CName filltagname))		
   (dbSave CV)
   (dbClose CV)
   (dbSave tagView)
    polyCVID
    )
  )
(defun MakeMidLayoutTagCell ( CellName @key ( ViewName "layout" ) (orientation "MXR90") (TargetViewName "layout_tag") )
    let( ( cv pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName)
        if( cv == nil
            printf( "Cannot open %s\n" CellName)
            MakeMidLayoutTag( cv ?orientation orientation ?TargetViewName TargetViewName ))
        t
    )
)

(defun getPloc ( cellView instance transform fout)
    (let (sub nv ng xlim ylim tx tm hiername subname orient isProteus isSram isPG x y)
        if(transform == nil transform=list(list( 0 0 ) "R0" 1.0))
        if(instance~>objType == "inst"
            orient=instance~>orient
            orient=car(instance~>tileArray)
        )
        if(instance==nil
            tx=transform
            tx=dbConcatTransform(list(instance~>xy orient 1.0) transform)
        )
        if((cellView~>cellName == "globals.wires.POWER_GRID_M78") then
            ng=1
            nv=1
            for(nx 0 instance~>rows-1
                for(ny 0 instance~>columns-1
                    x=car(instance~>xy)+nx*instance~>uX
                    y=cadr(instance~>xy)+ny*instance~>uY
                    if(nx == instance->rows-1
                        limx=2
                        limx=1
                    )
                    if(ny == instance->columns-1
                        limy=1
                        limy=0
                    )
                    for(ax 0 limx
                    for(ay 0 limy
                        tm=dbConcatTransform(list(list(x+ax*1.56 y+ay*3.12)  orient 1.0) transform)
                        fprintf( fout "GND.extra%03d %.3f %.3f M8 GROUND\n" ng round(car(car(tm))*1000)/1000.0 round(cadr(car(tm))*1000)/1000.0 tm)
                        ng++
                    ))
                    for(ax 0 limx
                    for(ay 0 0
                        tm=dbConcatTransform(list(list(x+ax*1.56 y+ay*3.12+1.56)  orient 1.0) transform)
                        fprintf( fout "Vdd.extra%03d %.3f %.3f M8 POWER\n" nv round(car(car(tm))*1000)/1000.0 round(cadr(car(tm))*1000)/1000.0 tm)
                        nv++
                    ))
                    ))
        )
        foreach(ix setof( ix cellView~>instances t)
            getPloc( ix~>master~>cellView, ix, tx fout)
        )
))

(defun makePloc ( libName cellName viewName fileName )
    (let (cv fout)
        cv=nrOpenCellViewReadable(libName cellName viewName)
        if(cv then
            fout=outfile(fileName)
            if( fout then
                getPloc(cv nil nil fout)
                close(fout);
                system(sprintf(nil "/bin/sort %s -o %s" fileName fileName))
            else
                printf("Cannot open %s\n" fileName))
        else
            printf("Cannot read %s %s %s\n" libName cellName viewName)
        )
    t
    )
)
