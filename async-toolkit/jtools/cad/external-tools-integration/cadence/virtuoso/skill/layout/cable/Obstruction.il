(defun Obstruction ( LibName CellName ViewName ) 
 let( ( SHAPE) 
	CellView=nrOpenCellViewReadable( LibName CellName ViewName)
	foreach(SHAPE CellView->shapes
		(let (shp channelwidth channelhops sbufornt rbufornt channelbuf channelname channellayer cellname cn piecechannelname1 piecechannelname2 piecechannelname3 channame lowercase1 lowercase2 ncv mcv)
		when(SHAPE~>purpose=="boundary"
		dbCreatePath(CellView list("y2" "drawing") SHAPE~>points SHAPE~>width "varExtendExtend")
		channelwidth=SHAPE~>ChannelWidth
		channelhops=SHAPE~>ChannelHops
		sbufornt=SHAPE~>sbuf_orient
		rbufornt=SHAPE~>rbuf_orient
		channelbuf=SHAPE~>ChannelBuf
		channelname=SHAPE~>ChannelName
		channellayer=SHAPE~>layerName
		piecechannelname1=parseString( channelname "[")
		when(length(piecechannelname1)>=2
		piecechannelname2=parseString( nth(1 piecechannelname1) "]")
		lowercase1=lowerCase(nth(0 piecechannelname1))
		channame=strcat(lowercase1 "_" car(piecechannelname2)))
		when(length(piecechannelname1)<2
		channame=lowerCase(channelname))		
		cellname=sprintf( nil "chip.rrc.util.cable.%s_CABLE_MBUFS-L%d_%d-R.%s" channelbuf channelhops channelwidth channame)
		println(cellname)
		dbDeleteObject(SHAPE)
		leLayerAnd(CellView list("y2" "drawing") list("M1" "boundary") list("OBS" "bus"))
		leLayerAnd(CellView list("y2" "drawing") list("M2" "boundary") list("OBS" "bus"))
		leLayerAnd(CellView list("y2" "drawing") list("M3" "boundary") list("OBS" "bus"))
		leLayerAnd(CellView list("y2" "drawing") list("M4" "boundary") list("OBS" "bus"))
		leLayerAnd(CellView list("y2" "drawing") list("M5" "boundary") list("OBS" "bus"))
		leLayerAnd(CellView list("y2" "drawing") list("M6" "boundary") list("OBS" "bus"))
		leLayerAnd(CellView list("y2" "drawing") list("M7" "boundary") list("OBS" "bus"))
		leLayerAnd(CellView list("y2" "drawing") list("M8" "boundary") list("OBS" "bus"))
		leLayerAnd(CellView list("y2" "drawing") list("M9" "boundary") list("OBS" "bus"))
      		ncv = nrOpenCellViewWritable( "chip.rrc.util.cable" cellname "cable")
			when(ncv!=nil
			ddDeleteObj(ddGetObj("chip.rrc.util.cable" cellname "cable"))
				ncv=nil)
		foreach(shp CellView~>shapes
		(let (obj)
		when(shp~>lpp==list("OBS" "bus")
			when(ncv==nil
      				mcv = nrOpenCellViewWritable( "chip.rrc.util.cable" cellname "cable")

		dbCopyShape(shp mcv)
		dbDeleteObject(shp)
		dbSave(mcv)
		dbPurge(mcv)
			))		
		when(shp~>lpp==list("y2" "drawing")
			obj=dbCreatePath(CellView list(channellayer "boundary") shp~>points shp~>width "varExtendExtend")
			(dbReplaceProp obj "ChannelName" "string" channelname)
			(dbReplaceProp obj "ChannelBuf" "string" channelbuf)
			(dbReplaceProp obj "rbuf_orient" "string" rbufornt)
			(dbReplaceProp obj "sbuf_orient" "string" sbufornt)
			(dbReplaceProp obj "ChannelHops" "list" channelhops)
			(dbReplaceProp obj "ChannelWidth" "list" channelwidth)
			dbDeleteObject(shp))))

		)
		))
			))
(defun DrawCableOBS ()
	(let (fl k text lcv kcv libname cellname viewname piece n path)
	lcv=nrOpenCellViewWritable( "chip.rrc.util.cable" "chip.rrc.util.cable.ALL_PATHS.1000" "scratch")
			when(lcv!=nil
			ddDeleteObj(ddGetObj("chip.rrc.util.cable" "chip.rrc.util.cable.ALL_PATHS.1000" "scratch"))
				lcv=nil)
			when(lcv==nil
      				kcv = nrOpenCellViewWritable( "chip.rrc.util.cable" "chip.rrc.util.cable.ALL_PATHS.1000" "scratch")
	fl = infile("path.txt")
	while( gets(k fl) != nil
  	text = parseString( k " \n")
	piece=parseString(car(text) ".")
        libname=nth( 0 piece )
	cellname=car(text)
        for( n 1 length(piece)-3
            libname = strcat( libname "." nth( n piece )))
		kcv = nrOpenCellViewWritable( "chip.rrc.util.cable" "chip.rrc.util.cable.ALL_PATHS.1000" "scratch")
		path=dbCreateInstByMasterName(kcv libname cellname "floorplan" "path" 0.0:0 "R0")
		dbFlattenInst(path 1)
		dbSave(kcv)
		dbPurge(kcv))
		Obstruction( "chip.rrc.util.cable" "chip.rrc.util.cable.ALL_PATHS.1000" "scratch" )
	)
))
