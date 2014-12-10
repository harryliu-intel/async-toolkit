
(defun fillCell ( CellName ViewName )
    let( ( lcv cv ncv mcv newCellName pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
	when( ((cv == nil)&&(ViewName == "distort"))
	ViewName="layout"
	lcv=nrOpenCellViewReadable( LibName CellName ViewName ))
	when( ((cv == nil)&&(ViewName == "distort_pg"))
	ViewName="layout_pg"
	lcv=nrOpenCellViewReadable( LibName CellName ViewName ))
        newCellName = strcat(CellName "_hvt") 
	mcv=nrOpenCellViewWritable( LibName newCellName ViewName)
        when( cv == nil
            printf( "Cannot open distort/distort_pg view for %s\n" CellName)
            )
	when( lcv == nil
            printf( "Cannot open layout/layout_pg view for %s\n" CellName)
            )
	when( mcv == nil
	    printf("HVT subtype already exists %s\n" newCellName))
         when( ((cv != nil)&&(mcv != nil))
            printf("yes in distort/distort_pg\n")
            ;dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
	foreach(inst cv->instances
		when( inst->libName=="synthesis.shared.fill"
		dbCopyFig(inst ncv)
            ;ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            ;Fill( ncv ViewName)
            ;SyncToNetlist()
    )
	when( inst->libName=="lib.util.spare"
	dbCopyFig(inst ncv)
	))
dbSave( ncv )
dbClose( ncv )
 )
         when( ((lcv != nil)&&(mcv != nil))
            printf("yes in layout/layout_pg\n")
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
	foreach(inst lcv->instances
		when( inst->libName=="synthesis.shared.fill"
		dbCopyFig(inst ncv)
    )
	when( inst->libName=="lib.util.spare"
	dbCopyFig(inst ncv)
	))
dbSave( ncv )
dbClose( ncv )
 )

))

(defun fill ()
	(let (text CellName num)
	fl = infile("list_5.txt")
	while( gets(k fl) != nil
  	text = parseString( k " \n") 
  	CellName = nth(0 text)
	num = nth(1 text)
	when( num != "0"
  	fillCell(CellName "distort")
  	fillCell(CellName "distort_pg")
))))


