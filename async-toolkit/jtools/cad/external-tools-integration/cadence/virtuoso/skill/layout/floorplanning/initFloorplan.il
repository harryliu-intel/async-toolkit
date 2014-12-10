; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( flInitFloorplan ( libName cellName viewName
                     @key
                      ( Verbose nil )
                      ( RegenerateFromCast nil )
                      ( ScriptFileName "" )
                      ( SnapXGrid 0.01 )
                      ( MaxHeapSize (nrGetMaxHeapSize) )
                      ( InstOrderList nil )
                      ( TaskOption "" )
                      ( x_offset 0.0 )
                      ( y_offset 0.0 )
                      ( SubcellsToCreate nil )
                      ( DoNotMoveExistingInstances t )
                      ( RunSyncToNetlist t )
                     )
  prog( ( TargetDFIIPath NetlistDir DirectivesDir Command DirectiveTable InstOrderFile CellView p_in 
         width height x1 y1 x y prBoundBox thisInst inst line instances instName lineList instMaster scriptFilterCmd
         unit command commandList )
    TargetDFIIPath = ConfigFileGetValue( TheCDSConfigTable "TEMP" )
    NetlistDir = sprintf( nil "%s/ilnets" TargetDFIIPath)  
    DirectivesDir = sprintf( nil "%s/ildirectives" TargetDFIIPath )
    InstOrderFile = sprintf( nil "%s/%s.instOrder" TargetDFIIPath cellName)
    CellView=nrOpenCellViewWritable( libName cellName viewName )
    scriptFilterCmd=""
    unit=PowerGridPitch*4
    if( ScriptFileName != "" then
      if( isFile(ScriptFileName) then
        scriptFilterCmd=sprintf(nil "| %s %s" ScriptFileName cellName)
      else
        printf("Warning: Script File %s does not exist, skipped script filtering.\n" ScriptFileName)
      )
    )
    if( CellView == nil then 
      printf("Error: Failed to open cellview %s (%s) for write.\n" cellName viewName)
      return(0)
    )
    if( InstOrderList == nil then
      nrLoadRoutedDirective( CellView 
                        ?Verbose Verbose
                        ?RegenerateFromCast RegenerateFromCast 
                        ?MaxHeapSize MaxHeapSize
      )

      if( RunSyncToNetlist then 
        SyncNetlistGenFromSkillNetlistUsingPDKInfo( 
           CellView
           NetlistDir 
           DirectivesDir
           ?DoNotMoveExistingInstances DoNotMoveExistingInstances
        )
        dbSave( CellView )
      )
    )
    return(CellView)
  )
)

defun( flInitFloorplanWithInstOrderList ( libName cellName viewName
                     @key
                      ( Verbose nil )
                      ( RegenerateFromCast nil )
                      ( ScriptFileName "" )
                      ( SnapXGrid 0.01 )
                      ( MaxHeapSize (nrGetMaxHeapSize) )
                      ( InstOrderList nil )
                      ( TaskOption "" )
                      ( x_offset 0.0 )
                      ( y_offset 0.0 )
                      ( SubcellsToCreate nil )
                      ( DoNotMoveExistingInstances t )
                      ( RunSyncToNetlist nil )
                     )
  prog( ( TargetDFIIPath NetlistDir DirectivesDir Command DirectiveTable InstOrderFile CellView p_in 
         width height x1 y1 x y prBoundBox thisInst inst line instances instName lineList instMaster scriptFilterCmd
         unit command commandList selectedSubcells thisObj)
    TargetDFIIPath = ConfigFileGetValue( TheCDSConfigTable "TEMP" )
    NetlistDir = sprintf( nil "%s/ilnets" TargetDFIIPath)  
    DirectivesDir = sprintf( nil "%s/ildirectives" TargetDFIIPath )
    InstOrderFile = sprintf( nil "%s/%s.instOrder" TargetDFIIPath cellName)
    CellView=nrOpenCellViewWritable( libName cellName viewName )
    selectedSubcells=nil
    foreach( thisObj geGetSelectedSet()
      if( thisObj~>objType=="inst" then selectedSubcells=cons( thisObj~>name selectedSubcells ))
    )
    printf( "Processing selected subcells: %L\n" selectedSubcells )
    scriptFilterCmd=""
    unit=PowerGridPitch*4
    if( ScriptFileName != "" then
      if( isFile(ScriptFileName) then
        scriptFilterCmd=sprintf(nil "| %s %s" ScriptFileName cellName)
      else
        printf("Warning: Script File %s does not exist, skipped script filtering.\n" ScriptFileName)
      )
    )
    if( CellView == nil then 
      printf("Error: Failed to open cellview %s (%s) for write.\n" cellName viewName)
      return(0)
    )
    if( InstOrderList == nil then
      nrLoadRoutedDirective( CellView 
                        ?Verbose Verbose
                        ?RegenerateFromCast RegenerateFromCast 
                        ?MaxHeapSize MaxHeapSize
      )
      Command = sprintf( nil
                "%s/cast_query --cast-path=%s --cell=%s --task=initial_floorplan%s --no-header --no-recurse --translate=cadence --cadence-name --max-heap-size=%dM --64 %s > %s"
                PackageGetBinRoot( )
                ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
                CellView~>cellName
                TaskOption
                MaxHeapSize 
                scriptFilterCmd
                InstOrderFile 
                )
      printf( "%s\n" Command )
      shell( Command )
      p_in=infile(InstOrderFile)
      InstOrderList=nil
      while( gets( line p_in)
        line=car(parseString( line "\n"))
        InstOrderList=append( InstOrderList list( line ))
      )
      close(p_in)
    )
    x=x_offset
;    println(InstOrderList)
    printf("Processing %s creating subcells: " CellView~>cellName )
    println(SubcellsToCreate)
    foreach( line InstOrderList
      y=y_offset
      nextX=x
      lineList=parseString( line " " )
      while(lineList!= nil
        instName=car(lineList)
        lineList=cdr(lineList)
        if( instName!=nil && !rexMatchp("^<.*>$" instName)  then
          if( DoNotMoveExistingInstances == nil || member( instName SubcellsToCreate ) || member( instName selectedSubcells ) then
            thisInst=car( setof( inst CellView~>instances inst~>name==instName ))
            if( thisInst == nil then
              printf("Error: instance %s does not exist in layout.\n" instName )
            else
              instMaster=nrOpenCellViewReadable( thisInst~>libName thisInst~>cellName thisInst~>viewName)
              prBound=setof( lpp instMaster~>lpps lpp~>layerName=="prBoundary" )
              if((prBound !=nil) then
                prBoundBox=nil
                foreach( thisLayer prBound
                  foreach( shape thisLayer~>shapes
                    if((shape~>objType!="label" && prBoundBox==nil) then
                      prBoundBox=shape~>bBox
                    )
                  )
                )
                if((prBoundBox==nil || thisInst~>xy==nil) then 
                  prBoundBox=instMaster~>bBox
                  printf("Warning: %s (%s) does not have rectangle prBoundary. Use bBox instead.\n"
                    thisInst~>name thisInst~>cellName)
                )
              else 
                prBoundBox=instMaster~>bBox
                printf("Warning: %s (%s) does not have prBoundary layer. Use bBox instead.\n"
                  thisInst~>name thisInst~>cellName)
              )
              printf("%f %f %L " x y prBoundBox)
              x1=x-leftEdge(prBoundBox)
              y1=y-bottomEdge(prBoundBox)
              width=rightEdge(prBoundBox)-leftEdge(prBoundBox)
              height=topEdge(prBoundBox)-bottomEdge(prBoundBox)
              width=ceiling(width/SnapXGrid)*SnapXGrid
              thisInst~>xy=list( x1 y1 )
              printf("%L\n" list( x1 y1 ))
;            dbMoveFig( thisInst CellView list(x1-car(thisInst~>xy):y1-cadr(thisInst~>xy) "R0"))
              y=y+height
              nextX=max( x+width nextX )
            )
          )
        else 
          if(rexMatchp("^<.*>$" instName) then
            command=substring(instName 2 strlen(instName)-2)
            commandList=parseString( command "=" )
            case( car(commandList)
              ("unit" sscanf(cadr(commandList) "%f" x1) unit=x1 )
              ("xSpace" sscanf(cadr(commandList) "%f" x1) x=x+x1*unit nextX=max(x nextX))
              ("ySpace" sscanf(cadr(commandList) "%f" y1) y=y+y1*unit )
              ("x" sscanf(cadr(commandList) "%f" x1) x=x1*unit nextX=max(x nextX))
              ("y" sscanf(cadr(commandList) "%f" y1) y=y1*unit+y_offset )
              (t printf("Unrecognized Command \"%s\".\n" command))
            )
          )
        )
      )
      x=nextX
    )
    return( CellView )
    
  )
)

