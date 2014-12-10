; Copyright 2005 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( flAddOrient ( orient1 instance )
  case( sprintf( nil "%s|%s" orient1 instance~>orient )
      ("R0|R0" "R0")
      ("MY|MY" "R0")
      ("R0|MY" "MY")
      ("MY|R0" "MY")
      ( t printf("Warning: illegal orient(%s)(%s) found!\n" instance~>cellName instance~>orient) "R0")
  )  
)

defun( flAddxy ( XY1 XY2 orient2 )
  let(( X3string Y3string x3 y3)
    case( orient2
      ("R0" X3string=sprintf( nil "%s +%s" caar(XY1) caar(XY2))
            Y3string=sprintf( nil "%s +%s" cadar(XY1) cadar(XY2))
            x3=caadr(XY1)+caadr(XY2) 
            y3=cadadr(XY1)+cadadr(XY2)
      )
      ("MY" X3string=sprintf( nil "%s -%s" caar(XY1) caar(XY2))
            Y3string=sprintf( nil "%s +%s" cadar(XY1) cadar(XY2))
            x3=caadr(XY1)-caadr(XY2) 
            y3=cadadr(XY1)+cadadr(XY2)
      )
      ( t printf("Warning: illegal orient(%s) found!\n" orient2) 
            X3string=sprintf( nil "%s +%s" caar(XY1) caar(XY2))
            Y3string=sprintf( nil "%s +%s" cadar(XY1) cadar(XY2))
            x3=caadr(XY1)+caadr(XY2) 
            y3=cadadr(XY1)+cadadr(XY2)
      )
    )
    rexCompile("--")
    X3string=rexReplace(X3string "+" 0)
    Y3string=rexReplace(Y3string "+" 0)
    rexCompile("[+]-")
    X3string=rexReplace(X3string "-" 0)
    Y3string=rexReplace(Y3string "-" 0)
    rexCompile("^top [+]")
    X3string=rexReplace(X3string "" 0)
    Y3string=rexReplace(Y3string "" 0)
    rexCompile("^top ")
    X3string=rexReplace(X3string "" 0)
    Y3string=rexReplace(Y3string "" 0)
    list( list(X3string Y3string) list(x3 y3))
  )
)

defun( flGetLeafCellInfo ( inst parentCellName parentxy parentOrient)
  let( (bBox transform xy orient instCellView width height XYlist variableName prBound shape yy)
     width=-1 height=-1 orient=inst~>orient
     instCellView=inst~>master
     variableName=sprintf(nil "%s|%s" parentCellName inst~>name)
     XYlist=list( list(variableName variableName) inst~>xy)
     prBound=car(setof( lpp instCellView~>lpps lpp~>layerName=="prBoundary" && lpp~>purpose=="drawing"))
     if( prBound && prBound~>nShapes != 0 then              
       if( prBound~>nShapes == 1 then
         shape=car(prBound~>shapes)
         if(shape~>objType=="rect" then 
           bBox=shape~>bBox
         else 
           printf("Warning: Non rectagular prBound in %s(%s), use cellview bBox instead of prBound layer.\n" inst~>name inst~>cellName)                   bBox=shape~>bBox
         )
       else
         printf("Warning: More than one prBound layer found in %s(%s)(%d), use first one.\n" inst~>name inst~>cellName prBound~>nShapes)           
         shape=car(prBound~>shapes)
         bBox=shape~>bBox
       )
    else
      printf("Warning: No prBound layer found in %s(%s), use cellview bBox instead.\n" inst~>name inst~>cellName)           
      bBox=instCellView~>bBox
    )

    width=caadr(instCellView~>bBox)-caar(instCellView~>bBox)
    height=cadadr(instCellView~>bBox)-cadar(instCellView~>bBox)
    yy=cadr(inst~>xy)+cadar(instCellView~>bBox)  
             
    list( sprintf(nil "%s|%s" parentCellName inst~>name) inst~>cellName parentxy bBox  parentOrient )
  )
)

defun( flGetMidCellInfo (instance parentCellName parentxy parentOrient hierachyName
                        @key
                        ( SkipCellList nil )
           )
  let( ( orient instances XYlist variableName fullInstName inst instances LibCellsToIgnore 
         instCellView instName)
    LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs ) ) )
    variableName=sprintf(nil "%s|%s" parentCellName instance~>name)
    instCellView=instance~>master
    instances= car( NameFilterInstances( instCellView~>instances LibCellsToIgnore ))
;    instances=setof( inst instances !member(inst~>cellName SkipCellList))
    orient=flAddOrient( parentOrient instance)
    XYlist=flAddxy( parentxy list( list(variableName variableName) instance~>xy) parentOrient)
    instName=sprintf( nil "%s|%s" parentCellName instance~>name )
    xValueTable[instName]=car(instance~>xy)
    xLeftEdgeTable[instName]=caar(instance~>bBox)
    fullInstName=sprintf( nil "%s|%s" hierachyName instance~>name )
    if( instances !=nil && !member(instance~>cellName SkipCellList) && instance~>viewName != "layout" then
      foreach( inst instances
        flGetMidCellInfo( inst instance~>cellName XYlist orient
                 sprintf( nil "%s|%s" hierachyName instance~>name ) ?SkipCellList SkipCellList)
      )
      midCellList=cons( 
         list( instName 
               instance~>cellName XYlist
               instance~>orient fullInstName) 
         midCellList )
      instList=cons( list( "mid" fullInstName instance~>cellName XYlist orient ) instList )
    else
      leafCellList=cons( append( flGetLeafCellInfo( instance parentCellName XYlist orient ) list(fullInstName)) leafCellList )
      instList=cons( list( "leaf" fullInstName instance~>cellName XYlist orient ) instList )
    )
  )
)

defun( flFillMidCellInfo ( cellView paraTable 
                         @key
                         ( SaveCellView nil )
                         ( SkipCellList nil )
         )
  let( ( LibCellsToIgnore instances inst x cv instanceMasters transfrom errorCount)
    errorCount=0
    LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs )))
    instances= car( NameFilterInstances( cellView~>instances LibCellsToIgnore ))
    if( instances !=nil then
      printf("Processing %s...\n" cellView~>cellName)
      cv=nrOpenCellViewWritable(cellView~>libName cellView~>cellName cellView~>viewName)
      if(cv==nil then 
        printf("Error: Unable to open %s %s(%s) for write.\n" cellView~>libName cellView~>cellName cellView~>viewName)
        errorCount=errorCount+1
      else
        foreach( inst instances
          x=paraTable[sprintf( nil "%s|%s" cellView~>cellName inst~>name )]-car(inst~>xy)
          transform=list(list(x 0) "R0" 1.0)
          dbMoveFig( inst cellView transform)
        )
        if( SaveCellView then dbSave( cellView ))
      )
    )
    instanceMasters=nil
    instances=setof( inst instances !member(inst~>cellName SkipCellList) && inst~>viewName != "layout" )
    foreach( cv instances~>master
      if(!member(cv~>cellName instanceMasters~>cellName) then
        instanceMasters=cons( cv instanceMasters )
        errorCount=errorCount+flFillMidCellInfo(cv paraTable ?SaveCellView SaveCellView ?SkipCellList SkipCellList)
      )
    )
    errorCount
  )
)

defun( flFillTopCellInfo ( cellView paraTable
                         @key
                         ( SaveCellView nil )
                         ( SkipCellList nil )
                         ( BlockLayer list("LOGO" "drawing"))
         )
  let( ( LibCellsToIgnore cv errorCount blockLayer lpp shape instName cellName x1 y1 x2 y2 width height)
    errorCount=0
    cv=nrOpenCellViewWritable(cellView~>libName cellView~>cellName cellView~>viewName)
    if(cv==nil then 
        printf("Error: Unable to open %s %s(%s) for write.\n" cellView~>libName cellView~>cellName cellView~>viewName)
        errorCount=errorCount+1
    else
      blockLayer=car( setof(lpp cv~>lpps lpp~>layerName==car(BlockLayer) && lpp~>purpose==cadr(BlockLayer)))
      foreach( shape blockLayer~>shapes dbDeleteObject(shape))
      if( blockCellList then
        foreach( cell blockCellList
          instName=car(cell)
          cellName=cadr(cell)
          x1=caddr(cell)
          y1=cadddr(cell)
          x2=cadddr(cdr(cell))
          y2=cadddr(cddr(cell))
          width=cadr(x2)-cadr(x1)
          height=cadr(y2)-cadr(y1)
          x1=floor(xValueTable[instName]*1000.0+0.5)/1000.0
          y1=cadr(y1)          
          dbCreateRect( cv BlockLayer list( x1:y1 x1+width:y1+height))
        )
        if( SaveCellView then dbSave(cv))
      )
      errorCount=errorCount+flFillMidCellInfo(cv paraTable ?SaveCellView SaveCellView ?SkipCellList SkipCellList)
    ) 
    errorCount
  )
)
defun( flGetTopCellInfo ( cellView 
                        @key
                        ( SkipCellList nil )
                        ( RegenerateFromCast nil )
                        ( MaxHeapSize (nrGetMaxHeapSize) )
                        ( BlockLayer list("LOGO" "drawing") )
         )
  let( ( orient instances routedDirectiveList LibCellsToIgnore inst 
         blockCount shape cellName instName fullInstName bBox
         x1x2 y1 y2)
    LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs ) ) )
    blockLayer=car( setof(lpp cellView~>lpps lpp~>layerName==car(BlockLayer) && lpp~>purpose==cadr(BlockLayer)))
    blockCount=1
    if( blockLayer~>shapes then
      printf("Found %d blockage shapes in top cell.\n" blockLayer~>nShapes) 
      foreach( shape blockLayer~>shapes
        cellName=sprintf(nil "dummyblock%d" blockCount)
        instName=sprintf(nil "%s|%s" cellview~>cellName cellName)
        fullInstName=sprintf( nil "top|%s" cellName)
        blockCount=blockCount+1
        bBox=shape~>bBox
        x1=list( instName caar(bBox))
        x2=list( sprintf( nil "%s +%f" instName caadr(bBox)-caar(bBox)) caadr(bBox))
        y1=list( instName cadar(bBox))
        y2=list( sprintf( nil "%s +%f" instName cadadr(bBox)-cadar(bBox)) cadadr(bBox))
        blockCellList=cons( list( instName cellName x1 y1 x2 y2 fullInstName ) blockCellList )
        xValueTable[instName]=cadr(x1)
        xLeftEdgeTable[instName]=cadr(x1)        
      )
    )
    instances= car( NameFilterInstances( cellView~>instances LibCellsToIgnore ))
;    routedDirectiveList=nrCastQuery( cellView ?RegenerateFromCast RegenerateFromCast ?MaxHeapSize MaxHeapSize)
    foreach( inst instances
      flGetMidCellInfo( inst cellView~>cellName list(list("top" "top") list(0 0)) "R0" "top" 
          ?SkipCellList SkipCellList
      )
    )
  )
)

defun( flParseConstraint ( string variableTable replaceValueTable 
                        @key
                        ( SnapXGrid 0.0 )
       )
  let( (name sections paraTable para section op i j foundAlpha vName vPara para finalString vValue)
    paraTable=makeTable("myTable" 0.0)
    op=""
    section=listToVector(parseString(string " "))
    for(i 0 length(section)-1
      foundAlpha=0
      j=-1
      if(section[i]==">=" || section[i]=="<=" || section[i]=="=" then 
        op=section[i] 
      else
        while( foundAlpha==0 && j<strlen(section[i])-1
          j=j+1
          if(rexMatchp("[a-zA-Z_]" substring(section[i] j+1 1)) then foundAlpha=1)
        )
        if(foundAlpha==0 then vName="constant" 
        else 
          vName=substring(section[i] j+1 strlen(section[i])-j+1)
          vName=variableTable[vName]
        ) 
        if(j==0 then vPara="1.0"
        else vPara=substring(section[i] 1 j))
        if(!rexMatchp("[0-9.]" substring(vPara strlen(vPara) 1)) then vPara=strcat(vPara "1"))
        if(substring(vPara 1 2)=="+-" then vPara=strcat("-" substring(vPara 3 strlen(vPara)-2)))
        if(substring(vPara 1 2)=="--" then vPara=strcat("+" substring(vPara 3 strlen(vPara)-2)))
        sscanf( vPara "%f" para)
;        if!(rexMatchp("[a-zA-Z_]" vName) then
;           sscanf( vName "%f" vValue )   
;           para=para*vValue
;           vName="constant"
;        ) 
        if(op!="" then para=-para)
        paraTable[vName]=paraTable[vName]+para
      )
    )
    ; replace those vName with constant
    foreach(vName replaceValueTable
      paraTable["constant"]=paraTable["constant"]+paraTable[vName]*replaceValueTable[vName]
      paraTable[vName]=0.0
    )
    finalString=""
    foreach(vName paraTable
      if(vName!="constant" then 
        if(paraTable[vName]>0 then finalString=sprintf(nil "%s +%f%s" finalString paraTable[vName] vName))
        if(paraTable[vName]<0 then finalString=sprintf(nil "%s %f%s" finalString paraTable[vName] vName))
      )
    )
    ;snap paraTable["constant"] to SnapXGrid
    if( SnapXGrid>0.0 then
      if( op == ">=" then
        paraTable["constant"]=floor(paraTable["constant"]/SnapXGrid)*SnapXGrid 
      )
      if( op == "<=" then
        paraTable["constant"]=ceiling(paraTable["constant"]/SnapXGrid)*SnapXGrid 
      )
    )
    if(finalString != "" then
      finalString=sprintf(nil "%s %s %f" finalString op -paraTable["constant"])
    else
      ; when parameters of all vName is zero
      if( abs(paraTable["constant"])>1e-5 then
        if( op == ">=" && paraTable["constant"]<0.0 then
          printf( "Warning 0>=+1: %s\n" string ))
        if( op == "<=" && paraTable["constant"]>0.0 then
          printf( "Warning 0<=-1: %s\n" string ))
      )
      finalString=nil
    )
    finalString
  )
)

defun( flGetArrayName ( string )
  let((temp value)
    temp=parseString(string "[")
    value=car(temp)
    temp=cdr(temp)
    if( temp then
      while( cdr(temp)
        value=sprintf(nil "%s[%s" value car(temp))
        temp=cdr(temp)
      )
      if( rexMatchp("|" car(temp)) then value=nil)
    else value=nil
    )
    if(value && substring(value strlen(value) 1)=="]" then value=flGetArrayName(value))
    value
  )
)

defun( flSaveCoordinates ( cellview filename 
                       @key
                       ( SkipCellList nil )
                       ( BlockLayer list("LOGO" "drawing") )
        )
  let((instances errorCount p cellInfo x1 y1 x2 y2 xy1 xy2 minX1 minX2 minY1 minY2)
    instances=cellview~>instances
    midCellList=nil
    leafCellList=nil
    blockCellList=nil
    instList=nil
    errorCount=0
    xValueTable=makeTable("myTbale" 0)
    xLeftEdgeTable=makeTable("myTbale" 0)
    cellInfo=list( cellview~>libName cellview~>cellName cellview~>viewName )
    printf("Exporting Coordinates ...\n")
    flGetTopCellInfo( cellview ?SkipCellList SkipCellList ?BlockLayer BlockLayer)

    p=outfile(filename)
    if(p then
      fprintf(p "cellInfo= `%L\n" cellInfo)
      fprintf(p "SkipCellList= `%L\n" SkipCellList)
      fprintf(p "blockCellList= `%L\n" blockCellList)
      fprintf(p "BlockLayer= `%L\n" BlockLayer)
      valueList=tableToList(xValueTable)
      fprintf(p "valueList= nil\n")
      foreach( cell valueList 
        fprintf(p "valueList=cons( `%L valueList )\n" cell)
      )
      fprintf(p "leafCellList= nil\n")
      foreach( cell leafCellList 
        fprintf(p "leafCellList=cons( `%L leafCellList )\n" cell)
      )
      fprintf(p "midCellList= nil\n")
      foreach( cell midCellList 
        fprintf(p "midCellList=cons( `%L midCellList )\n" cell)
      )
      fprintf(p "instList= nil\n")
      foreach( cell instList 
        fprintf(p "instList=cons( `%L instList )\n" cell)
      )
      close(p)
      printf("Success saved coordinates to file %s.\n" filename)
    else
      printf("Fail to open file %s for write.\n" filename)
    )
  )
)

defun( flLoadCoordinates ( cellview filename 
                       @key
                       ( SkipCellList nil )
                       ( SaveCellView nil )
                       ( BlockLayer list("LOGO" "drawing") )
        )
  let((instances errorCount p cellInfo x1 y1 x2 y2 xy1 xy2 minX1 minX2 minY1 minY2)
    instances=cellview~>instances
    midCellList=nil
    leafCellList=nil
    blockCellList=nil
    instList=nil
    cellInfo=nil
    errorCount=0
    load(filename)
    xValueTable=makeTable("myTable" 0.0)
    foreach( para valueList xValueTable[car(para)]=cadr(para))
    if( car(cellInfo)==cellview~>libName && cadr(cellInfo)==cellview~>cellName && caddr(cellInfo)==cellview~>viewName then
      printf("Importing Coordinates ...\n")
      errorCount=flFillTopCellInfo(cellview xValueTable ?SaveCellView SaveCellView ?BlockLayer BlockLayer ?SkipCellList SkipCellList)
      if( errorCount==0 then
        printf("All cells has been updated.\n")
      else
        printf("Total %d errors found in importing coordinates. \n" errorCount)
      ) 
    else
      printf("The cellInfo of cellview and file %s does not match:\n" filename)
      printf("  Cellview: %s %s (%s)\n" cellview~>libName cellview~>cellName cellview~>viewName)
      printf("  File    : %s %s (%s)\n" car(cellInfo) cadr(cellInfo) caddr(cellInfo))
    )
  )
)

defun( flResolveOverlap ( cellview 
                       @key
                       ( SkipCellList nil )
                       ( FreezeCellList nil )
                       ( LPMacroFileName "glpsol.lpt")
                       ( LPOutputFileName "glpsol.out")
                       ( LPLogFileName "glpsol.log")
                       ( LPmemoFileName "glpsol.memo")
                       ( BlockLayer list("LOGO" "drawing"))
                       ( SaveCellView nil )
                       ( SaveFileName "temp.sav" )
                       ( AbutCellKeepOrder t )
                       ( SingleHierarchy nil )
                       ( xtotal 0 )
                       ( Mode 0 )
                       ( SnapXGrid 0.0 )
              )
  let((instances errorCount cell x1 y1 x2 y2 xy1 xy2 minX1 minX2 minY1 minY2 cv_minX topName xMinBound SaveFileName1 p p1)
    instances=cellview~>instances
    midCellList=nil
    leafCellList=nil
    blockCellList=nil
    instList=nil
    errorCount=0
    cv_minX=caar(cellview~>bBox)
    xVariableTable=makeTable("myTable" 0)
    xValueTable=makeTable("myTable" 0.0)
    xValueTableOld=makeTable("myTable" 0.0)
    xValueTableNew=makeTable("myTable" 0.0)
    xVariableTable["xtotal"]="xtotal"
    xLeftEdgeTable=makeTable("myTbale" 0)

    if( xtotal!=0 then 
      SaveFileName1=SaveFileName
      load(SaveFileName)
      foreach( para valueList xValueTableOld[car(para)]=cadr(para))
      SaveFileName=sprintf(nil "%s.%d" SaveFileName Mode)
    )

    if( SingleHierarchy then SkipCellList=append( cellview~>instances~>cellName SkipCellList ))
    flSaveCoordinates( cellview SaveFileName ?SkipCellList SkipCellList ?BlockLayer BlockLayer )
    if( xtotal!=0 then 
      load(SaveFileName)
      foreach( para valueList xValueTableNew[car(para)]=cadr(para))
      flLoadCoordinates( cellview SaveFileName1 ?SkipCellList SkipCellList ?BlockLayer BlockLayer )
    )

    PinUtilDeletePinsByType( cellview "InPlace" )

    printf("Building Constraints ...\n")

    p=outfile(LPMacroFileName "w")
    if( p==nil then printf("Can not open %s for write.\n" LPMacroFileName)) 
    p1=outfile(LPmemoFileName "w")
    if( p1==nil then printf("Can not open %s for write.\n" LPmemoFileName)) 
    fprintf(p "\\ Variable Map\n")

    unique=nil
    i=1
    foreach( cell blockCellList
      xVariableTable[car(cell)]=sprintf(nil "xb%04d" i)
      i=i+1
    )
    foreach( cell leafCellList
      if( !member( car(cell) unique) then 
        unique=cons( car(cell) unique)
        xVariableTable[car(cell)]=sprintf(nil "xl%04d" i)
        i=i+1
      )
    )
    foreach( cell midCellList
      if( !member( car(cell) unique) then 
        unique=cons( car(cell) unique)
        xVariableTable[car(cell)]=sprintf(nil "xm%04d" i)
        i=i+1
      )
    )

    ; process mid array contraints
    xMid=listToVector(midCellList)
    for(i 0 length(xMid)-2
      instName1=car(xMid[i])
      cellName1=cadr(xMid[i])
      posXY1=caddr(xMid[i])
      orient1=cadddr(xMid[i])
      fullInstName1=caddr(cddr(xMid[i]))
      fullInstArrayName1=flGetArrayName(fullInstName1)
      minX1=list(caar(posXY1) caadr(posXY1))
      minY1=list(cadar(posXY1) cadadr(posXY1))
      if( fullInstArrayName1 then
        for(j i+1 length(xMid)-1
          instName2=car(xMid[j])
          cellName2=cadr(xMid[j])
          posXY2=caddr(xMid[j])
          orient2=cadddr(xMid[j])
          fullInstName2=caddr(cddr(xMid[j]))
          fullInstArrayName2=flGetArrayName(fullInstName2)
          minX2=list(caar(posXY2) caadr(posXY2))
          minY2=list(cadar(posXY2) cadadr(posXY2))
          ; type 1: if two mid cells have same array name and same x coordinate, then combining two x varaibles into one
          if( fullInstArrayName1 && fullInstArrayName2 && fullInstArrayName1==fullInstArrayName2 &&
             cellName1==cellName2 && abs(cadr(minX1)-cadr(minX2))<0.001 then 
             variableName1=xVariableTable[instName1]
             variableName2=xVariableTable[instName2]
             foreach( vName xVariableTable
               if(xVariableTable[vName]==variableName2 then xVariableTable[vName]=variableName1)
             )
          ) ; type 1
        )
      )
    )

    xLeaf=nil
    foreach( cell leafCellList
      instName=car(cell)
      cellName=cadr(cell)
      XYpos=caddr(cell)
      bBox=cadddr(cell)
      originPos=cadr(XYpos)
      orient=caddr(cddr(cell))
      fullInstName=caddr(cdddr(cell))
      xy1=car(bBox)
      xy2=cadr(bBox)
      xy1=list(list(sprintf(nil "%f" car(xy1)) sprintf(nil "%f" cadr(xy1))) xy1)
      xy2=list(list(sprintf(nil "%f" car(xy2)) sprintf(nil "%f" cadr(xy2))) xy2)
      xy1=flAddxy( XYpos xy1 orient)
      xy2=flAddxy( XYpos xy2 orient)
      if(caadr(xy1)>caadr(xy2) then 
        x1=list( caar(xy2) caadr(xy2)) 
        x2=list( caar(xy1) caadr(xy1)) 
      else
        x1=list( caar(xy1) caadr(xy1)) 
        x2=list( caar(xy2) caadr(xy2)) 
      )
      if(cadadr(xy1)>cadadr(xy2) then 
        y1=list( cadar(xy2) cadadr(xy2)) 
        y2=list( cadar(xy1) cadadr(xy1)) 
      else
        y1=list( cadar(xy1) cadadr(xy1)) 
        y2=list( cadar(xy2) cadadr(xy2)) 
      )
      xLeaf=cons( list( instName cellName x1 y1 x2 y2 fullInstName) xLeaf)
    )

    xLeaf=listToVector(append(blockCellList xLeaf))
    ; process leaf array contraints
    for(i 0 length(xLeaf)-2
      instName1=car(xLeaf[i])
      cellName1=cadr(xLeaf[i])
      minX1=caddr(xLeaf[i])
      minY1=cadddr(xLeaf[i])
      maxX1=caddr(cddr(xLeaf[i]))
      maxY1=caddr(cdddr(xLeaf[i]))
      fullInstName1=cadddr(cdddr(xLeaf[i]))
      fullInstArrayName1=flGetArrayName(fullInstName1)
      if( fullInstArrayName1 then
        for(j i+1 length(xLeaf)-1
          instName2=car(xLeaf[j])
          cellName2=cadr(xLeaf[j])
          minX2=caddr(xLeaf[j])
          minY2=cadddr(xLeaf[j])
          maxX2=caddr(cddr(xLeaf[j]))
          maxY2=caddr(cdddr(xLeaf[j]))
          fullInstName2=cadddr(cdddr(xLeaf[j]))
          fullInstArrayName2=flGetArrayName(fullInstName2)

          ; type2: if two leaf cells have same array name and same x coordinate, then combining two x varaibles into one
          if( fullInstArrayName1 && fullInstArrayName2 && fullInstArrayName1==fullInstArrayName2 &&
             cellName1==cellName2 && abs(cadr(minX1)-cadr(minX2))<0.001 then 
             variableName1=xVariableTable[instName1]
             variableName2=xVariableTable[instName2]
             foreach( vName xVariableTable
               if(xVariableTable[vName]==variableName2 then xVariableTable[vName]=variableName1)
             )
          ) ; type 2

        )
      )
    )

    ; type 3.a: the leftmost subcell in each mid cell will not move
    ; all those subcell x position variable will become constant 
    xMinBound= cv_minX 
    xMinValueTable=makeTable("myTable" list(1e9 ""))
    foreach( instName xValueTable
      topName=car(parseString(instName "|"))
      if( !member(topName list("top" "xtotal")) then
        if(xLeftEdgeTable[instName]<car(xMinValueTable[topName]) then
          xMinValueTable[topName]=list( xLeftEdgeTable[instName] xValueTable[instName] instName )
          xMinBound=min( xMinBound xLeftEdgeTable[instName] xValueTable[instName] )   
        )
      )  
    )
    xReplaceValueTable=makeTable("myTable" 0.0)
    foreach( topName xMinValueTable
      xReplaceValueTable[xVariableTable[caddr(xMinValueTable[topName])]]=cadr(xMinValueTable[topName])
    )
    ; type 3.a

    ; type 3.b: the cells in FreezeCellList are not moving.
    foreach( instName FreezeCellList
      xReplaceValueTable[xVariableTable[instName]]=xValueTable[instName]
    )
    ; type 3.b    

    uniqueVNames=nil
    uniqueNames=nil
    paraTable=makeTable("myTable" 0.0)
    foreach( cell sort(tableToList( xVariableTable ) lambda((a b) strcmp(cadr(a) cadr(b))<1))
      fprintf(p1 "%s %s\n" cadr(cell) car(cell))
      fprintf(p "\\ %s %s\n" cadr(cell) car(cell))
      if( !member( cadr(cell) uniqueVNames) && !member( cadr(cell) foreach( mapcar i tableToList(xReplaceValueTable) car(i))) then 
         uniqueVNames=cons( cadr(cell) uniqueVNames)
         if(car(cell)!="xtotal" uniqueNames=cons( car(cell) uniqueNames))
      )
      paraTable[cadr(cell)]=paraTable[cadr(cell)]+if(rexMatchp("xl" cadr(cell)) 1.0 100.0)

    )


    fprintf(p "\nMinimize\n")
    if( xtotal == 0 then
      fprintf(p "  total: xtotal\n")
    else
      fprintf(p "  total: ")
      if( Mode==1 then
        foreach( cell uniqueNames
          if(xValueTableNew[cell]>= xValueTableOld[cell]
            fprintf(p "+%f%s " paraTable[xVariableTable[cell]] xVariableTable[cell]) 
            fprintf(p "-%f%s " paraTable[xVariableTable[cell]] xVariableTable[cell])
          )
        )
      else 
        foreach( cell uniqueVNames fprintf(p "+%f%s " paraTable[cell] cell))
      )
      fprintf(p "\n")
    )

    fprintf(p "\nSubject To\n")
    constraintList=""
    constraintCount=1

    if( xtotal !=0 && Mode==1 then
      foreach( cell uniqueNames 
        if(xValueTableNew[cell]>= xValueTableOld[cell] then
          constraint=sprintf(nil "%s >= %f" cell xValueTableOld[cell])
        else
          constraint=sprintf(nil "%s <= %f" cell xValueTableOld[cell])
        )
        fprintf(p1 "\\ Constraint%d: %s\n" constraintCount constraint) 
        constraintString=flParseConstraint(constraint xVariableTable xReplaceValueTable ?SnapXGrid SnapXGrid)
        if( constraintString then 
          fprintf(p "  Constraint%d: %s\n" constraintCount constraintString) 
        else
          fprintf(p "\\ Constraint%d: \n" constraintCount)
        )
        constraintCount=constraintCount+1
      )
      fprintf(p1 "\n")
      fprintf(p "\n")
    )
    for(i 0 length(xLeaf)-1
      instName1=car(xLeaf[i])
      cellName1=cadr(xLeaf[i])
      minX1=caddr(xLeaf[i])
      minY1=cadddr(xLeaf[i])
      maxX1=caddr(cddr(xLeaf[i]))
      maxY1=caddr(cdddr(xLeaf[i]))
      fullInstName1=cadddr(cdddr(xLeaf[i]))
      constraint=sprintf(nil "%s >= %f" car(minX1) cv_minX)
      fprintf(p1 "\\ Constraint%d: %s\n" constraintCount constraint) 
      constraintString=flParseConstraint(constraint xVariableTable xReplaceValueTable ?SnapXGrid SnapXGrid)
      if( constraintString then 
        fprintf(p "  Constraint%d: %s\n" constraintCount constraintString) 
      else
        fprintf(p "\\ Constraint%d: \n" constraintCount)
      )
      constraintCount=constraintCount+1
      constraint=sprintf(nil "%s -xtotal <= 0.0" car(maxX1))
      fprintf(p1 "\\ Constraint%d: %s\n" constraintCount constraint) 
      constraintString=flParseConstraint(constraint xVariableTable xReplaceValueTable ?SnapXGrid SnapXGrid)
      if( constraintString then 
        fprintf(p "  Constraint%d: %s\n" constraintCount constraintString) 
      else
        fprintf(p "\\ Constraint%d: \n" constraintCount)
      )
      constraintCount=constraintCount+1
    )
    fprintf(p1 "\n")
    fprintf(p "\n")

    for(i 0 length(xLeaf)-2
      instName1=car(xLeaf[i])
      cellName1=cadr(xLeaf[i])
      minX1=caddr(xLeaf[i])
      minY1=cadddr(xLeaf[i])
      maxX1=caddr(cddr(xLeaf[i]))
      maxY1=caddr(cdddr(xLeaf[i]))
      fullInstName1=cadddr(cdddr(xLeaf[i]))
      fullInstArrayName1=flGetArrayName(fullInstName1)
      for(j i+1 length(xLeaf)-1
        instName2=car(xLeaf[j])
        cellName2=cadr(xLeaf[j])
        minX2=caddr(xLeaf[j])
        minY2=cadddr(xLeaf[j])
        maxX2=caddr(cddr(xLeaf[j]))
        maxY2=caddr(cdddr(xLeaf[j]))
        fullInstName2=cadddr(cdddr(xLeaf[j]))
        fullInstArrayName2=flGetArrayName(fullInstName2)

        ; process leaf x contraints

        ; type 4: if two leaf cell has overlap Y, add an x constraint
        if(cadr(maxY1)>cadr(minY2)+0.001 && cadr(maxY2)>cadr(minY1)+0.001 then
          cond( 
            (cadr(minX1) < cadr(minX2) constraint= sprintf(nil "%s <= %s" car(maxX1) car(minX2)))
            (cadr(minX1) > cadr(minX2) constraint= sprintf(nil "%s >= %s" car(minX1) car(maxX2)))
            (t
              printf("Warning: %s %s overlap and have same x cordinate!\n" fullInstName1 fullInstName2 )
              cond(
                (cadr(maxX1) < cadr(maxX2) constraint= sprintf(nil "%s <= %s" car(maxX1) car(minX2))) 
                (cadr(maxX1) > cadr(maxX2) constraint= sprintf(nil "%s >= %s" car(minX1) car(maxX2))) 
                (t 
                   if( strcmp( fullInstArrayName1 fullInstArrayName2 )>0 then
                     constraint= sprintf(nil "%s <= %s" car(maxX1) car(minX2))
                   else
                     constraint= sprintf(nil "%s >= %s" car(minX1) car(maxX2))
                   )
                )
             ) 
            )
          )
          fprintf(p1 "\\ Constraint%d(type4): %s\n" constraintCount constraint) 
          constraintString=flParseConstraint(constraint xVariableTable xReplaceValueTable ?SnapXGrid SnapXGrid)
          if( constraintString then 
            fprintf(p "  Constraint%d: %s\n" constraintCount constraintString) 
          else
            fprintf(p "\\ Constraint%d: \n" constraintCount)
          )
          constraintCount=constraintCount+1
        ) ; type 4

        ; type 5: if two leaf cell has Y abut and different x, add an x constraint
        if( AbutCellKeepOrder && 
            (fullInstArrayName1==nil || fullInstArrayName2==nil || fullInstArrayName1!=fullInstArrayName2) &&
            (abs(cadr(maxY1)-cadr(minY2))<0.001 || abs(cadr(maxY2)-cadr(minY1))<0.001) &&
            (cadr(maxX1)>cadr(minX2)+0.001 && cadr(maxX2)>cadr(minX1)+0.001)
        then
;          fprintf(p1 "minX2: %f minX1: %f\n" cadr(minX2) cadr(minX1))
          if(abs(cadr(minX2)-cadr(minX1))<0.001 then
            constraint= sprintf(nil "%s = %s" car(minX1) car(minX2))
          )
          if(cadr(minX2)>cadr(minX1)+0.001 then
            constraint= sprintf(nil "%s <= %s" car(minX1) car(minX2))
          )
          if(cadr(minX1)>cadr(minX2)+0.001 then
            constraint= sprintf(nil "%s >= %s" car(minX1) car(minX2))
          )   
          fprintf(p1 "\\ Constraint%d(type5): %s\n" constraintCount constraint) 
          constraintString=flParseConstraint(constraint xVariableTable xReplaceValueTable ?SnapXGrid SnapXGrid)
          if( constraintString then 
            fprintf(p "  Constraint%d: %s\n" constraintCount constraintString) 
          else
            fprintf(p "\\ Constraint%d: \n" constraintCount)
          )
          constraintCount=constraintCount+1
        ) ; type 5
       
      )
    )

    if( xtotal != 0 then
      constraint= sprintf(nil "xtotal <= %f" xtotal+1e-4)
      fprintf(p1 "\\ Constraint%d: %s\n" constraintCount constraint)       
      fprintf(p "  Constraint%d: %s" constraintCount flParseConstraint(constraint xVariableTable xReplaceValueTable ?SnapXGrid SnapXGrid)) 
    )

    fprintf(p1 "\n")
    fprintf(p "\n")
    
    fprintf(p "\nBounds\n")
    foreach( vName uniqueVNames
      fprintf(p "  %s >= %f \n" vName xMinBound)
    )

    fprintf(p "End")

    close(p1)
    close(p)

    if( errorCount==0 then
      cmd=sprintf( nil "glpsol --lpt --min -o %s %s > %s" LPOutputFileName LPMacroFileName LPLogFileName)
      printf("Running %s\n" cmd)
      system(cmd)

      paraTable=makeTable("myTable" 0.0)
      foreach( vName xReplaceValueTable
        paraTable[vName]=xReplaceValueTable[vName]
      )
      p=infile(LPOutputFileName)
      if( p then
        gets(s p)
        while( !rexMatchp( "Status:" s) gets(s p))
        if( rexMatchp("OPTIMAL" s) then optimal=t else optimal=nil )
        while( !rexMatchp( "No. Column name  St   Activity     Lower bound   Upper bound    Marginal" s)
          gets(s p)
        )
        gets(s p)
        gets(s p)
        while(s
          if( rexMatchp("^[ \n]*$" s) then s=nil 
          else
            sscanf( s "%d %s %s %f" no vName st vPara )
            paraTable[vName]=vPara
            gets(s p)
          )  
        )
        close(p)

        xValueTable=makeTable("myTable" 0.0)
        foreach( vName xVariableTable
          xValueTable[vName]=paraTable[xVariableTable[vName]]
        )
        if(!optimal then
          printf("No Solution Found ...\n")
        else
          printf("Importing Coordinates ...\n")
          errorCount=flFillTopCellInfo(cellview xValueTable ?SaveCellView SaveCellView ?BlockLayer BlockLayer ?SkipCellList SkipCellList)
          if( errorCount==0 then
            printf("All cells has been updated.\n")
          else
            printf("Total %d errors found in importing coordinates. \n" errorCount)
          )
        ) 
      else
        printf("Error in running glpsol, please check %s\n" LPLogFileName)
      )
    else
      printf("Total %d errors found in generating contraints.\n" errorCount)
    )

    xValueTable["xtotal"]
  )
)


