; Copyright 2007 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "Diff Layout" "UIDiffLayout" )


defun( nrDiffLayout ( CellView1 CellView2 p_out )
  let((instTable1 instTable2 inst instName 
       shapeTable1 shapeTable2 shape shapeID
       countInst1 countInst2 countShape1 countShape2)
    instTable1=makeTable(`instTable nil)
    instTable2=makeTable(`instTable nil)
    fprintf(p_out "Layout1: %L(%L)\n vs  \nLayout2: %L(%L) ...\n" CellView1~>cellName CellView1~>viewName CellView2~>cellName CellView2~>viewName)
    printf("Layout1: %L(%L)\n vs \nLayout2: %L(%L) ...\n" CellView1~>cellName CellView1~>viewName CellView2~>cellName CellView2~>viewName)
    fprintf(p_out "Comparing instances...\n")
    printf("Comparing instances...\n")
    foreach( inst CellView1~>instances instTable1[inst~>name]=inst )
    foreach( inst CellView2~>instances instTable2[inst~>name]=inst )
    foreach( instName instTable1  
      if( instTable2[instName] then
        cond( 
          ( instTable1[instName]~>cellName!=instTable2[instName]~>cellName
            fprintf(p_out " %L has different cellName %L %L\n" instName instTable1[instName]~>cellName instTable2[instName]~>cellName)
          )
          ( instTable1[instName]~>orient!=instTable2[instName]~>orient
            fprintf(p_out " %L %L has different orient %L %L\n" instName instTable1[instName]~>cellName instTable1[instName]~>orient instTable2[instName]~>orient)
          )
          ( instTable1[instName]~>xy!=instTable2[instName]~>xy
            fprintf(p_out " %L %L has different xy %L %L\n" instName instTable1[instName]~>cellName instTable1[instName]~>xy instTable2[instName]~>xy)
          )
          ( t instTable1[instName]=nil instTable2[instName]=nil )
        )
      )
    )
    printf("Comparing shapes...\n")
    shapeTable1=makeTable(`shapeTable nil)
    shapeTable2=makeTable(`shapeTable nil)
    foreach( shape CellView1~>shapes
      shapeID= list( shape~>layerName shape~>purpose shape~>objType shape~>bBox shape~>points)
      shapeTable1[shapeID]=shape
    )
    foreach( shape CellView2~>shapes
      shapeID= list( shape~>layerName shape~>purpose shape~>objType shape~>bBox shape~>points)
      shapeTable2[shapeID]=shape
    )
    foreach( shapeID shapeTable1 
      if( shapeTable2[shapeID] then
        shapeTable1[shapeID]=nil shapeTable2[shapeID]=nil 
      )   
    )
    countInst1=0 
    countInst2=0 
    countShape1=0
    countShape2=0
    fprintf(p_out "\nSUMMARY:\n\n")
    fprintf(p_out "CellView1: %L %L %L\n" CellView1~>libName CellView1~>cellName CellView1~>viewName) 
    foreach( instName instTable1 
      if( instTable1[instName] then 
        fprintf(p_out "  instName: %L cellName: %L xy: %L orient %L\n" 
            instName 
            instTable1[instName]~>cellName 
            if(instTable1[instName]~>objType=="mosaicInst" then
              instTable1[instName]~>mosaic~>xy
            else
              instTable1[instName]~>xy
            ) 
            if(instTable1[instName]~>objType=="mosaicInst" then
              sprintf( nil "mosaic columns=%d rows=%d" instTable1[instName]~>mosaic~>columns instTable1[instName]~>mosaic~>rows)
            else
              instTable1[instName]~>orient
            ) 
        )
        countInst1++         
      )
    )
    fprintf(p_out "Unique shapes:\n")    
    foreach( shapeID shapeTable1
      if( shapeTable1[shapeID] then 
        fprintf(p_out "  %L\n" shapeID )
        countShape1++         
      )
    )
    fprintf(p_out "\nCellView2: %L %L %L\n" CellView2~>libName CellView2~>cellName CellView2~>viewName) 
    foreach( instName instTable2 
      if( instTable2[instName] then 
        fprintf(p_out "  instName: %L cellName: %L xy: %L orient %L\n" 
            instName 
            instTable2[instName]~>cellName 
            if(instTable2[instName]~>objType=="mosaicInst" then
              instTable2[instName]~>mosaic~>xy
            else
              instTable2[instName]~>xy
            ) 
            if(instTable2[instName]~>objType=="mosaicInst" then
              sprintf( nil "mosaic columns=%d rows=%d" instTable2[instName]~>mosaic~>columns instTable2[instName]~>mosaic~>rows)
            else
              instTable2[instName]~>orient
            ) 
        )
        countInst2++         
      )
    )
    fprintf(p_out "Unique shapes:\n")    
    foreach( shapeID shapeTable2
      if( shapeTable2[shapeID] then 
        fprintf(p_out "  %L\n" shapeID )
        countShape2++         
      )
    )
    printf("CellView1 unique inst count: %d\n" countInst1)
    printf("CellView2 unique inst count: %d\n" countInst2)
    printf("CellView1 unique shape count: %d\n" countShape1)
    printf("CellView2 unique shape count: %d\n" countShape2)
  )
)

( UICreateMenuItemCallBack 
  `UIDiffLayoutCB
  (lambda () 
    let((libName cellName)
      libName = UIGetCellView()~>libName
      cellName = UIGetCellView()~>cellName
      UIDiffLayout_Form->UIDiffLayout_LibName1->value=libName
      UIDiffLayout_Form->UIDiffLayout_LibName2->value=libName
      UIDiffLayout_Form->UIDiffLayout_CellName1->value=cellName
      UIDiffLayout_Form->UIDiffLayout_CellName2->value=cellName
      hiDisplayForm( UIDiffLayout_Form ) 
    ) 
  ) 
)

( UICreateAction
  'UIDiffLayoutFormCB
  (lambda ()
    (let ( cv1 cv2 p_out )
      CellView=UIGetCellView()
      cv1=nrOpenCellViewReadable(UIDiffLayout_Form->UIDiffLayout_LibName1->value
             UIDiffLayout_Form->UIDiffLayout_CellName1->value
             UIDiffLayout_Form->UIDiffLayout_ViewName1->value
          )
      cv2=nrOpenCellViewReadable(UIDiffLayout_Form->UIDiffLayout_LibName2->value
             UIDiffLayout_Form->UIDiffLayout_CellName2->value
             UIDiffLayout_Form->UIDiffLayout_ViewName2->value
          )
       p_out=outfile(UIDiffLayout_Form->UIDiffLayout_LogFilename->value)
       cond(
         ( cv1==nil printf("Can not open %s(%s) for read.\n"              
                                 UIDiffLayout_Form->UIDiffLayout_CellName1->value
                                 UIDiffLayout_Form->UIDiffLayout_ViewName1->value ))
         ( cv2==nil printf("Can not open %s(%s) for read.\n"              
                                 UIDiffLayout_Form->UIDiffLayout_CellName1->value
                                 UIDiffLayout_Form->UIDiffLayout_ViewName1->value ))
         ( p_out==nil printf("Can not open %s for write.\n" UIDiffLayout_Form->UIDiffLayout_LogFilename->value ))      
         ( t nrDiffLayout( cv1 cv2 p_out ))
       )
       close(p_out)
    ) 
  ) 
)
    
( UICreateComponent
  "UIDiffLayout_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIDiffLayout_LibName1
                  ?prompt "LibName 1"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIDiffLayout_CellName1
                  ?prompt "CellName 1"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIDiffLayout_ViewName1
                  ?prompt "ViewName 1"
                  ?defValue "layout" )
                ( hiCreateStringField
                  ?name `UIDiffLayout_LibName2
                  ?prompt "LibName 2"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIDiffLayout_CellName2
                  ?prompt "CellName 2"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIDiffLayout_ViewName2
                  ?prompt "ViewName 2"
                  ?defValue "layout_pg" )
                ( hiCreateStringField
                  ?name `UIDiffLayout_LogFilename
                  ?prompt "Compare Log"
                  ?defValue "compare.log" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIDiffLayout_Form
          ?formTitle "Diff Layout"  
          ?fields Fields
          ?callback "( UIDiffLayoutFormCB )"
          ) ) )
    ) )
        
 
