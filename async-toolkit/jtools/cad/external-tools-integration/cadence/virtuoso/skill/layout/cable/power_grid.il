; Power Grid Constants
(defvar   VDDS_X  26.4  )
(defvar   VDDS_Y   8.8  )
(defvar   VDDF_X  26.4  )
(defvar   VDDF_Y   8.8  )
(defvar   SYNC_X   4.4  )
(defvar   SYNC_Y   8.8  )
(defvar  ASYNC_X   3.12 )
(defvar  ASYNC_Y   8.8  )


(defun VerifyPowerGuides ( @key (CV (geGetEditCellView)))
  (let ( 
         VDDS_guides 
         VDDF_guides 
         SYNC_guides 
        ASYNC_guides 
       )

;  Get Guides
    VDDS_guides = setof( CV CV~>shapes car(CV~>lpp) == "y0" )
    VDDF_guides = setof( CV CV~>shapes car(CV~>lpp) == "y2" )
    SYNC_guides = setof( CV CV~>shapes car(CV~>lpp) == "y1" )
   ASYNC_guides = setof( CV CV~>shapes car(CV~>lpp) == "y3" )

;  Verify off-grid

   VerifyGuideGrid(  VDDS_guides  "VDDS"  VDDS_X  VDDS_Y )
   VerifyGuideGrid(  VDDF_guides  "VDDF"  VDDF_X  VDDF_Y )
   VerifyGuideGrid(  SYNC_guides  "SYNC"  SYNC_X  SYNC_Y )
   VerifyGuideGrid( ASYNC_guides "ASYNC" ASYNC_X ASYNC_Y )

;  Verify overlaps
   
   VerifyGuideOverlaps(  VDDS_guides 
                         VDDF_guides 
                         SYNC_guides 
                        ASYNC_guides )

 )
)

(defun VerifyGuideGrid ( guides type gridX gridY @key ( correctOffGrid nil ))
  (let ( guide pt )
 
  foreach( guide guides 
     when( guide~>objType == "rect"    points = guide~>bBox ) 
     when( guide~>objType == "polygon" points = guide~>bBox ) 
     foreach( pt points 
      when( (CheckOffGrid pt ?XGrid gridX ?YGrid gridY ) 
       printf( "Guide point %L not on required %s grid\n" pt type ) )
     )
   )
t
 )
)

(defun VerifyGuideOverlaps ( y0 y2 y1 y3 @key (CV (geGetEditCellView)))
  (let (  vdd_ovp dom_ovp guide   
          sync_vdds_ovp   async_vdds_ovp 
          sync_vddf_ovp   async_vddf_ovp 

    )

;   Check Overlaps between VDDS & VDDF domains
    vdd_ovp = dbLayerAnd( CV "y8" y0 y2 )
    when( vdd_ovp 
     foreach( ovp vdd_ovp 
       when( ovp~>objType == "rect"
        printf("Overlaps found between VDDF & VDDS guides @ %L\n" car(ovp~>bBox)) )
       when( ovp~>objType == "polygon"
        printf("Overlaps found between VDDF & VDDS guides @ %L\n" nth(0 ovp~>points)) )
      )
     )

;   Check Overlaps between SYNC & ASYNC domains
    dom_ovp = dbLayerAnd( CV "y8" y1 y3 )
    when( dom_ovp 
     foreach( ovp dom_ovp 
       when( ovp~>objType == "rect"
        printf("Overlaps found between SYNC & ASYNC guides @ %L\n" car(ovp~>bBox)) )
       when( ovp~>objType == "polygon"
        printf("Overlaps found between SYNC & ASYNC guides @ %L\n" nth(0 ovp~>points)) )
      )
     )

;   Check guides SYNC guide overlapping both SYNC VDDS & VDDF domains
    foreach( guide y1 
       sync_vdds_ovp = dbLayerAnd( CV "y8" guide y0 )
       sync_vddf_ovp = dbLayerAnd( CV "y8" guide y2 )
       when( sync_vdds_ovp && sync_vddf_ovp 
        foreach( ovp append(sync_vdds_ovp sync_vddf_ovp)
          when( ovp~>objType == "rect"
           printf("SYNC guide overlaps VDDS & VDDF @ %L\n" car(ovp~>bBox)) )
          when( ovp~>objType == "polygon"
           printf("SYNC guide overlaps VDDS & VDDF @ %L\n" nth(0 ovp~>points)) )
       )
      )
     )

;   Check guides ASYNC guide overlapping both SYNC VDDS & VDDF domains
    foreach( guide y3 
       async_vdds_ovp = dbLayerAnd( CV "y8" y3 y0 )
       async_vddf_ovp = dbLayerAnd( CV "y8" y3 y2 )
       when( async_vdds_ovp && async_vddf_ovp 
        foreach( ovp append(async_vdds_ovp async_vddf_ovp)
          when( ovp~>objType == "rect"
           printf("ASYNC guide overlaps VDDS & VDDF @ %L\n" car(ovp~>bBox)) )
          when( ovp~>objType == "polygon"
           printf("ASYNC guide overlaps VDDS & VDDF @ %L\n" nth(0 ovp~>points)) )
       )
      )
     )

    foreach( ovp CV~>shapes when(ovp~>lpp==list("y8" "drawing") dbDeleteObject(ovp)))
t
 )
)


(defun MakeCablePowerGrid ( @key (CV (geGetEditCellView)))
 (let (   prBound box boxes 

       )

     prBound = CV~>prBoundary
     if(prBound 
      then
       foreach(pt prBound~>points 
         when((CheckOffGrid pt) (error "Not All prBoundary Vertices Are On 3.12u Grid: %L" pt))
           )
      else
        (error "prBoundary doesnot exist !")
      )

     boxes = convertPolyObjToRect(prBound)
     foreach( box boxes DrawPGTileShapes(box ?CV CV))

 )
)

(defun MakeTopPowerGrid ( @key (CV (geGetEditCellView)))
 (let (   prBound box boxes 
                             
       )
     prBound = CV~>prBoundary

     if(prBound 
      then
       foreach(pt prBound~>points 
         when((CheckOffGrid pt ?XGrid 4.4 ?YGrid 8.8 ) (error "Not All prBoundary Vertices Are On Avago Sync Grid: %L" pt))
           )
      else
        (error "prBoundary doesnot exist !")
      )

     boxes = convertPolyObjToRect(prBound)
     foreach( box boxes DrawPGTileShapes(box ?CV CV ?M3 nil ?M4 nil ?M5 nil ?M6 nil ?M7 nil ?M8 nil ?M9 t ?M10 nil ))


 )
)

(defun MakeSync89Grid ( guide @key (CV (geGetEditCellView)) (file nil) (idx nil))
 (let (   prBound box boxes cool pts )

    when( guide~>objType == "polygon" pts = guide~>points )
    when( guide~>objType == "rect"    pts = guide~>bBox )

    foreach(pt pts ;guide~>points 
      when( car(guide~>lpp) == "y0"  
       when(!(CheckOffGrid pt ?XGrid 4.4 ?YGrid 8.8 ) cool = t ); (error "Not All prBoundary Vertices Are On Avago Sync Grid: %L" pt))
         )
      when( car(guide~>lpp) == "y8"  
       when((CheckOffGrid round(car(pt))*1000:round(cadr(pt))*1000 ?XGrid 3120 ?YGrid 8800 ) cool = t ); (error "Not All prBoundary Vertices Are On Avago Sync Grid: %L" pt))
         )
      when(!cool dbCreateRect(CV "PO" list(pt car(pt)+100:cadr(pt)+100)) printf("%s %L Not Cool\n", car(guide~>lpp) pt))
       )

     if( guide~>objType == "polygon" then boxes = convertPolyObjToRect(guide) else boxes = list(guide~>bBox))

 
    foreach( box boxes 
     when( car(guide~>lpp) == "y0" && cool
        DrawPGTileShapes(box ?CV CV  ?sync89 t ?file file ?idx idx ?M3 nil ?M4 nil ?M5 nil ?M6 nil ?M7 nil ?M8 nil ?M9 t ?M10 nil )
       )
     when( car(guide~>lpp) == "y8" && cool
        DrawPGTileShapes(box ?CV CV ?async89 t ?file file ?idx idx ?M3 nil ?M4 nil ?M5 nil ?M6 nil ?M7 nil ?M8 nil ?M9 t ?M10 nil )
       )
     )

 )
)

(defun recurseM89pg ( @key (CV (geGetEditCellView)) )
  (let ( shp ptr file )
    
   ptr = outfile("VIA89_loc_new2.txt")
   fprintf(ptr "#source name    #loc_x #loc_y   #layer name     #P/G\n")
    foreach( shp CV~>shapes MakeSync89Grid(shp ?file ptr) )
   close(ptr)
 )
)

(defun opM89 ( @key (CV (geGetEditCellView)) )
 (let ( ptr )

    ptr = outfile("VIA89_loc_new.txt")
    fprintf(ptr "#source name    #loc_x #loc_y   #layer name     #P/G\n")
    vias = CV~>vias
    for(I 0 length(vias)-1
      via = nth(I vias)
      when(via~>net~>name == "GND" fprintf(file "GND.extra%d %f %f M8 GROUND\n" I car(via~>xy) cadr(via~>xy) ))
      when(via~>net~>name == "VDD" fprintf(file "VDD.extra%d %f %f M8 POWER \n" I car(via~>xy) cadr(via~>xy) ))
     )
  close(ptr)

t
 )
)

(defun DrawPGTileShapes ( bBox @key (CV (geGetEditCellView)) (sync nil) (sync89 nil) (async89 nil) (file nil) (idx nil)
                         (M3 nil) (M4 nil) (M5 nil) (M6 nil) (M7 nil) (M8 nil) (M9 t) (M10 t) )
;                        (M3 t) (M4 t) (M5 t) (M6 t) (M7 t) (M8 t) (M9 t) (M10 t) )
  (let (  llx figs_G0 fig_G0    width   llcorner  VIA34   M3_width   x       VIA89     rows_ASYNC 
          urx figs_G1 fig_G1    height  urcorner  VIA45   M3_offset  y       VIA910    cols_ASYNC 
          lly figs_V0 fig_V0    cols    I J       VIA56   GND_net    gnd_via rows_SYNC rows89_SYNC   
          ury figs_V1 fig_V1    rows    VIA78     VIA67   VDD_net    vdd_via cols_SYNC cols89_SYNC   
       )                                           
     llcorner   =   car(bBox)
     urcorner   =  cadr(bBox)
	   llx        =   car(llcorner)
	   urx        =   car(urcorner)	
	   lly        =  cadr(llcorner)
	   ury        =  cadr(urcorner)	
     width      =  urx - llx
     height     =  ury - lly
     cols       =  round(width/3.12 )
     rows       =  round(height/3.12)
     rows_SYNC  =  round(height/8.8 )
     cols_SYNC  =  round(width/4.4 )

     rows_ASYNC  =  round(height/8.8 )
     cols_ASYNC  =  round(width/3.12 )

println( rows_SYNC )
println( cols_SYNC )
     
     M3_width  =  0.07
     M3_offset =  0.03

     GND_net   =  dbFindNetByName( CV "GND")
     VDD_net   =  dbFindNetByName( CV "Vdd")
     if( !GND_net 
      then
       GND_net =  dbCreateNet( CV "GND" ) 
       GND_net~>term~>direction = "inputOutput"
      else
       dbFindNetByName( CV "GND")~>term~>direction = "inputOutput"
        )
     if( !VDD_net 
      then
       VDD_net =  dbCreateNet( CV "Vdd" ) 
       VDD_net~>term~>direction = "inputOutput"
      else
       dbFindNetByName( CV "Vdd")~>term~>direction = "inputOutput"
        )

     VIA34  =  techFindViaDefByName( TechLib "M4_M3_R_H" )
     VIA45  =  techFindViaDefByName( TechLib "M5_M4_R_V" )
     VIA56  =  techFindViaDefByName( TechLib "M6_M5_R_H" )
     VIA67  =  techFindViaDefByName( TechLib "M7_M6_R_V" )
     VIA78  =  techFindViaDefByName( TechLib "M8_M7_R_H" )
     VIA89  =  techFindViaDefByName( TechLib "M9_M8"     )
     VIA910 =  techFindViaDefByName( TechLib "M10_M9"    )
     
     for(I 0 rows-1
       when(M3
         when( I == 0 
            fig_Gextra  = dbCreateRect( CV list("M3" "gnd") list(llx-0.105:lly-M3_offset llx+0.105+width:lly-M3_width-M3_offset) )
            figs_Gextra = dbGetTrueOverlaps( CV fig_Gextra~>bBox list("M3" "gnd") )
            foreach( fig figs_Gextra when( fig~>pin dbSubFigFromPin( fig~>pin fig ) ) )
            fig_Gextra  = leMergeShapes( figs_Gextra )
            dbCreatePin( GND_net car(fig_Gextra) )
           )

         when( I == rows-1 
            fig_Gextra  = dbCreateRect( CV list("M3" "gnd") list(llx-0.105:lly+((I+1)*3.12)+M3_offset llx+0.105+width:lly+((I+1)*3.12)+M3_width+M3_offset) )
            figs_Gextra = dbGetTrueOverlaps( CV fig_Gextra~>bBox list("M3" "gnd") )
            foreach( fig figs_Gextra when( fig~>pin dbSubFigFromPin( fig~>pin fig ) ) )
            fig_Gextra  = leMergeShapes( figs_Gextra )
            dbCreatePin( GND_net car(fig_Gextra) )
           )

          fig_G0 = dbCreateRect( CV list("M3" "gnd") list(llx-0.105:lly+(I*3.12)+M3_offset       llx+0.105+width:lly+(I*3.12)+M3_width+M3_offset) )
          fig_G1 = dbCreateRect( CV list("M3" "gnd") list(llx-0.105:lly+((I+1)*3.12)-M3_offset   llx+0.105+width:lly+((I+1)*3.12)-M3_width-M3_offset) )
          fig_V0 = dbCreateRect( CV list("M3" "vdd") list(llx-0.105:lly+((I+0.5)*3.12)+M3_offset llx+0.105+width:lly+((I+0.5)*3.12)+M3_width+M3_offset) )
          fig_V1 = dbCreateRect( CV list("M3" "vdd") list(llx-0.105:lly+((I+0.5)*3.12)-M3_offset llx+0.105+width:lly+((I+0.5)*3.12)-M3_width-M3_offset) )
  
          figs_G0 = dbGetTrueOverlaps( CV fig_G0~>bBox list("M3" "gnd") )
          figs_G1 = dbGetTrueOverlaps( CV fig_G1~>bBox list("M3" "gnd") )
          figs_V0 = dbGetTrueOverlaps( CV fig_V0~>bBox list("M3" "vdd") )
          figs_V1 = dbGetTrueOverlaps( CV fig_V1~>bBox list("M3" "vdd") )
  
          foreach( fig figs_G0 when( fig~>pin dbSubFigFromPin( fig~>pin fig ) ) )
          foreach( fig figs_G1 when( fig~>pin dbSubFigFromPin( fig~>pin fig ) ) )
          foreach( fig figs_V0 when( fig~>pin dbSubFigFromPin( fig~>pin fig ) ) )
          foreach( fig figs_V1 when( fig~>pin dbSubFigFromPin( fig~>pin fig ) ) )
  
          fig_G0 = leMergeShapes( figs_G0 )
          fig_G1 = leMergeShapes( figs_G1 )
          fig_V0 = leMergeShapes( figs_V0 )
          fig_V1 = leMergeShapes( figs_V1 )
  
          dbAddFigToNet( car(fig_G0) GND_net )
          dbAddFigToNet( car(fig_G1) GND_net )
          dbAddFigToNet( car(fig_V0) VDD_net )
          dbAddFigToNet( car(fig_V1) VDD_net )
;         dbCreatePin( GND_net car(fig_G0) )
;         dbCreatePin( GND_net car(fig_G1) )
;         dbCreatePin( VDD_net car(fig_V0) )
;         dbCreatePin( VDD_net car(fig_V1) )
       )

  for( J 0 2*cols
       when(M4
          GenerateTile(  llx+(J*1.56)  lly+(I*3.12)      VIA34 "H" list("VIA3" "gnd") nil list("M4" "gnd") GND_net rows cols )
          GenerateTile(  llx+(J*1.56) lly+((I+0.5)*3.12) VIA34 "H" list("VIA3" "vdd") nil list("M4" "vdd") VDD_net rows cols )
         when( I == rows-1
          GenerateTile(  llx+(J*1.56) lly+((I+1)*3.12)   VIA34 "H" list("VIA3" "gnd") nil list("M4" "gnd") GND_net rows cols )
          )                                                                                                         
       )                                                                                                            
       when(M5                                                                                                      
          GenerateTile(  llx+(J*1.56)  lly+(I*3.12)      VIA45 "V" list("VIA4" "gnd") nil list("M5" "gnd") GND_net rows cols )
          GenerateTile(  llx+(J*1.56) lly+((I+0.5)*3.12) VIA45 "V" list("VIA4" "vdd") nil list("M5" "vdd") VDD_net rows cols )
         when( I == rows-1                                                                                          
          GenerateTile(  llx+(J*1.56) lly+((I+1)*3.12)   VIA45 "V" list("VIA4" "gnd") nil list("M5" "gnd") GND_net rows cols )
          )                                                                                                         
       )                                                                                                            
       when(M6                                                                                                      
          GenerateTile(  llx+(J*1.56)  lly+(I*3.12)      VIA56 "H" list("VIA5" "gnd") nil list("M6" "gnd") GND_net rows cols )
          GenerateTile(  llx+(J*1.56) lly+((I+0.5)*3.12) VIA56 "H" list("VIA5" "vdd") nil list("M6" "vdd") VDD_net rows cols )
         when( I == rows-1                                                                                          
          GenerateTile(  llx+(J*1.56) lly+((I+1)*3.12)   VIA56 "H" list("VIA5" "gnd") nil list("M6" "gnd") GND_net rows cols )
          )                                                                                                         
       )                                                                                                            
       when(M7                                                                                                      
          GenerateTile(  llx+(J*1.56)  lly+(I*3.12)      VIA67 "V" list("VIA6" "gnd") nil list("M7" "gnd") GND_net rows cols )
          GenerateTile(  llx+(J*1.56) lly+((I+0.5)*3.12) VIA67 "V" list("VIA6" "vdd") nil list("M7" "vdd") VDD_net rows cols )
         when( I == rows-1                                                                                          
          GenerateTile(  llx+(J*1.56) lly+((I+1)*3.12)   VIA67 "V" list("VIA6" "gnd") nil list("M7" "gnd") GND_net rows cols )
          )                                                                                                         
       )                                                                                                            
                                                                                                                    
       when(M8                                                                                                      
        if( J == 0                                                                                                  
         then                                                                                                       
          GenerateTile(  llx+(J*1.56)  lly+(I*3.12)      VIA78 "H" list("VIA7" "gnd") nil list("M8" "gnd") GND_net rows cols )
          GenerateTile(  llx+(J*1.56) lly+((I+0.5)*3.12) VIA78 "H" list("VIA7" "vdd") nil list("M8" "vdd") VDD_net rows cols )
         else                                                                                                      
          GenerateTile(  llx+(J*1.56)  lly+(I*3.12)      VIA78 "H" list("VIA7" "gnd") nil nil              GND_net rows cols )
          GenerateTile(  llx+(J*1.56) lly+((I+0.5)*3.12) VIA78 "H" list("VIA7" "vdd") nil nil              VDD_net rows cols )
          )                                                                                                        
         when( I == rows-1                                                                                         
          if( J == 0                                                                                               
           then                                                                                                    
            GenerateTile(  llx+(J*1.56) lly+((I+1)*3.12) VIA78 "H" list("VIA7" "gnd") nil list("M8" "gnd") GND_net rows cols )
           else                                                                                                     
            GenerateTile(  llx+(J*1.56) lly+((I+1)*3.12) VIA78 "H" list("VIA7" "gnd") nil nil              GND_net rows cols )
            )
           )
       )

      )
    )


    when( sync
     for(I 0 rows_SYNC-1
      for( J 0 cols_SYNC-1
       when(M9
        if( J == 0
         then
          GenerateTile(  llx+(J*4.4)  lly+(I*     8.8) VIA89 "H"  list("VIA8" "gnd")  nil list("M9" "gnd") GND_net rows_SYNC cols_SYNC )
          GenerateTile(  llx+(J*4.4) lly+((I+0.5)*8.8) VIA89 "H"  list("VIA8" "vdd")  nil list("M9" "vdd") VDD_net rows_SYNC cols_SYNC )
         else
          GenerateTile(  llx+(J*4.4)  lly+(I*     8.8) VIA89 "H"  list("VIA8" "gnd")  nil nil              GND_net rows_SYNC cols_SYNC )
          GenerateTile(  llx+(J*4.4) lly+((I+0.5)*8.8) VIA89 "H"  list("VIA8" "vdd")  nil nil              VDD_net rows_SYNC cols_SYNC )
          )
         when( I == rows_SYNC-1
          if( J == 0
           then
            GenerateTile(  llx+(J*4.4) lly+((I+1)*8.8) VIA89 "H"  list("VIA8" "gnd")  nil list("M9" "gnd") GND_net rows_SYNC cols_SYNC )
           else
            GenerateTile(  llx+(J*4.4) lly+((I+1)*8.8) VIA89 "H"  list("VIA8" "gnd")  nil nil              GND_net rows_SYNC cols_SYNC )
            )
           )
         )

       when(M10
            GenerateTile( llx+( J*     26.4)   lly+(I*8.8)    VIA910 "H" nil nil list("M10" "drawing") GND_net rows_SYNC cols_SYNC )
            GenerateTile( llx+((J+0.5)*26.4)   lly+(I*8.8)    VIA910 "H" nil nil list("M10" "drawing") VDD_net rows_SYNC cols_SYNC )
;      when( I == rows_SYNC-1
;         if( J == 0
;          then
;           GenerateTile( llx+((J+1)*  26.4)   lly+(I*26.4)    VIA910 "H" nil nil list("M10" "drawing") GND_net cols_SYNC )
;          else
;           GenerateTile( llx+((J+1)*  26.4)   lly+(I*26.4)    VIA910 "H" nil nil nil              GND_net cols_SYNC )
;           )
;          )
         )
      )

       )
     )

    when( sync89
    when(M9
     for(  I 0 rows_SYNC-1
      for( J 0 cols_SYNC-1
          x       =  llx+(J*4.4) 
          y       =  lly+(I*8.8) 
          gnd_via =  dbCreateVia( CV VIA89  x:y "R0" ) 
       when(file      fprintf(file "GND.extra%d %f %f M8 GROUND\n" (random) x y ))
          vdd_via =  dbCreateVia( CV VIA89  x+2.2:y+4.4 "R0" ) 
       when(file      fprintf(file "VDD.extra%d %f %f M8 POWER\n"  (random) x+2.2 y+4.4 ))
          dbAddFigToNet( gnd_via GND_net )
          dbAddFigToNet( vdd_via VDD_net )
         when( J == cols_SYNC-1
          gnd_via =  dbCreateVia( CV VIA89  x+4.4:y "R0" ) 
       when(file      fprintf(file "GND.extra%d %f %f M8 GROUND\n" (random) x+4.4 y ))
          dbAddFigToNet( gnd_via GND_net )
          )
         when( I == rows_SYNC-1
          gnd_via =  dbCreateVia( CV VIA89  x:y+8.8 "R0" ) 
       when(file      fprintf(file "GND.extra%d %f %f M8 GROUND\n" (random)  x y+8.8 ))
          dbAddFigToNet( gnd_via GND_net )
          )
        )
      )
      x       =  llx+(cols_SYNC*4.4) 
      y       =  lly+(rows_SYNC*8.8) 
      println(x:y)
      gnd_via =  dbCreateVia( CV VIA89  x:y "R0" )    
      dbAddFigToNet( gnd_via GND_net )
   when(file          fprintf(file "GND.extra%d %f %f M8 GROUND\n" (random) x y))
     )
   )

    when( async89
    when(M9
     for(  I 0 rows_ASYNC-1
      for( J 0 cols_ASYNC-1
          x       =  llx+(J*3.12) 
          y       =  lly+(I*8.8) 
          gnd_via =  dbCreateVia( CV VIA89  x:y "R0" ) 
       when(file      fprintf(file "GND.extra%d %f %f M8 GROUND\n" (random) x y ))
          vdd_via =  dbCreateVia( CV VIA89  x+1.56:y+4.4 "R0" ) 
       when(file      fprintf(file "VDD.extra%d %f %f M8 POWER\n"  (random) x+1.56 y+4.4 ))
          dbAddFigToNet( gnd_via GND_net )
          dbAddFigToNet( vdd_via VDD_net )
         when( J == cols_ASYNC-1
          gnd_via =  dbCreateVia( CV VIA89  x+3.12:y "R0" ) 
       when(file      fprintf(file "GND.extra%d %f %f M8 GROUND\n" (random) x+3.12 y ))
          dbAddFigToNet( gnd_via GND_net )
          )
         when( I == rows_ASYNC-1
          gnd_via =  dbCreateVia( CV VIA89  x:y+8.8 "R0" ) 
       when(file      fprintf(file "GND.extra%d %f %f M8 GROUND\n" (random) x y+8.8 ))
          dbAddFigToNet( gnd_via GND_net )
          )
        )
      )
      x       =  llx+(cols_ASYNC*3.12) 
      y       =  lly+(rows_ASYNC*8.8) 
      println(x:y)
      gnd_via =  dbCreateVia( CV VIA89  x:y "R0" )    
    when(file     fprintf(file "GND.extra%d %f %f M8 GROUND\n" (random) x y ))
      dbAddFigToNet( gnd_via GND_net )
     )
   )

 )
)

(defun Generate89Tile ( x y via_type orient vlayer llayer ulayer net rows cols @key (CV (geGetEditCellView)) (sync nil) )
 (let ( top_loc top_ovp l_shape   top_via_rect 
        bot_loc bot_ovp l_shapes  bot_via_rect 
        top_via via shp u_shape  
        bot_via I       u_shapes 
       )

      case( orient
       ( "H"
        top_loc       =  x : y+0.065 
        bot_loc       =  x : y-0.065 
        top_via_rect  =  list(x-0.065 : y+0.04  x+0.065 : y+0.09 )
        bot_via_rect  =  list(x-0.065 : y-0.04  x+0.065 : y-0.09 )
        )
       ( "V"
        top_loc       =  x+0.065 : y 
        bot_loc       =  x-0.065 : y 
        top_via_rect  =  list(x+0.04 : y-0.065  x+0.09 : y+0.065 )
        bot_via_rect  =  list(x-0.04 : y-0.065  x-0.09 : y+0.065 )
        )
      )

      when(vlayer 
         top_via =  dbCreateVia( CV via_type top_loc "R0" ) 
         bot_via =  dbCreateVia( CV via_type bot_loc "R0" ) 

       when(car(vlayer) == "VIA9" 
           top_via~>cutRows    = 4
           top_via~>cutSpacing = 0.44
           )

         dbAddFigToNet( top_via net )
         dbAddFigToNet( bot_via net )
        
;        top_via =  dbCreateRect( CV vlayer top_via_rect ) 
;        bot_via =  dbCreateRect( CV vlayer bot_via_rect ) 
;        dbCreatePin( net top_via )
;        dbCreatePin( net bot_via )
        
;        top_ovp = dbGetOverlaps( CV top_via~>bBox )
;        bot_ovp = dbGetOverlaps( CV bot_via~>bBox )
;        foreach(via remove( top_via top_ovp ) 
;          when( via~>viaHeader~>viaDefName == top_via~>viaHeader~>viaDefName dbDeleteObject( via ) ) 
;          )
;        foreach(via remove( bot_via bot_ovp ) 
;          when( via~>viaHeader~>viaDefName == bot_via~>viaHeader~>viaDefName dbDeleteObject( via ) ) 
;          )
         )

      when(llayer 
           l_shape  = dbCreateRect( CV llayer list( x-0.105:y-0.105 x+0.105:y+0.105 ) )
;          l_shapes = dbGetTrueOverlaps( CV l_shape~>bBox llayer )
;          foreach( shp remove( l_shape l_shapes ) when( shp~>lpp == l_shape~>lpp dbDeleteObject( shp ) ) )
           dbAddFigToNet( l_shape net  )
         )
      when(ulayer 
        case(car(ulayer) 
          ( "M3" || "M4" || "M5" || "M6" || "M7"
           u_shape  = dbCreateRect( CV ulayer list( x-0.105:y-0.105 x+0.105:y+0.105 ) )
;          u_shapes = dbGetTrueOverlaps( CV u_shape~>bBox ulayer )
;          foreach( shp remove( u_shape u_shapes ) when( shp~>lpp == u_shape~>lpp dbDeleteObject( shp ) ) )
           dbAddFigToNet( u_shape net )
          )
          ("M8" 
           u_shape  = dbCreateRect( CV ulayer list( x-0.35:y-0.4 x+(cols*3.12)+0.35:y+0.4 ) )
           u_shapes = dbGetTrueOverlaps( CV u_shape~>bBox ulayer )
;          foreach( shp remove( u_shape u_shapes ) when( shp~>lpp == u_shape~>lpp dbDeleteObject( shp ) ) )
           u_shape  = leMergeShapes( u_shapes )
           dbAddFigToNet( car(u_shape) net )
          )
          ("M9" 
           u_shape  = dbCreateRect( CV ulayer list( x-1.85:y-0.35 x+(cols_SYNC*26.4)+1.85:y+0.35 ) )
           u_shapes = dbGetTrueOverlaps( CV u_shape~>bBox ulayer )
           u_shape  = leMergeShapes( u_shapes )
           dbAddFigToNet( car(u_shape) net )
          )
          ("M10" 
           u_shape  = dbCreateRect( CV ulayer list( x-1.85:y-0.35 x+1.85:y+8.8+0.35 ) )
           u_shapes = dbGetTrueOverlaps( CV u_shape~>bBox ulayer )
           u_shape  = leMergeShapes( u_shapes )
           dbAddFigToNet( car(u_shape) net )
          )
         )
        )

 )
)



(defun GenerateTile ( x y via_type orient vlayer llayer ulayer net rows cols @key (CV (geGetEditCellView)) (sync nil) )
 (let ( top_loc top_ovp l_shape   top_via_rect 
        bot_loc bot_ovp l_shapes  bot_via_rect 
        top_via via shp u_shape  
        bot_via I       u_shapes 
       )

      case( orient
       ( "H"
        top_loc       =  x : y+0.065 
        bot_loc       =  x : y-0.065 
        top_via_rect  =  list(x-0.065 : y+0.04  x+0.065 : y+0.09 )
        bot_via_rect  =  list(x-0.065 : y-0.04  x+0.065 : y-0.09 )
        )
       ( "V"
        top_loc       =  x+0.065 : y 
        bot_loc       =  x-0.065 : y 
        top_via_rect  =  list(x+0.04 : y-0.065  x+0.09 : y+0.065 )
        bot_via_rect  =  list(x-0.04 : y-0.065  x-0.09 : y+0.065 )
        )
      )

      when(vlayer 
         top_via =  dbCreateVia( CV via_type top_loc "R0" ) 
         bot_via =  dbCreateVia( CV via_type bot_loc "R0" ) 

       when(car(vlayer) == "VIA9" 
           top_via~>cutRows    = 4
           top_via~>cutSpacing = 0.44
           )

         dbAddFigToNet( top_via net )
         dbAddFigToNet( bot_via net )
        
;        top_via =  dbCreateRect( CV vlayer top_via_rect ) 
;        bot_via =  dbCreateRect( CV vlayer bot_via_rect ) 
;        dbCreatePin( net top_via )
;        dbCreatePin( net bot_via )
        
;        top_ovp = dbGetOverlaps( CV top_via~>bBox )
;        bot_ovp = dbGetOverlaps( CV bot_via~>bBox )
;        foreach(via remove( top_via top_ovp ) 
;          when( via~>viaHeader~>viaDefName == top_via~>viaHeader~>viaDefName dbDeleteObject( via ) ) 
;          )
;        foreach(via remove( bot_via bot_ovp ) 
;          when( via~>viaHeader~>viaDefName == bot_via~>viaHeader~>viaDefName dbDeleteObject( via ) ) 
;          )
         )

      when(llayer 
           l_shape  = dbCreateRect( CV llayer list( x-0.105:y-0.105 x+0.105:y+0.105 ) )
;          l_shapes = dbGetTrueOverlaps( CV l_shape~>bBox llayer )
;          foreach( shp remove( l_shape l_shapes ) when( shp~>lpp == l_shape~>lpp dbDeleteObject( shp ) ) )
           dbAddFigToNet( l_shape net  )
         )
      when(ulayer 
        case(car(ulayer) 
          ( "M3" || "M4" || "M5" || "M6" || "M7"
           u_shape  = dbCreateRect( CV ulayer list( x-0.105:y-0.105 x+0.105:y+0.105 ) )
;          u_shapes = dbGetTrueOverlaps( CV u_shape~>bBox ulayer )
;          foreach( shp remove( u_shape u_shapes ) when( shp~>lpp == u_shape~>lpp dbDeleteObject( shp ) ) )
           dbAddFigToNet( u_shape net )
          )
          ("M8" 
           u_shape  = dbCreateRect( CV ulayer list( x-0.35:y-0.4 x+(cols*3.12)+0.35:y+0.4 ) )
           u_shapes = dbGetTrueOverlaps( CV u_shape~>bBox ulayer )
;          foreach( shp remove( u_shape u_shapes ) when( shp~>lpp == u_shape~>lpp dbDeleteObject( shp ) ) )
           u_shape  = leMergeShapes( u_shapes )
           dbAddFigToNet( car(u_shape) net )
          )
          ("M9" 
           u_shape  = dbCreateRect( CV ulayer list( x-1.85:y-0.35 x+(cols_SYNC*26.4)+1.85:y+0.35 ) )
           u_shapes = dbGetTrueOverlaps( CV u_shape~>bBox ulayer )
           u_shape  = leMergeShapes( u_shapes )
           dbAddFigToNet( car(u_shape) net )
          )
          ("M10" 
           u_shape  = dbCreateRect( CV ulayer list( x-1.85:y-0.35 x+1.85:y+8.8+0.35 ) )
           u_shapes = dbGetTrueOverlaps( CV u_shape~>bBox ulayer )
           u_shape  = leMergeShapes( u_shapes )
           dbAddFigToNet( car(u_shape) net )
          )
         )
        )

 )
)

;(defun GenerateTile ( x y via_type orient llayer ulayer net @key (CV (geGetEditCellView)) )
; (let ( top_loc top_ovp l_shape   
;        bot_loc bot_ovp l_shapes 
;        top_via via shp u_shape  
;        bot_via I       u_shapes 
;       )
;
;      case( orient
;       ( "H"
;        top_loc =  x+(I*1.56) : y+0.065 
;        bot_loc =  x+(I*1.56) : y-0.065 
;        )
;       ( "V"
;        top_loc =  x+(I*1.56)+0.065 : y 
;        bot_loc =  x+(I*1.56)-0.065 : y 
;        )
;      )
;      top_via =  dbCreateVia( CV via_type top_loc "R0" ) 
;      bot_via =  dbCreateVia( CV via_type bot_loc "R0" ) 
;      dbAddFigToNet( top_via net )
;      dbAddFigToNet( bot_via net )
;
;      top_ovp = dbGetOverlaps( CV top_via~>bBox )
;      bot_ovp = dbGetOverlaps( CV bot_via~>bBox )
;      foreach(via remove( top_via top_ovp ) 
;        when( via~>viaHeader~>viaDefName == top_via~>viaHeader~>viaDefName dbDeleteObject( via ) ) 
;        )
;      foreach(via remove( bot_via bot_ovp ) 
;        when( via~>viaHeader~>viaDefName == bot_via~>viaHeader~>viaDefName dbDeleteObject( via ) ) 
;        )
;
;      when(llayer 
;           l_shape  = dbCreateRect( CV llayer list( x+(I*1.56)-0.105:y-0.105 x+(I*1.56)+0.105:y+0.105 ) )
;           l_shapes = dbGetTrueOverlaps( CV l_shape~>bBox llayer )
;           foreach( shp remove( l_shape l_shapes ) when( shp~>lpp == l_shape~>lpp dbDeleteObject( shp ) ) )
;;          l_shape  = leMergeShapes( l_shapes )
;;          dbCreatePin( GND_net car(l_shape) )
;           dbCreatePin( GND_net l_shape )
;         )
;      when(ulayer 
;           u_shape  = dbCreateRect( CV ulayer list( x+(I*1.56)-0.105:y-0.105 x+(I*1.56)+0.105:y+0.105 ) )
;           u_shapes = dbGetTrueOverlaps( CV u_shape~>bBox ulayer )
;           foreach( shp remove( u_shape u_shapes ) when( shp~>lpp == u_shape~>lpp dbDeleteObject( shp ) ) )
;;          u_shape  = leMergeShapes( u_shapes )
;;          dbCreatePin( GND_net car(u_shape) )
;           dbCreatePin( GND_net u_shape )
;         )
;
; )
;)

;      when(M5
;         GenerateTile(  llx lly+(I*3.12)       VIA45 "V" list("M4" "gnd") list("M5" "gnd") GND_net )
;         GenerateTile(  llx lly+((I+0.5)*3.12) VIA45 "V" list("M4" "vdd") list("M5" "vdd") VDD_net )
;        when( I == rows-1                                                     
;         GenerateTile(  llx lly+((I+1)*3.12)   VIA45 "V" list("M4" "gnd") list("M5" "gnd") GND_net )
;         )
;      )
;      when(M6
;         GenerateTile(  llx lly+(I*3.12)       VIA56 "H" list("M5" "gnd") list("M6" "gnd") GND_net )
;         GenerateTile(  llx lly+((I+0.5)*3.12) VIA56 "H" list("M5" "vdd") list("M6" "vdd") VDD_net )
;        when( I == rows-1
;         GenerateTile(  llx lly+((I+1)*3.12)   VIA56 "H" list("M5" "gnd") list("M6" "gnd") GND_net )
;         )
;      )
;      when(M7
;         GenerateTile(  llx lly+(I*3.12)       VIA67 "V" list("M6" "gnd") list("M7" "gnd") GND_net )
;         GenerateTile(  llx lly+((I+0.5)*3.12) VIA67 "V" list("M6" "vdd") list("M7" "vdd") VDD_net )
;        when( I == rows-1
;         GenerateTile(  llx lly+((I+1)*3.12)   VIA67 "V" list("M6" "gnd") list("M7" "gnd") GND_net )
;         )
;      )
;      when(M8
;         GenerateTile(  llx lly+(I*3.12)       VIA78 "H" list("M7" "gnd") list("M8" "gnd") GND_net )
;         GenerateTile(  llx lly+((I+0.5)*3.12) VIA78 "H" list("M7" "vdd") list("M8" "vdd") VDD_net )
;        when( I == rows-1
;         GenerateTile(  llx lly+((I+1)*3.12)   VIA78 "H" list("M7" "gnd") list("M8" "gnd") GND_net )
;         )
;      )


;(defun fixAsyncGuide ()
; (let (  )
;
;  
;  for( I 0 length(guide~>points)-1
;    pt = nth(I guide~>points)
;    when( CheckOffGrid( pt ?XGrid ASYNC_X ?YGrid ASYNC_Y ) 
;        
;      )
;    )
;
; )
;)
