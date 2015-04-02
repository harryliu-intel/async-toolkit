(defun createCableLayout ( cableGuide @key (CV (geGetEditCellView)) (layoutView "cable"))
 (let ( 
        spec_root spec_temp 
        channelName  channelSBuf sbuf
        channelWidth channelRBuf mbuf
         adpL_CV  adpL_cell channelHops  channelBuf  channelSpec channelWidth2
         adpR_CV  adpR_cell  sbuf_cell   sbuf_CV
        mbufL_CV mbufL_cell  rbuf_cell   rbuf_CV
        mbufR_CV mbufR_cell  mbuf_cell   mbuf_CV
        cable_CV cable_cell cable_cell  cable_CV
       )
 
       spec_root       =  "/nfs/site/disks/wdisk.83/pankala1/rrc_1/spec"
       channelName     =  cableGuide~>ChannelName
       channelWidth    =  cableGuide~>ChannelWidth
       channelHops     =  cableGuide~>ChannelHops
       channelBuf      =  cableGuide~>ChannelBuf
       client_spec     =  ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
       channelWidth2   =  round((channelWidth+1)/2)
 
       case( channelBuf
         (  "DENSE" || "NYB" || "TS"
          channelSBuf     =  cableGuide~>ChannelSbufOrient ; sbuf_orient
          channelRBuf     =  cableGuide~>ChannelRbufOrient ; rbuf_orient
;         channelSBuf     =  cableGuide~>sbuf_orient
;         channelRBuf     =  cableGuide~>rbuf_orient
         )
         ( "SYNC_NODE" 
         channelSBuf     =  ""
         channelRBuf     =  ""
         )
       )

       case( channelBuf
         (  "DENSE" || "NYB" || "TS"

          if(channelBuf then buf = channelBuf else buf = "NYB")
          if(channelBuf=="DENSE" then buf = "" else buf = strcat(buf "_"))
   
          if(!channelSBuf then sbuf="1000_H" else sbuf=channelSBuf)
          if(!channelRBuf then rbuf="1000_H" else rbuf=channelRBuf)
          spec_temp = parseString(lowerCase(channelName) "[]")
          if(length(spec_temp)>1
            then        
              channelSpec = strcat(car(spec_temp) "_" cadr(spec_temp))
            else
              channelSpec = lowerCase(channelName)
            )

          sprintf( sbuf_cell  "chip.rrc.util.cable.%sCABLE_SBUF-L%d-R.%s" buf channelWidth sbuf) 
          sprintf( rbuf_cell  "chip.rrc.util.cable.%sCABLE_RBUF-L%d-R.%s" buf channelWidth rbuf) 
          sprintf( mbuf_cell  "chip.rrc.util.cable.%sCABLE_MBUFS-L%d_%d-R.%s"  buf channelHops channelWidth2 channelSpec) 
          sprintf(cable_cell  "chip.rrc.util.cable.%sCABLE-L%d_%d-R.%s"        buf channelHops channelWidth  channelSpec) 
           
          println( sbuf_cell )
          println( rbuf_cell )
          println( mbuf_cell )
          println(cable_cell )
           
            sbuf_CV  =   nrOpenCellViewWritableByName( sbuf_cell "cable")
            rbuf_CV  =   nrOpenCellViewWritableByName( rbuf_cell "cable")
            mbuf_CV  =   nrOpenCellViewWritableByName( mbuf_cell "cable")
           cable_CV  =   nrOpenCellViewWritableByName(cable_cell "cable")
          
          nrSyncToNetlist( sbuf_CV)
          nrSyncToNetlist( rbuf_CV)
          nrSyncToNetlist( mbuf_CV)
          nrSyncToNetlist(cable_CV)
         
          SetCableCellProp(mbuf_CV "TRUE")
          CreateCable(cableGuide mbuf_CV)
         
          dbSave( sbuf_CV)
          dbSave( rbuf_CV)
          dbSave( mbuf_CV)
          dbSave(cable_CV)
         
          dbClose( sbuf_CV)
          dbClose( rbuf_CV)
          dbClose( mbuf_CV)
          dbClose(cable_CV)
         )
         (  "TS_18"

          if(channelBuf then buf = channelBuf else buf = "NYB")
          if(channelBuf=="DENSE" then buf = "" else buf = strcat(buf "_"))
   
          if(!channelSBuf then sbuf="1000_H" else sbuf=channelSBuf)
          if(!channelRBuf then rbuf="1000_H" else rbuf=channelRBuf)
          spec_temp = parseString(lowerCase(channelName) "[]")
          if(length(spec_temp)>1
            then        
              channelSpec = strcat(car(spec_temp) "_" cadr(spec_temp))
            else
              channelSpec = lowerCase(channelName)
            )

          sprintf( sbuf_cell  "chip.rrc.util.cable.TS_CABLE_SBUF_18-L%d-R.%s"  channelWidth sbuf) 
          sprintf( rbuf_cell  "chip.rrc.util.cable.TS_CABLE_RBUF-L%d-R.%s"  channelWidth rbuf) 
          sprintf( mbuf_cell  "chip.rrc.util.cable.TS_CABLE_MBUFS-L%d_%d-R.%s"   channelHops channelWidth2 channelSpec) 
          sprintf(cable_cell  "chip.rrc.util.cable.TS_CABLE_18-L%d_%d-R.%s"         channelHops channelWidth  channelSpec) 
           
          println( sbuf_cell )
          println( rbuf_cell )
          println( mbuf_cell )
          println(cable_cell )
           
            sbuf_CV  =   nrOpenCellViewWritableByName( sbuf_cell "cable")
            rbuf_CV  =   nrOpenCellViewWritableByName( rbuf_cell "cable")
            mbuf_CV  =   nrOpenCellViewWritableByName( mbuf_cell "cable")
           cable_CV  =   nrOpenCellViewWritableByName(cable_cell "cable")
          
          nrSyncToNetlist( sbuf_CV)
          nrSyncToNetlist( rbuf_CV)
          nrSyncToNetlist( mbuf_CV)
          nrSyncToNetlist(cable_CV)
         
          SetCableCellProp(mbuf_CV "TRUE")
          CreateCable(cableGuide mbuf_CV)
         
          dbSave( sbuf_CV)
          dbSave( rbuf_CV)
          dbSave( mbuf_CV)
          dbSave(cable_CV)
         
          dbClose( sbuf_CV)
          dbClose( rbuf_CV)
          dbClose( mbuf_CV)
          dbClose(cable_CV)
         )
         (  "XBUS"

          xadpL           =  cableGuide~>XBUS_Adapter_L   
          xadpR           =  cableGuide~>XBUS_Adapter_R   
          xchannelHopsL   =  cableGuide~>XBUS_L_ChannelHops
          xchannelHopsR   =  cableGuide~>XBUS_R_ChannelHops
          channelName = lowerCase(channelName)
          spec_temp   = parseString( channelName "[]" )
          spec_temp2  = parseString( channelName "_[]" )
          
          when( nth(0 spec_temp2) == "rx" || nth(0 spec_temp2) == "tx"
             rxchannelSpec = strcat("rx_xbus_port_" cadr(spec_temp))
             txchannelSpec = strcat("tx_xbus_port_" cadr(spec_temp))
             channelSpec   = strcat("xbus_port_"    cadr(spec_temp))
           )
          when( nth(0 parseString( channelName "_[]" )) == "rx"  rxchannelSpec = strcat(car(spec_temp) "_" cadr(spec_temp)))  


          sprintf(  adpL_cell "chip.rrc.util.cable.CABLE_SBUF_XbusPortHalf.%s"  xadpL )
          sprintf(  adpR_cell "chip.rrc.util.cable.CABLE_RBUF_XbusPortHalf.%s"  xadpR )
          sprintf( mbufL_cell "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s" xchannelHopsL txchannelSpec )
          sprintf( mbufR_cell "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s" xchannelHopsR rxchannelSpec )
          sprintf( cable_cell "chip.rrc.util.cable.CABLE_XbusPort-L%d_%d-R.%s"  xchannelHopsL xchannelHopsR channelSpec )
           
          println(  adpL_cell )
          println(  adpR_cell )
          println( mbufL_cell )
          println( mbufR_cell )
          println( cable_cell )
           
             adpL_CV  =  nrOpenCellViewWritableByName(  adpL_cell "cable")
             adpR_CV  =  nrOpenCellViewWritableByName(  adpR_cell "cable")
            cable_CV  =  nrOpenCellViewWritableByName( cable_cell "cable")
          
          nrSyncToNetlist(  adpL_CV )
          nrSyncToNetlist(  adpR_CV )
         
          if(  nth(0 spec_temp2) == "tx" 
           then
            mbufL_CV  =  nrOpenCellViewWritableByName( mbufL_cell "cable")
            nrSyncToNetlist( mbufL_CV )
          SetCableCellProp(mbufL_CV "TRUE")
             CreateCable(cableGuide mbufL_CV )
          dbSave( mbufL_CV )
          dbClose( mbufL_CV )
           else 
            mbufR_CV  =  nrOpenCellViewWritableByName( mbufR_cell "cable")
            nrSyncToNetlist( mbufR_CV )
          SetCableCellProp(mbufR_CV "TRUE")
             CreateCable(cableGuide mbufR_CV )
          dbSave( mbufR_CV )
          dbClose( mbufR_CV )
           )
         
    ;      nrSyncToNetlist( cable_CV )

          dbSave(  adpL_CV )
          dbSave(  adpR_CV )
          dbSave( cable_CV )
         
          dbClose(  adpL_CV )
          dbClose(  adpR_CV )
          dbClose( cable_CV )

          printf( "chip.rrc.util.cable.CABLE_SBUF_XbusPortHalf.%s\n"  xadpL )
          printf( "chip.rrc.util.cable.CABLE_RBUF_XbusPortHalf.%s\n"  xadpR )
          printf( "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s\n" xchannelHopsL txchannelSpec )
          printf( "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s\n" xchannelHopsR rxchannelSpec )
          printf( "chip.rrc.util.cable.CABLE_XbusPort-L%d_%d-R.%s\n"  xchannelHopsL xchannelHopsR channelSpec )

         )
         (  "SYNC_NODE"

          if(channelBuf then buf = channelBuf else buf = "NYB")
          if(channelBuf=="DENSE" then buf = "" else buf = strcat(buf "_"))
   
          if(!channelSBuf then sbuf=sbuf_orient else sbuf=channelSBuf)
          if(!channelRBuf then rbuf=rbuf_orient else rbuf=channelRBuf)
          spec_temp = parseString(lowerCase(channelName) "[]")
          if(length(spec_temp)>1
            then        
              channelSpec = strcat(car(spec_temp) "_" cadr(spec_temp))
            else
              channelSpec = lowerCase(channelName)
            )

          sprintf(cable_cell  "chip.rrc.util.cable.%sCABLE-L%d_%d-R.%s" buf channelHops channelWidth  channelSpec) 
           
          println(cable_cell )
           
          cable_CV  =   nrOpenCellViewWritableByName(cable_cell layoutView)
          
          nrSyncToNetlist(cable_CV)
         
          SetCableCellProp(cable_CV "TRUE")
          CreateCable(cableGuide cable_CV)
         
          dbSave(cable_CV)
         
          dbClose(cable_CV)
          dbPurge(cable_CV)
         )
      )
;     dbPurge( sbuf_CV)
;     dbPurge( rbuf_CV)
;     dbPurge( mbuf_CV)
;     dbPurge(cable_CV)

 )
)

(defun CreateCableGuide ( cableGuide @key (CV (geGetEditCellView)))
  (let (  pts     width  I
          new_pts offset new_path
          cls     mod_pt
        )

   pts    =   cableGuide~>points 
   new_pts =   nil 
   cls    =   ClassifyPoints(pts)
   width  =   CBLGetPathWidth(?CableShape cableGuide)
   offset =   width*TrackPitch - (TrackPitch/2) 

   for( I 0 length(pts)-1
     case(nth(I cls)
        ( "HE" || "EH" 
          mod_pt = car(nth(I pts)):cadr(nth(I pts))-offset
        )
        ( "VE" || "EV" 
          mod_pt = car(nth(I pts))-offset:cadr(nth(I pts))
        )
        ( "HV" || "VH" 
          mod_pt = car(nth(I pts))-offset:cadr(nth(I pts))-offset
        )
      )
      new_pts = append(new_pts list(mod_pt))
     )

   new_path = dbCreatePath(CV list("M8" "bus") new_pts 0.78)
   dbCopyProp(cableGuide new_path)

new_path
 )
)

(defun CreateCable ( CableGuide CV  @key (Route nil) )
  (let (  newCableGuide
          newCableShape
   )

   newCableGuide = dbCopyShape( CableGuide CV )
   newCableShape = CreateCableGuide(newCableGuide ?CV CV)

   CreateCableProps(?paths list(newCableShape) ?CV CV)

   PlaceSyncCableBuffers(?CableShape newCableShape ?CV CV)
   when( Route RouteSyncCableBuffers(?CableShape newCableShape ?CV CV) )
   println(CV~>cellName)

t
 )
)

(defun createCableFloorplan ( cableGuide  @key (CV (geGetEditCellView)) (fp t) (ft nil))
 (let ( 
        spec_root spec_temp  newCV sh cbl
        channelName  channelSBuf sbuf cabCV cab_CV
        channelWidth channelRBuf mbuf
         adpL_CV  adpL_cell channelHops  channelBuf  channelSpec channelWidth2
         adpR_CV  adpR_cell  sbuf_cell   sbuf_CV             lcab_CV   
        mbufL_CV mbufL_cell  rbuf_cell   rbuf_CV             rcab_CV 
        mbufR_CV mbufR_cell  mbuf_cell   mbuf_CV lnewCV      cblL    
        cable_CV cable_cell cable_cell  cable_CV             cblR    
       )
 
       spec_root       =  "/nfs/site/disks/wdisk.83/pankala1/rrc_1/spec"
       channelName     =  cableGuide~>ChannelName
       channelWidth    =  cableGuide~>ChannelWidth
       channelHops     =  cableGuide~>ChannelHops
       channelBuf      =  cableGuide~>ChannelBuf
       client_spec     =  ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
       channelWidth2   =  round((channelWidth+1)/2)
 
       case( channelBuf
         (  "DENSE" || "NYB" || "TS"
         channelSBuf     =  cableGuide~>sbuf_orient
         channelRBuf     =  cableGuide~>rbuf_orient
         )
         ( "SYNC_NODE" 
         channelSBuf     =  ""
         channelRBuf     =  ""
         )
       )

          spec_temp = parseString(lowerCase(channelName) "[]")
          if(length(spec_temp)>1
            then        
              channelSpec = strcat(car(spec_temp) "_" cadr(spec_temp))
            else
              channelSpec = lowerCase(channelName)
            )  
          if(channelBuf then buf = channelBuf else buf = "NYB")
          if(channelBuf=="DENSE" then buf = "" else buf = strcat(buf "_"))

  case(channelBuf
  (  "DENSE" || "NYB" || "TS"
   sprintf( mbuf_cell  "chip.rrc.util.cable.%sCABLE_MBUFS-L%d_%d-R.%s"  buf channelHops channelWidth2 channelSpec) 
   sprintf(cable_cell  "chip.rrc.util.cable.%sCABLE-L%d_%d-R.%s"        buf channelHops channelWidth  channelSpec) 

   mbuf_CV  =   nrOpenCellViewReadableByName(  mbuf_cell  "floorplan")
   newCV  =     nrOpenCellViewReadableByName( cable_cell  "floorplan")

  when( mbuf_CV  CDSP4CellView(mbuf_CV "edit")   dbClose(mbuf_CV) )
  when( newCV  CDSP4CellView(newCV   "edit")   dbClose(newCV)   )
   mbuf_CV  =   nrOpenCellViewReadableByName(  mbuf_cell  "floorplan")
  when( mbuf_CV ddDeleteObj(ddGetObj("chip.rrc.util.cable" mbuf_cell "floorplan")) )

   mbuf_CV  =   nrOpenCellViewReadableByName( mbuf_cell "cable")
   lnewCV = betterCopyCellView( mbuf_CV "chip.rrc.util.cable" mbuf_cell "floorplan" nil nil t)
           dbSave(lnewCV)
          dbClose(lnewCV)

   newCV = nrOpenCellViewReadableByName( cable_cell "floorplan")
  when( newCV ddDeleteObj(ddGetObj("chip.rrc.util.cable" cable_cell "floorplan")) )
   newCV = nrOpenCellViewWritableByName( cable_cell "floorplan")

     nrSyncToNetlist(newCV)

;  when( fp 
;   cabCV = nrOpenCellViewWritableByName( cable_cell "floorplan")
;   when( newCV ddDeleteObj(ddGetObj("chip.rrc.util.cable" cable_cell "floorplan")) )
;
;   cab_CV = nrOpenCellViewWritableByName( cable_cell "cable")
;   cabCV = betterCopyCellView( cab_CV "chip.rrc.util.cable" cable_cell "floorplan" nil nil t)
;   )

   lnewCV  =   nrOpenCellViewWritableByName( mbuf_cell "floorplan" )
   cbl = car(setof( sh lnewCV~>shapes sh~>lpp==list("M8" "bus")))
   CreateCableBoundary(?CableShape cbl ?CV lnewCV ?CreatePRObject t)

   printf( "%s\n" newCV~>cellName)
   printf( "%s\n" newCV~>viewName)

   printf( "%s\n" lnewCV~>cellName)
   printf( "%s\n" lnewCV~>viewName)

   foreach( sh setof( ins newCV~>instances ins~>name=="sbuf" ) println("hello") sh~>orient="MXR90" sh~>xy=nth(0 cbl~>points))
   foreach( sh setof( ins newCV~>instances ins~>name=="rbuf" ) println("hello") sh~>orient="MXR90" sh~>xy=nth(length(cbl~>points)-1 cbl~>points))
   foreach( sh setof( ins newCV~>instances ins~>name=="mbuf" ) println("hello") sh~>orient="R0"    sh~>xy=list(0 0))


          dbSave(newCV)
    CDSP4CellView(newCV "add")
         dbClose(newCV)
         dbPurge(newCV)


           dbSave(lnewCV)
    CDSP4CellView(lnewCV "add")
          dbClose(lnewCV)
          dbPurge(lnewCV)


;     when( fp 
;      CDSP4CellView(cabCV "add")
;       dbSave(cabCV)
;      dbClose(cabCV)
;    ; dbPurge(cabCV)
;      )

   )
  ( "SYNC_NODE"
   sprintf( mbuf_cell  "chip.rrc.util.cable.%sCABLE-L%d_%d-R.%s"  buf channelHops channelWidth channelSpec) 

   newCV = nrOpenCellViewWritableByName( mbuf_cell "floorplan")
   when( newCV ddDeleteObj(ddGetObj("chip.rrc.util.cable" mbuf_cell "floorplan")) )

   mbuf_CV  =   nrOpenCellViewWritableByName( mbuf_cell "cable")
   newCV = betterCopyCellView( mbuf_CV "chip.rrc.util.cable" mbuf_cell "floorplan" nil nil t)

   cbl = car(setof( sh newCV~>shapes sh~>lpp==list("M8" "bus")))

   printf("%s\n" newCV~>cellName)
   printf("%s\n" newCV~>viewName)

   newCV  =   nrOpenCellViewWritableByName( mbuf_cell "floorplan" )
   CreateCableBoundary(?CableShape cbl ?CV newCV ?CreatePRObject t)

   foreach( sh setof( ins cabCV~>instances ins~>name=="sbuf"||ins~>name=="rbuf" ) sh~>"MXR90" )
   foreach( sh cabCV~>instances sh~>xy=list(0 0) )

    CDSP4CellView(newCV "add")

          dbSave(newCV)
         dbClose(newCV)
   )
  ( "XBUS"

          xadpL           =  cableGuide~>XBUS_Adapter_L   
          xadpR           =  cableGuide~>XBUS_Adapter_R   
          xchannelHopsL   =  cableGuide~>XBUS_L_ChannelHops
          xchannelHopsR   =  cableGuide~>XBUS_R_ChannelHops
          channelName     = lowerCase(channelName)
          spec_temp       = parseString( channelName "[]" )
          spec_temp2      = parseString( channelName "_[]" )
          
          when( nth(0 spec_temp2) == "rx" || nth(0 spec_temp2) == "tx"
             rxchannelSpec = strcat("rx_xbus_port_" cadr(spec_temp))
             txchannelSpec = strcat("tx_xbus_port_" cadr(spec_temp))
             channelSpec   = strcat("xbus_port_"    cadr(spec_temp))
           )
          when( nth(0 parseString( channelName "_[]" )) == "rx"  rxchannelSpec = strcat(car(spec_temp) "_" cadr(spec_temp)))  

          printf( "chip.rrc.util.cable.CABLE_SBUF_XbusPortHalf.%s\n"  xadpL )
          printf( "chip.rrc.util.cable.CABLE_RBUF_XbusPortHalf.%s\n"  xadpR )
          printf( "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s\n" xchannelHopsL txchannelSpec )
          printf( "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s\n" xchannelHopsR rxchannelSpec )
          printf( "chip.rrc.util.cable.CABLE_XbusPort-L%d_%d-R.%s\n"  xchannelHopsL xchannelHopsR channelSpec )

          sprintf(  adpL_cell "chip.rrc.util.cable.CABLE_SBUF_XbusPortHalf.%s"  xadpL )
          sprintf(  adpR_cell "chip.rrc.util.cable.CABLE_RBUF_XbusPortHalf.%s"  xadpR )
          sprintf( mbufL_cell "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s" xchannelHopsL txchannelSpec )
          sprintf( mbufR_cell "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s" xchannelHopsR rxchannelSpec )
          sprintf( cable_cell "chip.rrc.util.cable.CABLE_XbusPort-L%d_%d-R.%s"  xchannelHopsL xchannelHopsR channelSpec )

   lcab_CV  = nrOpenCellViewReadableByName( mbufL_cell  "floorplan")
   rcab_CV  = nrOpenCellViewReadableByName( mbufR_cell  "floorplan")
   when( lcab_CV CDSP4CellView(lcab_CV "edit") dbClose(lcab_CV ))
   when( rcab_CV CDSP4CellView(rcab_CV "edit") dbClose(rcab_CV ))

   cabl_CV = nrOpenCellViewWritableByName( mbufL_cell  "cable")
   cabr_CV = nrOpenCellViewWritableByName( mbufR_cell  "cable")

   betterCopyCellView( cabl_CV "chip.rrc.util.cable" mbufL_cell  "floorplan" nil nil t)
   betterCopyCellView( cabr_CV "chip.rrc.util.cable" mbufR_cell  "floorplan" nil nil t)

   lcab_CV  = nrOpenCellViewWritableByName( mbufL_cell  "floorplan")
   rcab_CV  = nrOpenCellViewWritableByName( mbufR_cell  "floorplan")
   cblL     = car(setof( sh lcab_CV~>shapes sh~>lpp==list("M8" "bus")))
   cblR     = car(setof( sh rcab_CV~>shapes sh~>lpp==list("M8" "bus")))

   CreateCableBoundary(?CableShape cblL ?CV lcab_CV ?CreatePRObject t)
   CreateCableBoundary(?CableShape cblR ?CV rcab_CV ?CreatePRObject t)

   newCV = nrOpenCellViewReadableByName(cable_cell  "floorplan")
   when( newCV CDSP4CellView(newCV "edit") dbClose(newCV))
   newCV = nrOpenCellViewWritableByName(cable_cell  "floorplan")

   nrSyncToNetlist(newCV)

   foreach( sh setof( ins newCV~>instances ins~>name=="sbufR" ) println("hello") sh~>orient="MXR90" sh~>xy=nth(0 cblR~>points))
   foreach( sh setof( ins newCV~>instances ins~>name=="rbufL" ) println("hello") sh~>orient="MXR90" sh~>xy=nth(length(cblL~>points)-1 cblL~>points))
   foreach( sh setof( ins newCV~>instances ins~>name=="cabL"  ) println("hello") sh~>orient="R0"    sh~>xy=list(0 0))
   foreach( sh setof( ins newCV~>instances ins~>name=="cabR"  ) println("hello") sh~>orient="R0"    sh~>xy=list(0 0))

   dbSave(newCV  )
   dbSave(lcab_CV)
   dbSave(rcab_CV)

    CDSP4CellView(newCV    "add")
    CDSP4CellView(lcab_CV  "add")
    CDSP4CellView(rcab_CV  "add")

   dbClose(newCV  )
   dbClose(lcab_CV)
   dbClose(rcab_CV)

;   dbPurge(newCV  )
;   dbPurge(lcab_CV)
;   dbPurge(rcab_CV)
   )

  )
t
 
  )
)

(defun createCableFloorplan18 ( cableGuide  @key (CV (geGetEditCellView)) (fp t) )
 (let ( 
        spec_root spec_temp lnewCV newCV mlCV sh cbl lnewRCV newRCV
        channelName  channelSBuf sbuf cabCV cab_CV
        channelWidth channelRBuf mbuf
         adpL_CV  adpL_cell channelHops  channelBuf  channelSpec channelWidth2
         adpR_CV  adpR_cell  sbuf_cell   sbuf_CV
        mbufL_CV mbufL_cell  rbuf_cell   rbuf_CV
        mbufR_CV mbufR_cell  mbuf_cell   mbuf_CV
        cable_CV cable_cell cable_cell  cable_CV
       )
 
       spec_root       =  "/nfs/site/disks/wdisk.83/pankala1/rrc_1/spec"
       channelName     =  cableGuide~>ChannelName
       channelWidth    =  cableGuide~>ChannelWidth
       channelHops     =  cableGuide~>ChannelHops
       channelBuf      =  cableGuide~>ChannelBuf
       client_spec     =  ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
       channelWidth2   =  round((channelWidth+1)/2)
 
       case( channelBuf
         (  "TS_18"
         channelSBuf     =  cableGuide~>sbuf_orient
         channelRBuf     =  cableGuide~>rbuf_orient
         )
       )

          spec_temp = parseString(lowerCase(channelName) "[]")
          if(length(spec_temp)>1
            then        
              channelSpec = strcat(car(spec_temp) "_" cadr(spec_temp))
            else
              channelSpec = lowerCase(channelName)
            )  
          if(channelBuf then buf = channelBuf else buf = "NYB")
          if(channelBuf=="DENSE" then buf = "" else buf = strcat(buf "_"))

  case(channelBuf
  (  "TS_18" || "TS"
   buf = "TS_"
   when( channelBuf == "TS_18"
   sprintf( mbuf_cell  "chip.rrc.util.cable.%sCABLE_MBUFS-L%d_%d-R.%s"  buf channelHops channelWidth2 channelSpec) 
   sprintf(cable_cell  "chip.rrc.util.cable.%sCABLE_18-L%d_%d-R.%s"        buf channelHops channelWidth  channelSpec) 
   )

   when( channelBuf == "TS"
   sprintf( mbuf_cell  "chip.rrc.util.cable.%sCABLE_MBUFS-L%d_%d-R.%s"  buf channelHops channelWidth2 channelSpec) 
   sprintf(cable_cell  "chip.rrc.util.cable.%sCABLE-L%d_%d-R.%s"        buf channelHops channelWidth  channelSpec) 
   )

   printf("chip.rrc.util.cable.%sCABLE_MBUFS-L%d_%d-R.%s\n"  buf channelHops channelWidth2 channelSpec) 
   printf("chip.rrc.util.cable.%sCABLE_18-L%d_%d-R.%s\n"     buf channelHops channelWidth  channelSpec) 

   mbuf_CV  =   nrOpenCellViewReadableByName( mbuf_cell "cable")
   lnewRCV  =   nrOpenCellViewReadableByName( mbuf_cell "floorplan")
   when( lnewRCV  CDSP4CellView(lnewRCV "edit") dbClose(lnewRCV))
   lnewCV  =   nrOpenCellViewWritableByName( mbuf_cell "floorplan")
   if( !lnewCV then CDSP4CellView(lnewCV "edit") else ddDeleteObj(ddGetObj("chip.rrc.util.cable" mbuf_cell "floorplan")) )
   lnewCV = betterCopyCellView( mbuf_CV "chip.rrc.util.cable" mbuf_cell "floorplan" nil nil t)

   newRCV = nrOpenCellViewReadableByName( cable_cell "floorplan")
   when( newRCV  CDSP4CellView(newRCV "edit") dbClose(newRCV))
   newCV = nrOpenCellViewWritableByName( cable_cell "floorplan")
   if( !newCV  then CDSP4CellView(newCV "edit") else ddDeleteObj(ddGetObj("chip.rrc.util.cable" cable_cell "floorplan")) )
   newCV = nrOpenCellViewWritableByName( cable_cell "floorplan")

          nrSyncToNetlist(newCV)

;         nrSyncToNetlist(lnewCV)
;  mlCV = nrOpenCellViewReadableByName(mbuf_cell  "floorplan")

   println(newCV~>cellName)
   println(newCV~>viewName)

   cbl = car(setof( sh lnewCV~>shapes sh~>lpp==list("M8" "bus")))
   foreach( sh setof( ins lnewCV~>instances ins~>libName=="standard.reset" ) dbDeleteObject(sh) )

   foreach( sh setof( ins newCV~>instances ins~>name=="sbuf" ) println("hello") sh~>orient="MXR90" sh~>xy=nth(0 cbl~>points))
   foreach( sh setof( ins newCV~>instances ins~>name=="rbuf" ) println("hello") sh~>orient="MXR90" sh~>xy=nth(length(cbl~>points)-1 cbl~>points))
   foreach( sh setof( ins newCV~>instances ins~>name=="mbuf" ) println("hello") sh~>orient="R0"    sh~>xy=list(0 0))

          dbSave(newCV)
          dbSave(lnewCV)
    CDSP4CellView(newCV "add")
    CDSP4CellView(lnewCV "add")

         dbClose(newCV)
         dbClose(lnewCV)
         dbPurge(newCV)
         dbPurge(lnewCV)
         dbClose(mbuf_CV)
      )

   )

t
 
  )
)


(defun GetCableNames ( cableGuide )
 (let (  
        spec_root spec_temp                                                      xadpL           adpL_cell 
        channelName  channelSBuf sbuf                                            xadpR           adpR_cell 
        channelWidth channelRBuf mbuf                                            xchannelHopsL  mbufL_cell 
         adpL_CV  adpL_cell channelHops  channelBuf  channelSpec channelWidth2   xchannelHopsR  mbufR_cell 
         adpR_CV  adpR_cell  sbuf_cell   sbuf_CV   cell0                         channelName    cable_cell 
        mbufL_CV mbufL_cell  rbuf_cell   rbuf_CV   cell1                         spec_temp      
        mbufR_CV mbufR_cell  mbuf_cell   mbuf_CV   cell2                         spec_temp2     
        cable_CV cable_cell cable_cell  cable_CV   cell3 
       )


       channelName     =  cableGuide~>ChannelName
       channelWidth    =  cableGuide~>ChannelWidth
       channelHops     =  cableGuide~>ChannelHops
       channelBuf      =  cableGuide~>ChannelBuf
       client_spec     =  ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
       channelWidth2   =  round((channelWidth+1)/2)

 ;     when( channelBuf == "TS_18" channelBuf = "TS" )
 
       case( channelBuf
         (  "DENSE" || "NYB" || "TS" || "TS_18"
          channelSBuf     =  cableGuide~>ChannelSBufOrient 
          channelRBuf     =  cableGuide~>ChannelRBufOrient 
         )
         ( "SYNC_NODE" 
         channelSBuf     =  ""
         channelRBuf     =  ""
         )
       )

       case( channelBuf
         (  "DENSE" || "NYB" || "TS" || "SYNC_NODE"
          if(channelBuf then buf = channelBuf else buf = "NYB")
          if(channelBuf=="DENSE" then buf = "" else buf = strcat(buf "_"))
   
          if(!channelSBuf then sbuf="H" else sbuf=channelSBuf)
          if(!channelRBuf then rbuf="H" else rbuf=channelRBuf)
          spec_temp = parseString(lowerCase(channelName) "[]")
          if(length(spec_temp)>1
            then        
              channelSpec = strcat(car(spec_temp) "_" cadr(spec_temp))
            else
              channelSpec = lowerCase(channelName)
            )

         sprintf( cell0 "chip.rrc.util.cable.%sCABLE_SBUF-L%d-R.%s" buf channelWidth sbuf) 
         sprintf( cell1 "chip.rrc.util.cable.%sCABLE_RBUF-L%d-R.%s" buf channelWidth rbuf) 
         sprintf( cell2 "chip.rrc.util.cable.%sCABLE_MBUFS-L%d_%d-R.%s"  buf channelHops channelWidth2 channelSpec) 
         sprintf( cell3 "chip.rrc.util.cable.wires.%sCABLE_MBUFS-L%d_%d-R_%s_POWER_GRID_TIEOFF"  buf channelHops channelWidth2 channelSpec) 
         sprintf( cell4 "chip.rrc.util.cable.%sCABLE-L%d_%d-R.%s"        buf channelHops channelWidth  channelSpec) 
         sprintf( cell5 "chip.rrc.util.cable.wires.%sCABLE-L%d_%d-R_%s_POWER_GRID_TIEOFF"        buf channelHops channelWidth  channelSpec) 
         list( cell0 
               cell1 
               cell2 
               cell3 
               cell4 
               cell5 )
        )
         (  "TS_18" 
   
          if(!channelSBuf then sbuf="H" else sbuf=channelSBuf)
          if(!channelRBuf then rbuf="H" else rbuf=channelRBuf)
          spec_temp = parseString(lowerCase(channelName) "[]")
          if(length(spec_temp)>1
            then        
              channelSpec = strcat(car(spec_temp) "_" cadr(spec_temp))
            else
              channelSpec = lowerCase(channelName)
            )

         sprintf( cell0 "chip.rrc.util.cable.TS_CABLE_SBUF_18-L%d-R.%s"  channelWidth sbuf) 
         sprintf( cell1 "chip.rrc.util.cable.TS_CABLE_RBUF-L%d-R.%s"  channelWidth rbuf) 
         sprintf( cell2 "chip.rrc.util.cable.TS_CABLE_MBUFS-L%d_%d-R.%s"   channelHops channelWidth2 channelSpec) 
         sprintf( cell3 "chip.rrc.util.cable.TS_CABLE_18-L%d_%d-R.%s"         channelHops channelWidth  channelSpec) 
         list( cell0 
               cell1 
               cell2 
               cell3 )
        )
       ( "XBUS"
          xadpL           =  cableGuide~>XBUS_Adapter_L   
          xadpR           =  cableGuide~>XBUS_Adapter_R   
          xchannelHopsL   =  cableGuide~>XBUS_L_ChannelHops
          xchannelHopsR   =  cableGuide~>XBUS_R_ChannelHops
          channelName     = lowerCase(channelName)
          spec_temp       = parseString( channelName "[]" )
          spec_temp2      = parseString( channelName "_[]" )
          
          when( nth(0 spec_temp2) == "rx" || nth(0 spec_temp2) == "tx"
             rxchannelSpec = strcat("rx_xbus_port_" cadr(spec_temp))
             txchannelSpec = strcat("tx_xbus_port_" cadr(spec_temp))
             channelSpec   = strcat("xbus_port_"    cadr(spec_temp))
           )
          when( nth(0 parseString( channelName "_[]" )) == "rx"  rxchannelSpec = strcat(car(spec_temp) "_" cadr(spec_temp)))  

          printf( "chip.rrc.util.cable.CABLE_SBUF_XbusPortHalf.%s\n"  xadpL )
          printf( "chip.rrc.util.cable.CABLE_RBUF_XbusPortHalf.%s\n"  xadpR )
          printf( "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s\n" xchannelHopsL txchannelSpec )
          printf( "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s\n" xchannelHopsR rxchannelSpec )
          printf( "chip.rrc.util.cable.CABLE_XbusPort-L%d_%d-R.%s\n"  xchannelHopsL xchannelHopsR channelSpec )

          sprintf(  adpL_cell "chip.rrc.util.cable.CABLE_SBUF_XbusPortHalf.%s"  xadpL )
          sprintf(  adpR_cell "chip.rrc.util.cable.CABLE_RBUF_XbusPortHalf.%s"  xadpR )
          sprintf( mbufL_cell "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s" xchannelHopsL txchannelSpec )
          sprintf( mbufR_cell "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s" xchannelHopsR rxchannelSpec )
          sprintf( cable_cell "chip.rrc.util.cable.CABLE_XbusPort-L%d_%d-R.%s"  xchannelHopsL xchannelHopsR channelSpec )
         list(  adpL_cell 
                adpR_cell 
               mbufL_cell 
               mbufR_cell 
               cable_cell 
            )
        )

       )



 )
)

(defun ppcnames ( cableGuide )
 (let (  
        spec_root spec_temp                                                      xadpL           adpL_cell 
        channelName  channelSBuf sbuf                                            xadpR           adpR_cell 
        channelWidth channelRBuf mbuf                                            xchannelHopsL  mbufL_cell 
         adpL_CV  adpL_cell channelHops  channelBuf  channelSpec channelWidth2   xchannelHopsR  mbufR_cell 
         adpR_CV  adpR_cell  sbuf_cell   sbuf_CV   cell0       cell4             channelName    cable_cell 
        mbufL_CV mbufL_cell  rbuf_cell   rbuf_CV   cell1                         spec_temp      
        mbufR_CV mbufR_cell  mbuf_cell   mbuf_CV   cell2                         spec_temp2     
        cable_CV cable_cell cable_cell  cable_CV   cell3 
       )


       channelName     =  cableGuide~>ChannelName
       channelWidth    =  cableGuide~>ChannelWidth
       channelHops     =  cableGuide~>ChannelHops
       channelBuf      =  cableGuide~>ChannelBuf
       client_spec     =  ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
       channelWidth2   =  round((channelWidth+1)/2)
 
       case( channelBuf
         (  "DENSE" || "NYB" || "TS"
          channelSBuf     =  cableGuide~>ChannelSBufOrient 
          channelRBuf     =  cableGuide~>ChannelRBufOrient 
         )
         ( "SYNC_NODE" 
         channelSBuf     =  ""
         channelRBuf     =  ""
         )
       )

       case( channelBuf
         (  "DENSE" || "NYB" || "TS" || "SYNC_NODE"
          if(channelBuf then buf = channelBuf else buf = "NYB")
          if(channelBuf=="DENSE" then buf = "" else buf = strcat(buf "_"))
   
          if(!channelSBuf then sbuf="H" else sbuf=channelSBuf)
          if(!channelRBuf then rbuf="H" else rbuf=channelRBuf)
          spec_temp = parseString(lowerCase(channelName) "[]")
          if(length(spec_temp)>1
            then        
              channelSpec = strcat(car(spec_temp) "_" cadr(spec_temp))
            else
              channelSpec = lowerCase(channelName)
            )

          printf( "    chip.rrc.util.cable.%sCABLE(%d,%d) :>\n      chip.rrc.util.cable.%sCABLE(%d,%d).%s ;\n" 
                       buf channelHops channelWidth                   buf channelHops channelWidth   channelSpec) 

         sprintf( cell0 "chip.rrc.util.cable.%sCABLE_SBUF-L%d-R.%s" buf channelWidth sbuf) 
         sprintf( cell1 "chip.rrc.util.cable.%sCABLE_RBUF-L%d-R.%s" buf channelWidth rbuf) 
         sprintf( cell2 "chip.rrc.util.cable.%sCABLE_MBUFS-L%d_%d-R.%s"  buf channelHops channelWidth2 channelSpec) 
         sprintf( cell3 "chip.rrc.util.cable.%sCABLE(%d,%d).%s"        buf channelHops channelWidth  channelSpec) 
         sprintf( cell4 "    chip.rrc.util.cable.%sCABLE-(%d,%d) :>\n      chip.rrc.util.cable.%sCABLE-L%d_%d-R.%s ;\n" 
                             buf channelHops channelWidth                   buf channelHops channelWidth   channelSpec) 

         list( cell0 
               cell1 
               cell2 
               cell3 
               cell4 )
        )
       ( "XBUS"
          xadpL           =  cableGuide~>XBUS_Adapter_L   
          xadpR           =  cableGuide~>XBUS_Adapter_R   
          xchannelHopsL   =  cableGuide~>XBUS_L_ChannelHops
          xchannelHopsR   =  cableGuide~>XBUS_R_ChannelHops
          channelName     = lowerCase(channelName)
          spec_temp       = parseString( channelName "[]" )
          spec_temp2      = parseString( channelName "_[]" )
          
          when( nth(0 spec_temp2) == "rx" || nth(0 spec_temp2) == "tx"
             rxchannelSpec = strcat("rx_xbus_port_" cadr(spec_temp))
             txchannelSpec = strcat("tx_xbus_port_" cadr(spec_temp))
             channelSpec   = strcat("xbus_port_"    cadr(spec_temp))
           )
          when( nth(0 parseString( channelName "_[]" )) == "rx"  rxchannelSpec = strcat(car(spec_temp) "_" cadr(spec_temp)))  

          printf( "    chip.rrc.util.cable.CABLE_XbusPort-(%d,%d) :>\n      chip.rrc.util.cable.CABLE_XbusPort(%d,%d).%s ;\n"  
                       xchannelHopsL xchannelHopsR                           xchannelHopsL xchannelHopsR channelSpec )

          sprintf(  adpL_cell "chip.rrc.util.cable.CABLE_SBUF_XbusPortHalf.%s"  xadpL )
          sprintf(  adpR_cell "chip.rrc.util.cable.CABLE_RBUF_XbusPortHalf.%s"  xadpR )
          sprintf( mbufL_cell "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s" xchannelHopsL txchannelSpec )
          sprintf( mbufR_cell "chip.rrc.util.cable.CABLE_MBUFS_XbusPortHalf-L%d-R.%s" xchannelHopsR rxchannelSpec )
          sprintf( cable_cell "chip.rrc.util.cable.CABLE_XbusPort-L%d_%d-R.%s"  xchannelHopsL xchannelHopsR channelSpec )

          sprintf( topSpec "    chip.rrc.util.cable.CABLE_XbusPort(%d,%d) :>\n      chip.rrc.util.cable.CABLE_XbusPort(%d,%d).%s ;"  
                                xchannelHopsL xchannelHopsR                           xchannelHopsL xchannelHopsR channelSpec )

         list(  adpL_cell 
                adpR_cell 
               mbufL_cell 
               mbufR_cell 
               cable_cell topSpec
            )
        )

       )



 )
)

(defun P4Cable (  @key (CMD "edit") (CV (geGetEditCellView)) (CableShape car(geGetSelectedSet())) )
  (let ( names  mbuf_cell   			mbuf_CV  
                mbufpg_cell 		mbufpg_CV  
               cable_cell   			cabCV    

        )
     
     names = GetCableNames( CableShape )


       case( CableShape~>ChannelBuf
         (  "DENSE" || "NYB" || "TS"

						 mbuf_cell   = nth( 2 names )
						 mbufpg_cell = nth( 3 names ) 
						cable_cell   = nth( 4 names )
 

								mbuf_CV  =   nrOpenCellViewReadableByName(  mbuf_cell  "layout")
							mbufpg_CV  =   nrOpenCellViewReadableByName(  mbufpg_cell  "layout")
								cabCV    =   nrOpenCellViewReadableByName( cable_cell  "layout")

           printf( "%s cell %s\n" CMD  mbuf_cell   )
           printf( "%s cell %s\n" CMD  mbufpg_cell )
           printf( "%s cell %s\n" CMD cable_cell   )

					 when( mbuf_CV CDSP4CellView(mbuf_CV CMD)       )
					 when( mbufpg_CV CDSP4CellView(mbufpg_CV CMD)   )
					 when( cabCV   CDSP4CellView(cabCV   CMD)       )
           when( CMD == "edit" 
            dbPurge(mbuf_CV)  
            dbPurge(mbufpg_CV)
             dbPurge(cabCV  ) 
            )
         )
         (  "SYNC_NODE"

						cable_cell   = nth( 4 names )
						cable_pg_cell   = nth( 5 names )
						cabCV        = nrOpenCellViewReadableByName( cable_cell  "layout")
						cabpgCV        = nrOpenCellViewReadableByName( cable_pg_cell  "layout")

           printf( "%s cell %s\n" CMD cable_cell   )
           printf( "%s cell %s\n" CMD cable_pg_cell   )

					 when( cabCV     CDSP4CellView( cabCV    CMD)  )
					 when( cabpgCV   CDSP4CellView( cabpgCV  CMD)  )

           when( CMD == "edit" 
             dbPurge(cabCV  )
             dbPurge(cabpgCV)
            )

         )


       )

t  

 )
)

(defun updateHop ( N @key (CV (geGetEditCellView)) (CableShape car(geGetSelectedSet())))
  (let ( hop buf i bufB mbuf
                   bufF 
                   bufR 
)

   foreach( i CV~>instances 
      parts = parseString( i~>name "[]" )
      case( nth(0 parts) 
       ( "mbuf"
         hop = evalstring(nth( 1 parts ))
         buf = evalstring(nth( 2 parts ))
         i~>name=sprintf( nil "tempmbuf_%d_%d_" hop buf )
        )
       ( "bufReset"
         hop = nth( 1 parts )
         i~>name=sprintf( nil "tempbuf_%s_" hop )
        )
       ( "fwdBufReset"
         hop = nth( 1 parts )
         i~>name=sprintf( nil "tempfbuf_%s_" hop )
        )
       ( "rampReset"
         hop = nth( 1 parts )
         i~>name=sprintf( nil "temprbuf_%s_" hop )
        )
      )
    )

   for( I 0 CableShape~>ChannelHops-1
    bufB = dbFindAnyInstByName( CV  sprintf( nil "tempbuf_%d_" I ) )
    bufF = dbFindAnyInstByName( CV  sprintf( nil "tempfbuf_%d_" I ) )
    bufR = dbFindAnyInstByName( CV  sprintf( nil "temprbuf_%d_" I ) )

    bufB~>name = sprintf( nil "bufReset[%d]" I+N )
    bufF~>name = sprintf( nil "fwdBufReset[%d]" I+N )
    bufR~>name = sprintf( nil "rampReset[%d]" I+N )

    for( J 0 19
      mbuf = dbFindAnyInstByName( CV  sprintf( nil "tempmbuf_%d_%d_" I J ) )
      mbuf~>name = sprintf( nil "mbuf[%d][%d]" I+N J )
      )
    )

t

 )
)

; (defun cableMisc ( CableShape @key (CV (geGetEditCellView)) )
;   (let ( )
; 
;   createCableSpec( CableShape ?myspec "/nfs/site/disks/wdisk.68/vjain9/rrc_1/spec" )
; 
;   names = GetCableNames( CableShape )
;   mbuf
; 
;   newCV  = betterCopyCellView(delCV libLoc cellName "floorplan" nil nil t)
; 
;  )
; )


; (defun hackDft ( @key (CV (geGetEditCellView)) (CableShape car(geGetSelectedSet())))
;   (let ( hop buf i bufB mbuf
;                    bufF 
;                    bufR 
; )
; 
;    foreach( i CV~>instances 
;     parts = parseString( i~>name "[]" )
;     when( nth(0 parts) == "mbuf"
;       hop = evalstring(nth( 1 parts ))
;       buf = evalstring(nth( 2 parts ))
; 
;       case( buf
;        ( 0 || 1 || 2 
;          i~>name=sprintf( nil "mbuf_sin[%d][%d]" hop buf )
;         )
;        ( 3 || 4 || 5
;          i~>name=sprintf( nil "mbuf_sout[%d][%d]" hop buf-3 )
;         )
;        ( 6
;          i~>name=sprintf( nil "mbuf_ingerr[%d]" hop )
;         )
;        ( 7
;          i~>name=sprintf( nil "mbuf_egerr[%d]" hop )
;         )
; 
;       )
;      )
;     )
; 
; t
; 
;  )
; )
; 
; foreach(c wcv()~>instances
; k=parseString(c~>name "[]")
; when(nth(0 k) == "mbuf_sin"
;  c~>name=sprintf( nil "temp[%s][%s]" nth(1 k) nth(2 k) )
;  )
; 
; )
; 
; foreach(c wcv()~>instances
; k=parseString(c~>name "[]")
; 
; when(nth(0 k) == "temp"
;  hop=evalstring(nth(1 k))
;  c~>name=sprintf( nil "mbuf_sin[%s][%s]" 27-hop nth(2 k) )
;  )
; 
; )
; 
; 

(defun reportStruts ( list )
 (let ( outFl  inFl mem mbufCV struts k strutsm cabCV )

	  inFl  = infile( list )
	  outFl = outfile( "cable_nostruts.txt" )
		
		while( gets(mem inFl)
;		 printf("%s" car(parseString(mem "\n")))
		 cabCV = nrOpenCellViewReadableByName(car(parseString(mem "\n")) "layout")
     when( !cabCV printf( "Layout view doesnot exist for %s \n" cabCV~>cellName ) )
     struts = setof( k cabCV~>instances 
                     k~>cellName == "globals.wires.HORIZONTAL_STRUTS_M357" || 
                     k~>cellName == "globals.wires.HORIZONTAL_STRUTS_M35" || 
                     k~>cellName == "globals.wires.HORIZONTAL_STRUTS_M24" || 
                     k~>cellName == "globals.wires.VERTICAL_STRUTS_M246" )

     mbufCV = dbFindAnyInstByName( cabCV "mbuf" )
     when( mbufCV 
        strutsm = setof( k mbufCV~>master~>instances 
                        k~>cellName == "globals.wires.HORIZONTAL_STRUTS_M357" || 
                        k~>cellName == "globals.wires.HORIZONTAL_STRUTS_M35" || 
                        k~>cellName == "globals.wires.HORIZONTAL_STRUTS_M24" || 
                        k~>cellName == "globals.wires.VERTICAL_STRUTS_M246" )
      )

     when( !struts && !strutsm
       fprintf( outFl "%s\n" cabCV~>cellName ) 
       printf( "Struts don't exist for %s \n" cabCV~>cellName ) 
      )


   ; dbClose( mbufCV )
     dbClose( cabCV )
		)

close(outFl)

t
 )
)

(defun UpdateCableStruts ( CableShape  @key (CV (geGetEditCellView)) ) 
 (let ( names mbuf_cell cable_cell  obs_CV month tempView 
              mbuf_CV   cabCV       stamp  date  
         )

       names = GetCableNames( CableShape )
       case( CableShape~>ChannelBuf

         (  "DENSE" || "NYB" || "TS" || "TS_18"

					mbuf_cell   = nth( 2 names )
					mbuf_CV   =   nrOpenCellViewReadableByName(  mbuf_cell  "layout")
          printf( "P4Edit cell %s\n" mbuf_cell   )
          if( mbuf_CV 
            then
             stamp = dbGetFileTimeStamp(mbuf_CV)
             month = nth(0 parseString(stamp " "))
             date  = nth(1 parseString(stamp " "))
             CDSP4CellView(mbuf_CV "edit") 
             dbPurge(mbuf_CV)
             tempView = sprintf(nil "cable_%s_%s" month date )
             UpdateCableObstructions( ?Selected list(CableShape) ?view tempView )

					   obs_CV = nrOpenCellViewReadableByName( mbuf_cell tempView )
					   mbuf_CV = nrOpenCellViewWritableByName( mbuf_cell "layout" )
             DeleteAutoGndShields( ?CV mbuf_CV )

             foreach( shp mbuf_CV~>shapes when( shp~>autoBlockage dbDeleteObject(shp) ) )
             foreach( shp obs_CV~>shapes 
               when( shp~>autoBlockage 
                shp~>TimeStamp=stamp
                dbCopyShape( shp mbuf_CV )
              )
             )
             DrawGndShield( mbuf_CV )
             dbSave(mbuf_CV)
             dbPurge(mbuf_CV)
             dbClose(obs_CV)
             printf( "Updated GND Struts for %s\n" mbuf_cell   )
            else
             printf( "layout view not found for %s\n" mbuf_cell   )
           )
          
         )
         (  "SYNC_NODE"

					cable_cell   = nth( 4 names )
					cabCV        = nrOpenCellViewReadableByName( cable_cell  "layout")
          printf( "P4Edit cell %s\n" cable_cell )
          if(  cabCV
            then
             stamp = dbGetFileTimeStamp(cabCV)
             month = nth(0 parseString(stamp " "))
             date  = nth(1 parseString(stamp " "))
             CDSP4CellView(cabCV "edit") 
             dbPurge(cabCV)
             tempView = sprintf(nil "cable_%s_%s" month date )
             UpdateCableObstructions( ?Selected list(CableShape) ?view tempView )

					   obs_CV = nrOpenCellViewReadableByName( cable_cell tempView )
					   cabCV = nrOpenCellViewWritableByName( cable_cell "layout" )
             DeleteAutoGndShields( ?CV cabCV )

             foreach( shp cabCV~>shapes when( shp~>autoBlockage dbDeleteObject(shp) ) )
             foreach( shp obs_CV~>shapes 
               when( shp~>autoBlockage 
                shp~>TimeStamp=stamp
                dbCopyShape( shp cabCV )
              )
             )
             DrawGndShield( cabCV )
              dbSave(cabCV)
             dbPurge(cabCV)
             dbClose(obs_CV)
             printf( "Updated GND Struts for %s\n" cable_cell )
            else
             printf( "layout view not found for %s\n" cable_cell )
           )


         )
       )

t
 )
)

(defun DeleteAutoGndShields ( @key (CV (geGetEditCellView)) (all nil) )
 (let ( struts k )

   struts = setof( k CV~>instances 
                 k~>cellName == "globals.wires.HORIZONTAL_STRUTS_M357" || 
                 k~>cellName == "globals.wires.VERTICAL_STRUTS_M246" )
   
  foreach( k struts dbDeleteObject( k ) )

t 
 )
)
