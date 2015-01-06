(defun createHopsFile ( @key (CV (geGetEditCellView)) (Selected (geGetSelectedSet)) )
 (let ( outfl s hops hopDist cableFl shapes )

	hopDist = 290
	outfl = outfile("temp_hops.txt") 
  writtenList = nil
  if(Selected then shapes = Selected else shapes = CV~>shapes )

	foreach(s shapes ; CV~>shapes 
		when(s~>ChannelName
      parts = parseString(s~>ChannelName "[]")
      if(length(parts)>1
      then
      when(!member(car(parts) writtenList)
       writeHopsArray(outfl nth(0 parts))
       writtenList=cons(car(parts) writtenList)
       )
      else
      hops= s~>ChannelHops ; round(CAComputeCableLength(s)/hopDist)
			fprintf(outfl "  int HOPS_%s = %d;\n" s~>ChannelName hops)
       )
      )
		)

  close(outfl)
  cableFl = strcat(CV~>cellName "_cableHops.txt")
; sh(strcat("rm " cableFl))
  sh(strcat("cp temp_hops.txt " cableFl))

 )
)

(defun writeHopsArray (fl ch  @key (CV (geGetEditCellView)) )
 let( (I J hopArray pt p currName currHop hops 	hopDist )
	hopDist = 290

   I = 0
   pt = setof(s CV~>shapes s~>ChannelName)
   sprintf(hopArray "  int HOPS_%s = {" ch)
   foreach(p pt
    when(car(parseString(p~>ChannelName "[]")) == ch 
      I = I+1
     )
   )
   printf("%s[0..%d]\n" ch I-1)

   for(J 0 I-1
   sprintf(currName "%s[%d]" ch J)
   when( J < I-1
   foreach(p pt
    when(p~>ChannelName == currName 
      case( ch
      ( "RX_XBUS_PORT"
      hops= p~>XBUS_R_ChannelHops 
      sprintf(currHop "%d, " hops)
       fprintf( fl "%s \t %d \n" p~>ChannelName p~>XBUS_R_ChannelHops )
        )
      ( "TX_XBUS_PORT"
      hops= p~>XBUS_L_ChannelHops 
      sprintf(currHop "%d, " hops)
       fprintf( fl "%s \t %d \n" p~>ChannelName p~>XBUS_L_ChannelHops )
        )
      ( t
      hops= p~>ChannelHops 
      sprintf(currHop "%d, " hops)
       fprintf( fl "%s \t %d \n" p~>ChannelName p~>ChannelHops )
        )
      )
      hopArray = strcat(hopArray currHop)
     )
    )
    )
   when( J == I-1
   foreach(p pt
    when(p~>ChannelName == currName 
      case( ch
      ( "RX_XBUS_PORT"
      hops= p~>XBUS_R_ChannelHops 
      sprintf(currHop "%d, " hops)
       fprintf( fl "%s \t %d \n" p~>ChannelName p~>XBUS_R_ChannelHops )
        )
      ( "TX_XBUS_PORT"
      hops= p~>XBUS_L_ChannelHops 
      sprintf(currHop "%d, " hops)
       fprintf( fl "%s \t %d \n" p~>ChannelName p~>XBUS_L_ChannelHops )
        )
      ( t
      hops= p~>ChannelHops 
      sprintf(currHop "%d, " hops)
       fprintf( fl "%s \t %d \n" p~>ChannelName p~>ChannelHops )
        )
      )
      hopArray = strcat(hopArray currHop)
     )
    )
    )
   )
   hopArray = strcat(hopArray "};")

  fprintf(fl "%s\n" hopArray)

 )
)

(defun updateCableHops ( cableGuide @key (CV (geGetEditCellView)))
 (let ( hops hopDist )

 			hopDist =  290 ; NYB Hops Distance
      when( cableGuide~>ChannelBuf == "TS"    hopDist =  230 )
      when( cableGuide~>ChannelBuf == "DENSE" hopDist =  290 )
 			hops    =   s~>ChannelHops ; round(CAComputeCableLength(s)/hopDist)
 			cableGuide~>ChannelHops = hops

 )
)

(defun recurseHops ( @key (CV (geGetEditCellView)) )
 (let ( guides s )

      guides = setof(s CV~>shapes s~>ChannelName&&s~>ChannelWidth)
      println(length(guides))
      foreach(s guides updateCableHops(s))

 )
)
;foreach(s CV~>shapes when(s~>ChannelName
;parts=parseString(s~>ChannelName "[]")
;when(car(parts)=="HEADER_SUBSEG" sprintf(temp "HEADER_SUB[%s]" cadr(parts)) s~>ChannelName=temp)))
