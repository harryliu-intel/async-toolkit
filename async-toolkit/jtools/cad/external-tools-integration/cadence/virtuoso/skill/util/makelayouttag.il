; skill to do layout_tag cell
; AAG
; $Id$
; $DateTime$

(defun MakeALabel ( cv lpp point label )
    let( ( x y )
      x=car(point)
      y=cadr(point)
      dbCreateLabel(cv lpp point label "centerCenter" "R0" "stick" 0.08)
      dbCreateRect(cv lpp list(list(x-0.005 y-0.005) list(x+0.005 y+0.005)))
    )
)

(defun MakeLeafTag ( CellView @key (orientation "MXR90") (TargetViewName "layout_tag") )
    let( (point trans inst rcv box x y x1 y1 fill)
        trans=list(0:0 orientation 1.0)
        rcv=nrOpenCellViewWritable(
            CellView~>libName
            CellView~>cellName
            TargetViewName ?mode "w" )
        if( rcv != nil then
            foreach( label setof( l CellView~>shapes l~>objType=="label" && l~>theLabel != "GND" && l~>theLabel != "Vdd")
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
            inst = dbCreateInst( rcv CellView "layout1" 0:0 orientation )
            if( inst == nil
                printf( "Cannot create inst\n" ))


        ;I have completely removed the original code drawing stripes of implants and wells around the perimeter.
        ;Instead, it will tile fill cells around the edges, more representative of the real usage
            
            ; the below ASSUMES that the rotation is MXR90, it breaks if it is not MXR90
            ; also note that x and y are interchanged because of the MXR90 rotation
            maxx=caadr(GetPrbound(CellView)->bBox)
            maxy=cadadr(GetPrbound(CellView)->bBox)
            x=caar(GetPrbound(CellView)->bBox)-4*DefaultWiringPitch
            y=cadar(GetPrbound(CellView)->bBox)
	    dbCreateRect(rcv BoundaryLPP list(y:caar(GetPrbound(CellView)->bBox) maxy:maxx)) ; adds a prBoundary shape to the tag view
            while(x <= maxx+4*DefaultWiringPitch
              dbCreateInstByMasterName( rcv "globals" "globals.wires.POLY_FILL_S52" "layout" nil list(y x) "R90" )
              dbCreateInstByMasterName( rcv "globals" "globals.wires.POLY_FILL_S52" "layout" nil list(maxy x) "MXR90" )
              x=x+4*DefaultWiringPitch
            )
            x=caar(GetPrbound(CellView)->bBox)
            y=cadar(GetPrbound(CellView)->bBox)
            while(y < maxy
              dbCreateInstByMasterName( rcv "globals" "globals.wires.POLY_FILL_S52" "layout" nil list(y x) "R270" )
              dbCreateInstByMasterName( rcv "globals" "globals.wires.POLY_FILL_S52" "layout" nil list(y maxx) "MXR90" )
              y=y+2*TrackPitch
            )
            maxx=caadr(GetPrbound(CellView)->bBox)
            maxy=cadadr(GetPrbound(CellView)->bBox)
            x1=caar(GetPrbound(CellView)->bBox)
            x=x1
            n=0
                dbCreateLabel( rcv list("M3" "gnd") cadar(GetPrbound(CellView)->bBox)+0.06:caar(GetPrbound(CellView)->bBox)+0.06 "GND" "centerCenter" "R0" "roman" 0.005 )
                dbCreateLabel( rcv list("M3" "gnd") cadadr(GetPrbound(CellView)->bBox)-0.06:caar(GetPrbound(CellView)->bBox)+0.06 "GND" "centerCenter" "R0" "roman" 0.005 )
            while(x <= maxx+4*DefaultWiringPitch
              y1=cadar(GetPrbound(CellView)->bBox)
              y=y1
              while(y <= maxy+2*TrackPitch
                  dbCreateInstByMasterName( rcv "globals" "globals.wires.POWER_GRID_M34" "layout" nil list(y x) "R90" )
                  MakeALabel( rcv list("M4" "gnd") y:x sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M4" "gnd") y:x+TrackPitch sprintf(nil "GND" n++) )
                  if(y==y1 MakeALabel( rcv list("M4" "gnd") y-2*TrackPitch:x sprintf(nil "GND" n++) ))
                  if(y==y1 MakeALabel( rcv list("M4" "gnd") y-2*TrackPitch:x+TrackPitch sprintf(nil "GND" n++) ))
                  MakeALabel( rcv list("M4" "vdd") y-TrackPitch:x "Vdd" )
                  MakeALabel( rcv list("M4" "vdd") y-TrackPitch:x+TrackPitch "Vdd")
                  if(x+2*TrackPitch > maxx+4*DefaultWiringPitch then
                      MakeALabel( rcv list("M4" "vdd") y-TrackPitch:x+2*TrackPitch "Vdd" )
                      MakeALabel( rcv list("M4" "gnd") y:x+2*TrackPitch sprintf(nil "GND" n++) )
                      if(y==y1 MakeALabel( rcv list("M4" "gnd") y-2*TrackPitch:x+2*TrackPitch sprintf(nil "GND" n++) ))
                  )
                  y=y+2*TrackPitch
              )
              x=x+2*TrackPitch
            )

            dbSave( rcv )
            dbClose( rcv )
        else
            printf( "Cannot open cell\n" )
        )
        t
    )
)

(defun MakeProteusTag ( CellView @key (orientation "MXR90") (TargetViewName "layout_tag") )
    let( (point trans inst rcv box x y x1 y1 y2 y3 maxy1 fill)
        trans=list(0:0 orientation 1.0)
        rcv=nrOpenCellViewWritable(
            CellView~>libName
            CellView~>cellName
            TargetViewName ?mode "w" )
        if( rcv != nil then
            foreach( label setof( l CellView~>shapes l~>objType=="label" && l~>theLabel != "GND" && l~>theLabel != "Vdd")
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
            inst = dbCreateInst( rcv CellView "layout1" 0:0 orientation )
            if( inst == nil
                printf( "Cannot create inst\n" ))

            maxx=caadr(GetPrbound(CellView)->bBox)
            maxy=cadadr(GetPrbound(CellView)->bBox)
            x=caar(GetPrbound(CellView)->bBox)
            x1=x+24*DefaultWiringPitch
            x3=x+36*DefaultWiringPitch
            x4=x+48*DefaultWiringPitch
            x5=x+72*DefaultWiringPitch
            y=cadar(GetPrbound(CellView)->bBox)-4*DefaultWiringPitch
	    y1=y-6*DefaultWiringPitch
	    y2=y-2*DefaultWiringPitch
            maxy1=maxy+6*DefaultWiringPitch
		n=0
	    dbCreateRect(rcv BoundaryLPP list(y:caar(GetPrbound(CellView)->bBox) maxy:maxx)) ; adds a prBoundary shape to the tag view
	when(maxx==1.56
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "R270" )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.17 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):2.72 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.945 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):-0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):-1.17 sprintf(nil "Vdd" n++) )
            )
	when(maxx==3.12
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x1) "MXR90" )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.17 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):2.72 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.945 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):-0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):-1.17 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):3.51 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):4.29 sprintf(nil "Vdd" n++) )
            )
	when(maxx==4.68
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x4) "R270" )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.17 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):2.72 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.945 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):3.5 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):4.29 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):5.85 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):5.07 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):-0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):-1.17 sprintf(nil "Vdd" n++) )

            )
	when(maxx==6.24
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x4) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x4) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x4) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x4) "MXR90" )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.17 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):2.72 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.945 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):3.5 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):4.29 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):5.85 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):5.07 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):6.63 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):7.41 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):-0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):-1.17 sprintf(nil "Vdd" n++) )
            )
	when(maxx==9.36
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x1) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x1) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x4) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x4) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x4) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x4) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x4) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x5) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x5) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(y2 x5) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.PLUG.0" "layout" nil list(maxy x5) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x5) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x5) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_BOT.0" "layout" nil list(y1 x5) "MXR90" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.END_TOP.0" "layout" nil list(maxy1 x5) "MXR90" )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.17 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):2.72 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):1.945 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):3.5 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):4.29 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):5.85 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):5.07 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):6.63 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):7.41 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):-0.39 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):-1.17 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):8.98 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):8.2 sprintf(nil "Vdd" n++) )
                  MakeALabel( rcv list("M2" "gnd") (y2+0.13):9.76 sprintf(nil "GND" n++) )
                  MakeALabel( rcv list("M2" "vdd") (y2+0.13):10.54 sprintf(nil "Vdd" n++) )
            )


            x=caar(GetPrbound(CellView)->bBox)
            y=cadar(GetPrbound(CellView)->bBox)-0*DefaultWiringPitch
            maxy2=maxy-1*DefaultWiringPitch
            y4=cadar(GetPrbound(CellView)->bBox)-10*DefaultWiringPitch
            maxy4=maxy+13*DefaultWiringPitch
	when((maxx==6.24)||(maxx==3.12)||(maxx==9.36)
            while(y < maxy2
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.FILL2.0" "layout" nil list(y x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.FILL2.0" "layout" nil list(y maxx) "MXR90" )
              y=y+2*DefaultWiringPitch
            ))
	when(maxx==1.56
            while(y < maxy2
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.FILL2.0" "layout" nil list(y x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.FILL2.0" "layout" nil list(y x1) "R270" )
              y=y+2*DefaultWiringPitch
            ))
	when(maxx==4.68
            while(y < maxy2
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.FILL2.0" "layout" nil list(y x) "R270" )
              dbCreateInstByMasterName( rcv "synthesis.fill" "synthesis.fill.FILL2.0" "layout" nil list(y x4) "R270" )
              y=y+2*DefaultWiringPitch
            ))

            maxx=caadr(GetPrbound(CellView)->bBox)
            maxy=cadadr(GetPrbound(CellView)->bBox)
            x1=caar(GetPrbound(CellView)->bBox)
            x=x1
            n=0
            while(x <= maxx+4*DefaultWiringPitch
              y1=cadar(GetPrbound(CellView)->bBox)
              y=y1
              while(y <= maxy+2*TrackPitch
                  dbCreateInstByMasterName( rcv "globals" "globals.wires.POWER_GRID_M34" "layout" nil list(y x) "R90" )
;                  MakeALabel( rcv list("M4" "gnd") y:x sprintf(nil "GND" n++) )
;                  MakeALabel( rcv list("M4" "gnd") y:x+TrackPitch sprintf(nil "GND" n++) )
;                  if(y==y1 MakeALabel( rcv list("M4" "gnd") y-2*TrackPitch:x sprintf(nil "GND" n++) ))
;                  if(y==y1 MakeALabel( rcv list("M4" "gnd") y-2*TrackPitch:x+TrackPitch sprintf(nil "GND" n++) ))
;                  MakeALabel( rcv list("M4" "vdd") y-TrackPitch:x "Vdd" )
;                  MakeALabel( rcv list("M4" "vdd") y-TrackPitch:x+TrackPitch "Vdd")
;                  if(x+2*TrackPitch > maxx+4*DefaultWiringPitch then
;                      MakeALabel( rcv list("M4" "vdd") y-TrackPitch:x+2*TrackPitch "Vdd" )
;                      MakeALabel( rcv list("M4" "gnd") y:x+2*TrackPitch sprintf(nil "GND" n++) )
;                      if(y==y1 MakeALabel( rcv list("M4" "gnd") y-2*TrackPitch:x+2*TrackPitch sprintf(nil "GND" n++) ))
;                  )
                  y=y+4*DefaultWiringPitch
              )
              x=x+2*TrackPitch
            )

            dbSave( rcv )
            dbClose( rcv )
        else
            printf( "Cannot open cell\n" )
        )
        t
    )
)


(defun MakeAvagoTag ( CellView @key (orientation "R0") (TargetViewName "layout_tag") )
    let( (point trans inst rcv box x y x1 y1 fill)
        trans=list(0:0 orientation 1.0)
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
            inst = dbCreateInst( rcv CellView "layout1" 0:0 orientation )
            if( inst == nil
                printf( "Cannot create inst\n" ))


        ;I have completely removed the original code drawing stripes of implants and wells around the perimeter.
        ;Instead, it will tile vendor.avago.svt.a28_filltap1_a1.0 cells around the edges,
            
            maxx=caadr(GetPrbound(CellView)->bBox)
            maxy=cadadr(GetPrbound(CellView)->bBox)
            x=caar(GetPrbound(CellView)->bBox)
            y=cadar(GetPrbound(CellView)->bBox)
            dbCreateRect(rcv BoundaryLPP list(caar(GetPrbound(CellView)->bBox):y maxx:maxy)) ; adds a prBoundary shape to the tag view
            while(y < maxy-1
              if( (mod (round (y/1.1)) 2) != 0
                 then 
                  dbCreateInstByMasterName( rcv "vendor.avago.svt" "vendor.avago.svt.a28_filltap1_a1.0" "layout" nil list(x y+1.1) "R180" )
                  dbCreateInstByMasterName( rcv "vendor.avago.svt" "vendor.avago.svt.a28_filltap1_a1.0" "layout" nil list(maxx y+1.1) "MX" )
                 else
                  dbCreateInstByMasterName( rcv "vendor.avago.svt" "vendor.avago.svt.a28_filltap1_a1.0" "layout" nil list(x y) "MY" )
                  dbCreateInstByMasterName( rcv "vendor.avago.svt" "vendor.avago.svt.a28_filltap1_a1.0" "layout" nil list(maxx y) "R0" )
              )
              y=y+1.1
            )

            dbSave( rcv )
            dbClose( rcv )
        else
            printf( "Cannot open cell\n" )
        )
        t
    )
)


(defun MakeLeafTagCell ( CellName @key ( ViewName "layout" ) (orientation "MXR90") (TargetViewName "layout_tag") )
    let( ( cv pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName)
        if( cv == nil
            printf( "Cannot open %s\n" CellName)
            MakeLeafTag( cv ?orientation orientation ?TargetViewName TargetViewName ))
        t
    )
)

(defun MakeAvagoTagCell ( CellName @key ( ViewName "layout" ) (orientation "R0") (TargetViewName "layout_tag") )
    let( ( cv pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName)
        if( cv == nil
            printf( "Cannot open %s\n" CellName)
            MakeAvagoTag( cv ?orientation orientation ?TargetViewName TargetViewName ))
        t
    )
)

(defun MakeProteusTagCell ( CellName @key ( ViewName "layout" ) (orientation "R0") (TargetViewName "layout_tag") )
    let( ( cv pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName)
        if( cv == nil
            printf( "Cannot open %s\n" CellName)
            MakeProteusTag( cv ?orientation orientation ?TargetViewName TargetViewName ))
        t
    )
)


(defun MakeLayoutTag ( CellView @key (orientation "MXR90") (TargetViewName "layout_tag") )
  (if (IsLeafCell CellView->cellName) then
    (MakeLeafTag CellView ?orientation orientation ?TargetViewName TargetViewName)
  else
    (MakeMidLayoutTag CellView ?orientation orientation ?TargetViewName TargetViewName)
  )
)

;(defun MakeLayoutTagCell ( CellName @key (ViewName "layout") 
;                                         (orientation "MXR90") 
;                                         (TargetViewName "layout_tag")
;                                         (CastPath ConfigFileGetValue(TheCDSConfigTable "CAST_PATH"))
;                                         (TempDir ConfigFileGetValue(TheCDSConfigTable "TEMP")) )
;   (if (IsLeafCell CellName ?tempdir TempDir ?castpath CastPath) then
;    (MakeLeafTagCell CellName ?ViewName ViewName  ?orientation orientation ?TargetViewName TargetViewName)
;  else
;    (MakeMidLayoutTagCell CellName ?ViewName ViewName  ?orientation orientation ?TargetViewName TargetViewName)
;  )
;) 

(defun MakeLayoutTagCell ( CellName @key (ViewName "layout")
                                         (orientation "MXR90")
                                         (TargetViewName "layout_tag")
                                         (CastPath ConfigFileGetValue(TheCDSConfigTable "CAST_PATH"))
                                         (TempDir ConfigFileGetValue(TheCDSConfigTable "TEMP")) )
   let( (isleaf isavago isoproteus )

      isleaf = (IsLeafCell CellName ?viewname ViewName ?tempdir TempDir ?castpath CastPath)
      isproteus = (IsProteusCell CellName ?viewname ViewName ?tempdir TempDir ?castpath CastPath)
      isavago = (IsAvagoCell CellName ?viewname ViewName ?tempdir TempDir ?castpath CastPath)

   (cond ((when isleaf 
             (MakeLeafTagCell CellName ?ViewName ViewName  ?orientation orientation ?TargetViewName TargetViewName)))
	((when isproteus 
             (MakeProteusTagCell CellName ?ViewName ViewName  ?orientation orientation ?TargetViewName TargetViewName)))
         ((when isavago
	     (MakeAvagoTagCell CellName ?ViewName ViewName  ?orientation "R0" ?TargetViewName TargetViewName)))
         (t  (MakeMidLayoutTagCell CellName ?ViewName ViewName  ?orientation orientation ?TargetViewName TargetViewName))
   )
 )
)

