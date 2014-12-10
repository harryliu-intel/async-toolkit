; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: Router
; Parameter: TargetViewName (string)
;            DoFile (string)
; Return:  RoutingGood (boolean)
;
; Run Router

defun( Router ( TargetViewName )

  let( ( CellView mycmd TclFile)
    CellView = dbOpenCellViewByType( 
            LibName
            CellName
            TargetViewName 
            "maskLayout"
            "a" ) 
    AddLeafBlockageCell( CellView )
    TclFile=strcat(CellDir "/router_temp.tcl")
    mycmd=sprintf( nil "/usr/bin/sed -e \"s/LIBNAME/%s/g\" -e \"s/CELLNAME/%s/g\" -e \"s/VIEWNAME/%s/g\" -e \"s^EXCLUDENET^%s^g\" <%s >%s\n" LibName CellName TargetViewName RouterExcludeNetFile RouterTemplateTclFile TclFile ) 
    println(mycmd)
    system(mycmd)
    geOpen(?lib LibName ?cell CellName ?view TargetViewName ?mode "a")
    deInstallApp(getCurrentWindow() "Virtuoso XL")

    RoutingGood = rdeSource(TclFile)
    DeleteLeafBlockageCell( CellView )
    dbSave( CellView )
  )
RoutingGood
)


 


