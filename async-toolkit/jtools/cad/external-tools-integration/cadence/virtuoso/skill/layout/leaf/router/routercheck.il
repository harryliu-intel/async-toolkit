; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: RouterCheck
; Parameter: CellView (CVId)
;            DRCRuleFile (string)
;            DRCAssuraSets (list)
; Return:  RoutingGood (boolean)
;
; Check DRC on routed view

defun( GetCastSpecDir ( CastPath )
  let((PathList SpecDirs SpecDir CastDirs CastDir)
    PathList= parseString(CastPath ":")
    SpecDirs=rexMatchList("spec" PathList) 
    CastDirs=rexMatchList("cast" PathList)
    CastDir=car(CastDirs)
    while(CastDirs=cdr(CastDirs)
      CastDir=strcat(CastDir ":" car(CastDirs))
    )
    SpecDir=car(SpecDirs)
    while(SpecDirs=cdr(SpecDirs)
      SpecDir=strcat(SpecDir ":" car(SpecDirs))
    )
    list( CastDir SpecDir)

  )
)


defun( RouterCheck ( CellView )
  let((glc_dfII_dir lve_dir lve_dfII_dir libdir libpath run_file p_out p_in hlvs_result_file hlvs_result )
    glc_dfII_dir=strcat( WorkingDir "/dfII" )
    lve_dir=strcat( WorkingDir "/lve" )
    lve_dfII_dir=strcat( WorkingDir "/lve/dfII" )
    system(strcat("mkdir -p " lve_dfII_dir))
    libname=car( NameParseCellName( CellName ))
    rexCompile("\\.")
    cadenceLibName=rexReplace(libname "#2e" 0)
    libdir=rexReplace(libname "/" 0)
    rexCompile("/[0-9a-zA-Z_]+$")
    libpath=rexReplace(libdir "" 0)
    system(sprintf( nil "mkdir -p %s/%s" lve_dfII_dir libpath))
    system(sprintf( nil "ln -s %s %s/%s" glc_dfII_dir lve_dfII_dir libdir))
    system(sprintf( nil "echo 'DEFINE %s %s' > %s/cds.lib.generated" cadenceLibName libdir lve_dfII_dir ))
    
    run_file=sprintf( nil "run_%s.sh" CellName )
    CastSpecDir = GetCastSpecDir( ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" ))

    p_out=outfile(strcat( lve_dir "/" run_file))
    fprintf( p_out "fulcrum --latest --pdk=tsmc28 --cadence=icc,assura_oa lve ")
    fprintf( p_out "--cast-dir=%s " car(CastSpecDir))
    fprintf( p_out "--spec-dir=%s " cadr(CastSpecDir))
    fprintf( p_out "--dfII-dir=%s " lve_dfII_dir)
    fprintf( p_out "--output-dir=%s " lve_dir)
    fprintf( p_out "--task=hlvs --nowellplugs=1 ")
    fprintf( p_out "--extracted-view=%s " CellView->viewName)
    fprintf( p_out "%s " CellName)
    close(p_out)
    system(sprintf(nil "cd %s; sh %s" lve_dir run_file))

    rexCompile("\\.")
    celldir=rexReplace(CellName "/" 0)
    hlvs_result_file=sprintf(nil "%s/%s/%s/hlvs.result" lve_dir celldir CellView->viewName)
    p_in=infile(hlvs_result_file)
    fscanf( p_in "%s" hlvs_result )
    close(p_in)
    RoutingGood =  hlvs_result=="PASS"  
  )
  RoutingGood 
)


