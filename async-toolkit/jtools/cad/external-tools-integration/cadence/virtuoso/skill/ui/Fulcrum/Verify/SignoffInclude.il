; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "Create Signoff Include" "UISignoffInclude" )

( UICreateMenuItemCallBack
  `UISignoffIncludeCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) )
      (let (
            ( RunName ( getq CellView cellName ) )
            ( TargetIncludeDir
              ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ) )
        (let (
              ( TargetInclude
                ( sprintf
                  nil "%s/%s.rsf"
                  TargetIncludeDir
                  RunName ) )
              ( SignoffFiles
                ( mapcar 
                  (lambda ( CellView )
                    ( sprintf
                      nil
                      "%s/drc_signoff"
                      ( NameGetCellPathFromCellNameAndView
                        CellView->cellName
                        CellView->viewName ) ) )
                  ( HierarchyGetAllCellsInTree
                    CellView
                    ( append
                      TechLibCellPairRegExs
                      ( append
                        StackLibCellPairRegExs
                        GateLibCellPairRegExs ) ) ) ) ) )
          
          (let (
                ( Cmd 
                  ( sprintf 
                    nil
                    "/usr/intel/bin/perl -I%s/share/script/perl/ve/front-end -I%s/share/script/perl/ve/back-end %s/share/script/perl/ve/front-end/vfe VerificationType=SignOffInclude RunName=%s WorkingDir=%s FlattenCellRegEx.='%s' FlattenCellRegEx.='%s' FlattenCellRegEx.='%s' "
                    ( PackageGetPackageRoot )
                    ( PackageGetPackageRoot )
                    ( PackageGetPackageRoot )
                    RunName
                    TargetIncludeDir
                    ( sprintf nil ".* layout %s" GateLibraryName )
                    ( sprintf nil ".* layout %s" StackLibraryName )
                    ( sprintf nil ".* .* %s" TechLibName )
                    ) ) )
            
            ( foreach SignoffFile SignoffFiles
                      (when ( isFile SignoffFile )
                        ( setq Cmd ( strcat Cmd "SignOff.=" SignoffFile " " ) ) ) )

            ( printf "%s\n" Cmd )
            ( system Cmd )
            ( printf "Include file is %s\n" TargetInclude )
        
) ) ) ) ) )
