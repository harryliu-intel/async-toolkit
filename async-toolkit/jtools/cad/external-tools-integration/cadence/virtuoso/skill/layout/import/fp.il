; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


;FPable: ( orient, x, y ) -> instance name
;derived from .fp files from Place and Route flow ( Astro )

(defun FPRenameInstancesFromFPTable ( Instances
                                      FPTable
                                      RenameFunc )

  ( rexCompile "^I[0-9]*$" )

  ( foreach Instance 
            Instances
            (let (
                  ( Key 
                    ( list 
                      Instance->orient
                      ( atof 
                        ( sprintf 
                          nil
                          "%f"
                          ( MathRoundToNearest
                            ( car Instance->xy )
                            .001 ) ) )
                      ( atof 
                        ( sprintf 
                          nil
                          "%f"
                          ( MathRoundToNearest
                            ( cadr Instance->xy )
                            .001
                          ) ) )
                      ) ) )
              (let (
                    ( NewInstanceName 
                      ( arrayref FPTable 
                                 Key 
                                 ) ) )

                (when ( stringp NewInstanceName )
                  ( dbSetq Instance
                           ( apply RenameFunc ( list NewInstanceName ) )
                           name ) )
                
;                ( println ( list Key NewInstanceName ) )
              

                (when ( rexExecute 
                        Instance->name )
                  (error 
                   ( sprintf
                     nil
                     "Instance %s at %L has a bad name\n" 
                     Instance->name
                     ( list Instance->xy Instance->orient )
                     ) ) ) ) ) ) )

(defun FPRenameInstancesDefault ( CellView
                                  FPTable
                                  )


  (let (
        ( Instances
          ( car 
            ( NameFilterInstances
              ( getq CellView instances )
              ( list
                ( list ".*" ".*via.*" )
                ( list ".*" ".*FILL.*" )
                ) ) ) ) )

    ( FPRenameInstancesFromFPTable 
      Instances
      FPTable
      `FPDefaultRename ) ) )

(defun FPDefaultRename ( Name )
  (let ( 
        ( Result "i_" )
        ( FirstChar t )
        )
    ( for I 1 ( strlen Name )
          (let (
                ( CurrChar ( substring Name I 1 ) ) )
            (let (
                  ( ConvertedChar (cond
                                   ( ( equal "." CurrChar ) "_D_" )
                                   ( ( and ( equal "x" CurrChar )
                                           FirstChar )
                                     "" )
                                   ( t CurrChar ) ) ) )
              ( setq FirstChar nil )
              ( setq Result ( strcat Result ConvertedChar ) ) ) ) )
    Result
    ) )
