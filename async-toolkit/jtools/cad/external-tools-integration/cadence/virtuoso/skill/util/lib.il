; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun LibCreateEmptyView ( LibName CellName LibPath TechLibName ViewName ViewType )
  (cond (
         ( not ( ddGetObj LibName CellName ViewName ) )
           (cond (
                  ( not ( ddGetObj LibName  ) )
                  ( ddCreateLib LibName LibPath )
                  ( hiiSetCurrentForm 'tcAttachTechForm)
                  tcAttachTechForm->tcDesignLibCyclic->value = LibName
                  tcAttachTechForm->tcBinaryTFLibCyclic->value = TechLibName
                  ( hiFormDone tcAttachTechForm)
 ) )
           (let (
                 ( CellView ( dbOpenCellViewByType LibName CellName ViewName ViewType "a" ) ) )
             ( dbSave CellView )
             ( dbPurge CellView ) ) ) ) )

(defun LibGetUnusedLibName ( Prefix LibDir )
  (let (
        ( CurrNum 0 ) 
        ( CurrName ( sprintf nil "%s%d" Prefix 0 ) ) )
    (while ( or ( ddGetObj CurrName )
                ( isDir ( sprintf nil "%s/%s" LibDir CurrName ) ) )
      ( setq CurrNum ( plus CurrNum 1 ) )
      ( setq CurrName ( sprintf nil "%s%d" Prefix CurrNum ) ) )
    ( println CurrName )
    CurrName ) )


(defun LibCreateTempLibrary ( TechFileData TempLibNamePrefix LibDir )
  (let (
        ( TempLibName ( LibGetUnusedLibName TempLibNamePrefix LibDir ) ) )
    (let (
          ( TempLibDir ( sprintf nil "%s/%s" LibDir TempLibName ) ) )
      (let (
            ( TempLib ( ddCreateLib TempLibName TempLibDir ) ) )
        ( techBindTechFile TempLib ( getq TechFileData libName ) )
        TempLibName ) ) ) )


(defun LibCreateTempLibraryFromCellView ( CellView TempLibNamePrefix LibDir )
  (let (
        ( TechFileDBObj ( techGetTechFile CellView ) ) )
    ( LibCreateTempLibrary
      TechFileDBObj
      TempLibNamePrefix
      LibDir ) ) )
