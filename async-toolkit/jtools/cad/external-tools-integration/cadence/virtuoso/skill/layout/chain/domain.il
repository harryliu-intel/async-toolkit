; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

/*DOC
Creates a DomainTable whose values are the domains of a cell.
This is used in <module layout.chain.superstack>
A domain is a set of componentinfos connected through a non-power net
*/

(defun SuperStackGetDomainTable ( ComponentInfos 
                                  NetTable
                                  PowerSymbol )

 (let ( 
        ;maps net name to domain 
        ( DomainTable ( makeTable `domain nil ) )  
        ;maps chain name to chain info
        ( ComponentInfoTable ( makeTable `chaininfos nil ) ) )
   ( foreach ComponentInfo ComponentInfos
             ( setarray ComponentInfoTable ( ComponentInfoGetName ComponentInfo ) ComponentInfo ) )

    ( SuperStackTraverseNetGraph
      ComponentInfoTable
      DomainTable 
      NetTable
      nil
      nil
      PowerSymbol
      PowerSymbol )
    DomainTable ) )

(defun SuperStackGetDomains ( ComponentInfos 
                              NetTable
                              PowerSymbol )

  (let (
        ( DomainTable ( SuperStackGetDomainTable 
                        ComponentInfos 
                        NetTable
                        PowerSymbol ) ) )

  ;  ( printf "Domains\n" )
  ;  ( foreach Table ( mapcar `cadr ( tableToList DomainTable ) )
  ;            ( printf "...\n" )
  ;            ( DominoPrintDominoList ( mapcar `cadr ( tableToList Table ) ) ) )
    ; ( shudder )
    (let (
          ( Domains ( mapcar 
                      (lambda ( Table )
                        ( mapcar `cadr ( tableToList Table ) ) )
                      ( mapcar `cadr ( tableToList DomainTable ) ) ) )
          ( DomainList nil ) )
      ( foreach Domain Domains
                (if ( not ( exists OtherDomain DomainList 
                                   ( exists ComponentInfo Domain
                                            ( exists OtherComponentInfo OtherDomain
                                                     ( equal ComponentInfo OtherComponentInfo ) ) ) ) )
                    ( setq DomainList ( cons Domain DomainList ) ) ) )
      (let (
            ( LeftOver ( ListDifference
                         ComponentInfos
                         ( ListNonDestructiveMapCan
                           (lambda ( x ) x )
                           DomainList )
                         (lambda ( x y ) ( equal x y ) )
                         nil ) ) )
        (if LeftOver
            ( cons LeftOver DomainList )
          DomainList ) ) ) ) )


(defun SuperStackGetComponentInfosOnNetName ( ComponentInfoTable NetTable NetName )
  ( mapcar 
    `car
    ( SuperStackGetComponentInfoSidePairsOnNetName ComponentInfoTable NetTable NetName ) ) )
  
(defun SuperStackGetComponentNameSidePairsOnNetName ( ComponentInfoTable NetTable Symbol NetName )
;  ( println NetName )  
  (let (
        ( AttachedInstanceNames
          ( ListRemoveDuplicatesFromStringList
            ( ListNonDestructiveMapCan
              (lambda ( Names ) Names )
              ( mapcar `cadr ( tableToList ( arrayref NetTable NetName ) ) ) ) ) ) )
    ( ListApplyFuncToListAndAccumulateNonNilResults  
      AttachedInstanceNames
      (lambda ( ComponentInfoName )
        (let ( 
              ( ComponentInfo ( arrayref ComponentInfoTable ComponentInfoName ) ) )
          (when ComponentInfo
            (cond (
                   ( SuperStackSymbolMatch 
                     ( DominoGetTopSymbol ComponentInfo ) 
                     Symbol )
                   ( list ComponentInfoName t ) )
                  (
                   ( SuperStackSymbolMatch 
                     ( DominoGetBottomSymbol ComponentInfo )
                     Symbol )
                   ( list ComponentInfoName nil ) ) ) ) ) )
      nil ) ) )

(defun SuperStackGetComponentInfosOnSymbol ( ComponentInfoTable NetTable Symbol )
  (when ( or ( car Symbol ) ( cadr Symbol ) )
      (let (
            ( ComponentNameSidePairsOnLeftNet
              (if ( car Symbol )
                  ( SuperStackGetComponentNameSidePairsOnNetName
                    ComponentInfoTable 
                    NetTable
                    Symbol
                    ( car Symbol ) ) ) )
            ( ComponentNameSidePairsOnRightNet
              (if ( cadr Symbol )
                  ( SuperStackGetComponentNameSidePairsOnNetName
                    ComponentInfoTable 
                    NetTable
                    Symbol
                    ( cadr Symbol ) ) ) ) )
        ( mapcar
          (lambda ( ComponentNameSidePair )
            ( arrayref ComponentInfoTable ( car ComponentNameSidePair ) ) )
          (cond (
                 ( null ( car Symbol ) )
                 ComponentNameSidePairsOnRightNet )
                (
                 ( null ( cadr Symbol ) )
                 ComponentNameSidePairsOnLeftNet )
                (
                 t
                 ( ListUnionNoTableElements
                   ComponentNameSidePairsOnLeftNet
                   ComponentNameSidePairsOnRightNet ) ) ) ) ) ) )


(defun SuperStackDomainGetOtherSymbol ( ComponentInfo
                                        Symbol )
  (if ( car ( DominoGetTopSymbol ComponentInfo ) )
      (if ( equal ( car ( DominoGetTopSymbol ComponentInfo ) ) ( car Symbol ) )
          ( DominoGetBottomSymbol ComponentInfo )
        ( DominoGetTopSymbol ComponentInfo ) )
    (if ( equal ( cadr ( DominoGetTopSymbol ComponentInfo ) ) ( cadr Symbol ) )
        ( DominoGetBottomSymbol ComponentInfo )
      ( DominoGetTopSymbol ComponentInfo ) ) ) )


(defun SuperStackTraverseNetGraph ( ComponentInfoTable DomainTable NetTable DomainSymbolTable DomainKey Symbol PowerSymbol )
  (let (
        ( ComponentInfosOnSymbol
          ( SuperStackGetComponentInfosOnSymbol ComponentInfoTable NetTable Symbol ) ) )
    ( foreach ComponentInfo ComponentInfosOnSymbol     
              (let ( 
                    ( OtherSymbol
                      ( SuperStackDomainGetOtherSymbol
                        ComponentInfo
                        Symbol ) ) )
                (let (
                      ( DomainComponentTable
                        (if ( arrayref DomainTable DomainKey ) 
                            ( arrayref DomainTable DomainKey ) 
                          ( makeTable `foo nil ) ) )
                      ( DomainSymbolTable 
                        (if DomainSymbolTable DomainSymbolTable ( makeTable `foo nil ) ) )
                      ( DomainKey 
                        (if DomainKey DomainKey OtherSymbol ) ) )

                  ( setarray DomainTable DomainKey DomainComponentTable )
                  ( setarray DomainComponentTable
                             ( ComponentInfoGetName ComponentInfo )
                             ComponentInfo )
                  ( setarray DomainSymbolTable 
                             Symbol 
                             Symbol )
                  (let (
                        ( AlreadyWalked
                          ( arrayref DomainSymbolTable OtherSymbol ) ) )
                    ( setarray DomainSymbolTable 
                               OtherSymbol
                               OtherSymbol )
                    (when  ( and ( not AlreadyWalked )
                                 ( not ( SuperStackSymbolMatch PowerSymbol OtherSymbol )  ) )
                      ( SuperStackTraverseNetGraph 
                        ComponentInfoTable
                        DomainTable 
                        NetTable
                        DomainSymbolTable 
                        DomainKey
                        OtherSymbol 
                        PowerSymbol ) ) ) ) ) ) ) )

/*
dt =
( SuperStackGetDomains;Table
  ( mapcar `ComponentInfoCreateComponentInfoFromComponent 
           (wcv)->instances )
  ( SuperStackCreateNetTable
    (wcv) 
    ( mapcar `ComponentInfoCreateComponentInfoFromComponent 
             (wcv)->instances )
    nil ) 
  ( list "GND" "Vdd" ) )

( println 
  ( mapcar
  (lambda ( L )
    ( mapcar
      `ComponentInfoGetName
      L ) )
  dt ) )
*/
