; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

/*DOC
This module has the basic ocde for constructing superstacks from <module layout.chain.componentinfo>s respresenting gates/stacks.<br>

Some data structures:<br>
<dl>
<dt> SuperStackSet
<dd> Represents a set of superstacks.  Maps name of first componentinfo in each superstack to a SuperStackInfo.  

<dt> SuperStackInfo
<dd> A table with entries for: a list of component infos, the min width, and the max width of those components.  We keep the widths around so when we're checking heuristics in <module layout.chain.superstack_tools> we don't have to traverse the componentinfolist.  This contains all the information  we need to create an acutal superstack instance.

<dt> Ring
<dd> If the top of a superstack connects to the bottom, we call it a ring.  This means the top/bottom of the superstack is arbitrary.  Cutting a ring means choosing some component in the superstack to be at the bottom.

<dt> NodeIndex
<dd> t=top of superstack, nil=bottom of superstack, integer=index of locations in superstack.

<dt> SuperStackInfoAndNodeIndex
<dd> (superstackinfo nodeindex).  Corresponds to some location in a superstack that could potentially be connected to a location in another superstack.  This is what we deal with when determining which superstacks can merge together.  Non rings can only merge at their top or bottom, because merging an internal location with another superstack would break the superstack.  For rings, we can choose internal locations (integers) as well, becuase the top or bottom is arbitrary to begin with/

<dt> MergeTestFuncInfoList 
<dd> List of heuristics which take two SuperStackInfoAndNodeIndex's and returns non-nil iff the 2 superstacks can merge together.

<dt> NonPowerMergeTestFuncInfoList 
<dd> List of heuristics to use before allowing mingling of domains.

<dt> PowerMergeTestFuncInfoList 
<dd> List of heuristics to use after domains are mingled.

<dt> Domain
<dd> See <layout.chain.domain>
</dl>  


This is how superstacks are generated:<br>
<ol>
<li> Create componentinfos from some set of gate/stack components.

<li> Determine the connected domains of components, and promote componentinfos to singleton superstacks.  We will merge superstacks into bigger superstacks.

<li> For each domain, greedily attempt to merge the superstacks in the domain together according to NonPowerMergeTestFuncInfoList heuristics.  That is, we just keep picking pairs of SuperStackInfoAndNodeIndex from different superstacks, and then ask the MergeTestFuncInfo heuristics if it's OK for them to merge together.  The simplest heuristic is that the nets must match.  Others say that the widths of the components must be close together, or the stack can't be too tall, or the components must share a non-power net,etc.

<li> Cut the superstack rings so that power nets or nets external to the ring are on the top/bottom.  If we didn't do this then there 

<li> Greedily attempt to merge the stacks in different domains together according to PowerMergeTestFuncInfoList heuristics.  

<li> Cut the superstack rings so that power nets or nets external to the ring are on the top/bottom.

</ol>

*/

;super stack info creation and access
(defun SuperStackCreateSuperStackInfoFromComponentInfo ( ComponentInfo )
  (let ( 
        ( Width ( ComponentInfoGetWidth ComponentInfo ) )
        ( Height ( ComponentInfoGetHeight ComponentInfo ) ) )
    ( SuperStackCreateSuperStackInfo ( list ComponentInfo ) Width Width Height ) ) )

(defun SuperStackCreateSuperStackInfoFromComponentInfoList ( ComponentInfoList 
                                                             SourceSuperStack
                                                             GetOverlapFunc
                                                             )
  (let (
        ( MinWidth 1e9 )
        ( MaxWidth 0 )
        ( Height 
          (when GetOverlapFunc
            ( SuperStackCalculateHeightFromComponentInfos
              ComponentInfoList
              GetOverlapFunc ) ) )
        )
    ( foreach ComponentInfo ComponentInfoList
              (let ( 
                    ( Width ( ComponentInfoGetWidth ComponentInfo ) ) )
                (if ( lessp Width MinWidth )
                    ( setq MinWidth Width ) )
                (if ( greaterp Width MaxWidth )
                    ( setq MaxWidth Width ) ) ) )
    ( SuperStackCreateSuperStackInfo
      ComponentInfoList
      MinWidth 
      MaxWidth
      Height
      ) ) )

(defun SuperStackCreateName ( SuperStackInfo ) 
  (let (
        ( Name "" )
        ( ComponentInfoList ( SuperStackGetComponentInfoList 
                              SuperStackInfo ) ) )
    ( foreach ComponentInfo ComponentInfoList
              ( setq Name ( sprintf nil "%s:%s" Name ( ComponentInfoGetName
                                                       ComponentInfo ) ) ) )
    Name ) )

;for inlining
(defun SuperStackGetComponentNameListFromSuperStackName ( SuperStackName )
  ( parseString SuperStackName ":" ) )

(defun SuperStackGetComponentIndexFromComponentName ( ComponentName  )
  ( rexMatchp "[0-9]+" ComponentName )
  ( atoi ( rexSubstitute "\\0" ) ) )
;;

(defun SuperStackCreateSuperStackInfo ( ComponentInfoList MinWidth MaxWidth Height )
  (let (
        ( SuperStackInfoTable ( makeTable `superstackinfo nil ) ) )
    ( setarray SuperStackInfoTable `minwidth MinWidth )
    ( setarray SuperStackInfoTable `maxwidth MaxWidth )
    ( setarray SuperStackInfoTable `height Height )
    ( DominoStackInit      
      SuperStackInfoTable
      ComponentInfoList )
    SuperStackInfoTable
 ) )

(defun SuperStackGetComponentInfoList ( SuperStackInfo )
  ( arrayref SuperStackInfo `list ) )

(defun SuperStackGetBottomComponentInfo ( SuperStackInfo )
  ( arrayref SuperStackInfo `bottom ) )

(defun SuperStackGetTopComponentInfo ( SuperStackInfo )
  ( arrayref SuperStackInfo `top ) )

(defun SuperStackGetMinWidth ( SuperStackInfo )
  ( arrayref SuperStackInfo `minwidth ) )

(defun SuperStackGetMaxWidth ( SuperStackInfo )
  ( arrayref SuperStackInfo `maxwidth ) )

(defun SuperStackGetHeight ( SuperStackInfo )
  ( arrayref SuperStackInfo `height ) )

(defun SuperStackGetSuperStackKey ( SuperStackInfo )
  ( ComponentInfoGetName
    ( SuperStackGetBottomComponentInfo SuperStackInfo ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creation of PCell  Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SuperStackParameter = ( list ComponentParameterInfo... )
;ComponentParameterInfo = ( list Parity ComponentParameterList )
;ComponentParameterList = the pcell parameter list for the chain

(defun SuperStackCreateSuperStackParameterFromSuperStackInfo ( SuperStackInfo )
  (let (
        ( LastComponentInfo nil ) )

    ( mapcar 
      (lambda ( ComponentInfo )
        (let (
              ( Parameter
                ( list 
                  ( ComponentInfoGetLibCellView
                    ComponentInfo )
                  ( ComponentInfoGetParity 
                    ComponentInfo )
                  ( ComponentInfoGetParameterList
                    ComponentInfo )
                  (when ( and LastComponentInfo
                              ( SuperStackSymbolMatch
                                ( DominoGetBottomSymbol ComponentInfo )
                                ( DominoGetTopSymbol LastComponentInfo ) ) )
                    t ) ) ) )
          ( setq LastComponentInfo ComponentInfo )
          Parameter
          ) )
      ( SuperStackGetComponentInfoList SuperStackInfo ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connecting the superstack instance terminals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SuperStackConnect ( CellView SuperStack SuperStackInfo ComponentPrefix )
  (let (
        ( CurrNum 0 ) )
    ( foreach ComponentInfo ( SuperStackGetComponentInfoList SuperStackInfo )
                ( mapcar
                  (lambda ( Conn )
                    (let (
                          ( TermName ( sprintf nil "%s%d.%s" ComponentPrefix CurrNum ( car Conn ) ) )
                          ( NetName ( cadr Conn ) ) )
                      (if ( dbFindTermByName ( getq SuperStack master ) TermName )
                          ( dbCreateConnByName
                            ( dbMakeNet CellView NetName )
                            SuperStack
                            TermName ) ) ) )
                  ( tableToList ( ComponentInfoGetConnTable ComponentInfo ) ) )
                ( setq CurrNum ( plus CurrNum 1 ) ) ) ) )


(defun SuperStackIsSymbolExternalToSuperStackInfo ( Symbol 
                                                    SuperStackInfo
                                                    NetTable )
  ( and
    ( SuperStackIsNetExternalToSuperStackInfo ( car Symbol )
                                              SuperStackInfo
                                              NetTable )
    ( SuperStackIsNetExternalToSuperStackInfo ( cadr Symbol )
                                              SuperStackInfo
                                              NetTable ) ) )
    
(defun SuperStackIsNetExternalToSuperStackInfo ( NetName
                                                 SuperStackInfo
                                                 NetTable )
  (if NetName
      (let (
            ( TermTable ( arrayref NetTable NetName ) ) )
        ;are tere any connected instances ( through instTerms ) that aren't in the stack
        ( or ( arrayref TermTable "EXTERNAL" )
             ( exists Key TermTable
                      ( exists InstanceName ( arrayref TermTable Key )                
                               ( not ( exists ComponentInfo ( SuperStackGetComponentInfoList SuperStackInfo )
                                              ( equal
                                                ( ComponentInfoGetName ComponentInfo )
                                                InstanceName ) ) ) ) ) ) ) ) )


;;
;; Calculate actual height
;;

(defun SuperStackCalculateHeightFromComponentInfoList ( ComponentInfoList
                                                        GetOverlapFunc )

  (let (
        ( Height 0.0 )
        ( LastComponentInfo nil ) )
    ( foreach ComponentInfo ComponentInfoList
              (let (
                    ( Overlap
                      (if ( and LastComponentInfo
                                ( not 
                                  ( or
                                    ( equal 
                                      ( PropGetPropValueFromPropertyList
                                        ( ComponentInfoGetParameterList ComponentInfo )
                                        "well_plugs" )
                                      "TRUE" )
                                    ( equal 
                                      ( PropGetPropValueFromPropertyList
                                        ( ComponentInfoGetParameterList LastComponentInfo )
                                        "well_plugs" )
                                      "TRUE" ) ) ) )
                          ( apply
                            GetOverlapFunc 
                            ( list ( ComponentInfoGetBottomCSInfo
                                     ComponentInfo )
                                   ( ComponentInfoGetTopCSInfo
                                    LastComponentInfo )
                                  ( SuperStackSymbolMatch
                                    ( DominoGetBottomSymbol
                                      ComponentInfo )
                                    ( DominoGetTopSymbol
                                      LastComponentInfo ) ) ) )
                        0.0 ) ) )

                /*
                ( printf "component height: %L overlap: %L\n" 
                         ( ComponentInfoGetHeight ComponentInfo )
                         Overlap )
                */
                ( setq Height 
                       ( plus Height 
                              ( difference ( ComponentInfoGetHeight ComponentInfo )
                                           Overlap ) ) )

                      
                ( setq LastComponentInfo ComponentInfo )
                ) )
    Height ) )

(defun SuperStackCalculateHeight ( SuperStackInfo 
                                   GetOverlapFunc )

  ( SuperStackCalculateHeightFromComponentInfoList 
    ( SuperStackGetComponentInfoList SuperStackInfo )
    GetOverlapFunc ) )

(defun SuperStackGetDefaultConstructor ( GetOverlapFunc )
  (lambda ( ComponentInfoList
            SourceSuperStackInfos )
    (let (
          ( MinWidth (if SourceSuperStackInfos
                         ( ListFindMinimum
                           SourceSuperStackInfos
                           (lambda ( SuperStackInfo )
                             ( SuperStackGetMinWidth SuperStackInfo ) ) )
                       ( ListFindMinimum  
                         ComponentInfoList 
                         (lambda ( ComponentInfo )
                           ( ComponentInfoGetWidth
                             ComponentInfo ) ) )
                         ) )
          ( MaxWidth (if SourceSuperStackInfos
                         ( ListFindMaximum
                           SourceSuperStackInfos
                           (lambda ( SuperStackInfo )
                             ( SuperStackGetMaxWidth SuperStackInfo ) ) )
                       ( ListFindMaximum
                         ComponentInfoList 
                         (lambda ( ComponentInfo )
                           ( ComponentInfoGetWidth
                             ComponentInfo ) ) ) ) )
            ( Height
              ( SuperStackCalculateHeightFromComponentInfoList 
                ComponentInfoList
                GetOverlapFunc ) )
            )
      ( SuperStackCreateSuperStackInfo
        ComponentInfoList         
        MinWidth
        MaxWidth
        Height
        ) ) ) )

(defun SuperStackFlattenSubSuperStackSets ( SuperStackSets )
  "trivially merges a list of superstack sets into a single superstack set"
  (let ( 
        ( NewSuperStackSet ( makeTable `superstack nil ) ) )
    ( foreach SuperStackSet SuperStackSets
              ( foreach Entry ( tableToList SuperStackSet )
                        ( setarray NewSuperStackSet ( car Entry ) ( cadr Entry ) ) ) )
    NewSuperStackSet ) )


(defun SuperStackCreateFromSuperStackSet ( CellView
                                           SuperStackSet
                                           SuperStackLibName
                                           SuperStackCellName
                                           SuperStackViewName
                                           ComponentPrefix ;prefix for instance/terminal names
                                           BVerbose
                                           @key 
                                           ( Align "ll" )
                                           )
  "Creates superstack instances"
  (let (
        ( ListOfSuperStackInfoSuperStackParameterPairs
          ( SuperStackCreateListOfSuperStackInfoSuperStackParameterPairsFromSuperStackSet 
            SuperStackSet ) ) )
    ( mapcar 
      (lambda ( SuperStackInfoSuperStackParameterPair )
        (let (
              ( SuperStackParameter ( cadr SuperStackInfoSuperStackParameterPair ) )
              ( SuperStackInfo ( car SuperStackInfoSuperStackParameterPair ) ) )
          (let (
                ( AlignPoint
                  (cond (
                         ( equal Align "ll" )
                         ( list
                           ( ListFindMinimum
                             ( SuperStackGetComponentInfoList
                               SuperStackInfo )
                             (lambda ( ComponentInfo )
                               ( RectGetLeft ( ComponentInfoGetBBox ComponentInfo ) ) ) )
                           ( ListFindMinimum
                             ( SuperStackGetComponentInfoList
                               SuperStackInfo )
                             (lambda ( ComponentInfo )
                               ( RectGetBottom ( ComponentInfoGetBBox ComponentInfo ) ) ) ) ) )
                        (
                         ( equal Align "lr" )
                         ( list
                           ( ListFindMaximum
                             ( SuperStackGetComponentInfoList
                               SuperStackInfo )
                             (lambda ( ComponentInfo )
                               ( RectGetRight ( ComponentInfoGetBBox ComponentInfo ) ) ) )
                           ( ListFindMinimum
                             ( SuperStackGetComponentInfoList
                               SuperStackInfo )
                             (lambda ( ComponentInfo )
                               ( RectGetBottom ( ComponentInfoGetBBox ComponentInfo ) ) ) ) ) ) ) )
                ( Orient 
                  (if ( ComponentInfoGetHParity 
                        ( DominoStackGetBottomDomino SuperStackInfo ) )
                      "MY" "R0" ) )
                )
            (let (
                  ( SuperStack 
                    ( dbCreateParamInstByMasterName
                      CellView
                      SuperStackLibName
                      SuperStackCellName
                      SuperStackViewName
                      nil
                      AlignPoint
                      "R0"
                      1
                      ( list 
                        ( list "ParameterInfoList" "ILList" SuperStackParameter ) ) ) ) )
              ;set name
              ( dbReplaceProp
                SuperStack
                "name"
                "string"
                ( SuperStackCreateName SuperStackInfo )
                )

              ;connect terminals
              ( SuperStackConnect 
                CellView
                SuperStack
                SuperStackInfo 
                ComponentPrefix )
                
              ( FigMove
                SuperStack
                AlignPoint
                ?Orient Orient
                ?Align Align
                )

              SuperStack
                ) ) ) )
      ListOfSuperStackInfoSuperStackParameterPairs ) ) )

(defun SuperStackSortValueFunc ( SuperStackInfo )
  ( cadr
    ( ComponentInfoGetPosition
      ( SuperStackGetBottomComponentInfo
        SuperStackInfo ) ) ) )

(defun SuperStackCreateListOfSuperStackInfoSuperStackParameterPairsFromSuperStackSet ( SuperStackSet )
  ( mapcar 
    (lambda ( SuperStackInfo )
      ( list 
        SuperStackInfo 
        ( SuperStackCreateSuperStackParameterFromSuperStackInfo
          SuperStackInfo
          ) ) )
    ( sort
      ( mapcar `cadr ( tableToList SuperStackSet ) )
      (lambda ( X Y )
        ( lessp
          ( SuperStackSortValueFunc X )
          ( SuperStackSortValueFunc Y )
        ) ) ) ) )


;maps net names to terminal tables
;terminal tables map terminal type to list of instance names
(defun SuperStackCreateNetTable ( CellView ComponentInfos OtherInstances )
  (let ( 
        ( NetTable ( makeTable `net nil ) ) )
    ( SuperStackAddComponentInfosToNetTable NetTable ComponentInfos )
    ( SuperStackAddInstancesToNetTable NetTable OtherInstances )
    ( SuperStackAddExternalToNetTable NetTable CellView )
    NetTable ) )

(defun SuperStackCreateNetTableEntry ( NetTable NetName TermName InstanceName )
  (let (
        ( TermTable 
          (cond (
                 ( arrayref NetTable NetName ) )
                (
                 ( makeTable `term nil ) ) ) ) )
    ( setarray NetTable NetName TermTable )
    ( setarray TermTable TermName ( cons InstanceName
                                         ( arrayref TermTable TermName ) ) ) ) )

(defun SuperStackAddExternalToNetTable ( NetTable CellView )
  ( foreach Net ( getq CellView nets )
            (if ( getq Net term )
                ( SuperStackCreateNetTableEntry NetTable ( getq Net name ) "EXTERNAL" nil ) ) ) )

(defun SuperStackAddComponentInfosToNetTable ( NetTable ComponentInfos )
    ( foreach ComponentInfo ComponentInfos 
              ( mapcar
                (lambda ( Conn )
                  ( SuperStackCreateNetTableEntry NetTable
                                                  ( cadr Conn )
                                                  ( car Conn )
                                                  ( ComponentInfoGetName ComponentInfo ) ) )

                ( tableToList ( ComponentInfoGetConnTable ComponentInfo ) ) ) ) )

(defun SuperStackAddInstancesToNetTable ( NetTable Gates )
  ( foreach Gate Gates
            ( foreach InstTerm ( getq Gate conns )
                      ( SuperStackCreateNetTableEntry NetTable
                                                      ( getq ( getq InstTerm net ) name ) 
                                                      ( getq InstTerm name )
                                                      ( getq Gate name ) ) ) ) )
                          

(defun SuperStackSymbolMatch ( SuperStackSymbol1 SuperStackSymbol2 )
  ( and 
    ( or 
      ( equal ( car SuperStackSymbol1 ) ( car SuperStackSymbol2 ) )
      ( null ( car SuperStackSymbol1 ) )
      ( null ( car SuperStackSymbol2 ) ) )

    ( or 
      ( equal ( cadr SuperStackSymbol1 ) ( cadr SuperStackSymbol2 ) )
      ( null ( cadr SuperStackSymbol1 ) )
      ( null ( cadr SuperStackSymbol2 ) ) )
) )

(defun SuperStackCreateSingletonSuperStackSetsFromComponentInfos ( ComponentInfos
                                                                   NetTable
                                                                   GNDNetName
                                                                   VddNetName
                                                                   )
  "Seed a list of superstacksets with a componentinfo from each domain"
  (let (
        ( Domains ( SuperStackGetDomains 
                    ComponentInfos 
                    NetTable
                    ( list GNDNetName
                           VddNetName ) ) ) )
    ( mapcar 
      (lambda ( Domain )
        (let (
              ( SuperStackSet ( makeTable `superstack nil ) ) )
          ( foreach ComponentInfo Domain
                    (let (
                          ( SuperStackInfo 
                            ( SuperStackCreateSuperStackInfoFromComponentInfo ComponentInfo ) ) )
                      ( setarray SuperStackSet ( SuperStackGetSuperStackKey SuperStackInfo ) SuperStackInfo ) ) )
          SuperStackSet ) )
      Domains ) ) )



(defun SuperStackCreateSuperStackSetFromComponentInfos
  ( NetTable
    ComponentInfos
    NonPowerMergeTestFuncInfoList 
    PowerMergeTestFuncInfoList 
    SuperStackSymbolMatchFunc
    GetOverlapFunc
    GNDNetName
    VddNetName
    BCutRings
    BVerbose )

  (let (
        ( SuperStackIsSymbolExternalToSuperStackFunc
          (lambda ( Symbol SuperStackInfo )
            ( SuperStackIsSymbolExternalToSuperStackInfo
              Symbol
              SuperStackInfo
              NetTable ) ) )
        ( DominoStackConstructor
          ( SuperStackGetDefaultConstructor GetOverlapFunc ) )
        ;seed initial superstack for each domain
        ( DomainSuperStackSets 
          ( SuperStackCreateSingletonSuperStackSetsFromComponentInfos
            ComponentInfos
            NetTable
            GNDNetName
            VddNetName
            ) ) )

      ( foreach 
        SuperStackSet
        DomainSuperStackSets 
        ;merge together stacks using 
        ;the heuristics in NonPowerMergeTestFuncInfoList
        ;superstacks stay within their domain at this point
        ( DominoStackMergeDominoStackSet
          SuperStackSet
          NonPowerMergeTestFuncInfoList
          DominoStackConstructor
          SuperStackSymbolMatchFunc
          BVerbose )
        (when BCutRings 
          ( DominoStackCutRings 
            SuperStackSet
            ( list ( list 
                     GNDNetName
                     VddNetName ) )
            SuperStackIsSymbolExternalToSuperStackFunc
            SuperStackSymbolMatchFunc
            DominoStackConstructor
            ) )
        )

      ;merge the domains together using the more conservative 
      ; heuristics in PowerMergeTestFuncInfoList
      (let ( 
            ( DomainSuperStackSet 
              ( SuperStackFlattenSubSuperStackSets
                DomainSuperStackSets ) ) )
        ( DominoStackMergeDominoStackSet
          DomainSuperStackSet
          PowerMergeTestFuncInfoList
          DominoStackConstructor
          SuperStackSymbolMatchFunc
          BVerbose )
        (when BCutRings
          ( DominoStackCutRings
            DomainSuperStackSet
            ( list ( list 
                     GNDNetName
                     VddNetName ) )
            SuperStackIsSymbolExternalToSuperStackFunc
            SuperStackSymbolMatchFunc
            DominoStackConstructor ) )
        DomainSuperStackSet ) ) )
