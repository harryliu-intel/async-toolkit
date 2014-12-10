; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


/*DOC
Dominos are used as the abstract type of components in a superstack, which become Domino Stacks.  See <module layout.chain.superstack>.

Data structures:<br>
<dl>
<dt>Domino
<dd>An ordered pair (a,b) of symbols.  Also thought of as an edge in a graph with nodes symbols.

<dt>DominoStack
<dd>An ordered list of (domino parity) pairs.  Also thoughtof as a graph.

<dt>Parity
<dd>nil for unflipped (a,b).  t for flipped (b,a).

<dt>Top/Bottom Domino
The first/last domino in a DominoStack.

<dt>Top/Bottom Symbol
<dd>The first/last symbol of the first/last domino in a DominoStack.

<dt>Ring
<dd>A DominoStack such that the bottom symbol is the same as the top symbol.  Also a cyclic graph.

<dt>NodeIndex
<dd>The index of a node in a DominoStack.  nil for the bottom symbol, t for the top symbol.  An integer 1..(number of dominos) for internal symbol nodes.

</dl>
*/

(defun DominoCreateDomino ( Name
                            BottomSymbol
                            TopSymbol
                            )
  (let (
        ( Domino 
          ( makeTable `foo nil ) ) )
    ( DominoInit
      Domino
      Name
      BottomSymbol
      TopSymbol ) ) )

(defun DominoInit ( Domino
                    Name
                    BottomSymbol
                    TopSymbol
                    )
  ( setarray Domino `name Name )
  ( setarray Domino "1" BottomSymbol )
  ( setarray Domino "2" TopSymbol )
  ( setarray Domino `parity nil ) )
  


(defun DominoGetName ( Domino )
  ( arrayref Domino `name ) )

(defun DominoGetParity ( Domino )
  ( arrayref Domino `parity ) )

(defun DominoSetParity ( Domino Value )
  ( setarray Domino `parity Value ) )

(defun DominoGetBottomSymbol ( Domino )
  (if ( DominoGetParity Domino )
      ( arrayref Domino "2" )
    ( arrayref Domino "1" ) ) )

(defun DominoGetTopSymbol ( Domino )
  (if ( DominoGetParity Domino )
      ( arrayref Domino "1" )
    ( arrayref Domino "2" ) ) )

        
;domino stacks
(defun DominoStackCreateDominoStack ( DominoList )
  (let (
        ( DominoStack ( makeTable `foo nil ) ) )
    ( DominoStackInit 
      DominoStack
      DominoList) ) )

(defun DominoStackInit ( DominoStack
                         DominoList
                         )
  ( setarray DominoStack `list DominoList )
  ( setarray DominoStack `top ( car ( last DominoList ) ) )
  ( setarray DominoStack `bottom ( car DominoList ) )
)

(defun DominoStackGetBottomDomino ( DominoStack )
  ( arrayref DominoStack `bottom ) )

(defun DominoStackGetTopDomino ( DominoStack )
  ( arrayref DominoStack `top ) )

(defun DominoStackGetDominoList ( DominoStack )
  ( arrayref DominoStack `list ) )

(defun DominoStackGetKey ( DominoStack )
  ( DominoGetName
    ( DominoStackGetBottomDomino DominoStack ) ) )
  
(defun DominoStackGetBottomSymbol ( DominoStack )
  ( DominoGetBottomSymbol ( arrayref DominoStack `bottom ) ) )

(defun DominoStackGetTopSymbol ( DominoStack )
  ( DominoGetTopSymbol ( arrayref DominoStack `top ) ) )

(defun DominoStackGetBottomSymbolOfTopDomino ( DominoStack )
  ( DominoGetBottomSymbol ( arrayref DominoStack `top ) ) )

(defun DominoStackGetTopSymbolOfBottomDomino ( DominoStack )
  ( DominoGetTopSymbol ( arrayref DominoStack `bottom ) ) )



(defun DominoStackGetDominoFromDominoStackAndNodeIndex ( DominoStackAndNodeIndex )
  ( car ( DominoStackGetDominoAndNodeIndexFromDominoStackAndNodeIndex DominoStackAndNodeIndex ) ) )

 (defun DominoStackGetDominoAndNodeIndexFromDominoStackAndNodeIndex ( DominoStackAndNodeIndex )
  (cond (
         ( equal ( cadr DominoStackAndNodeIndex ) t )
         ( list ( DominoStackGetTopDomino ( car DominoStackAndNodeIndex ) ) t ) )
        (
         ( equal ( cadr DominoStackAndNodeIndex ) nil )
         ( list ( DominoStackGetBottomDomino ( car DominoStackAndNodeIndex ) ) nil ) )
        ( 
         ( numberp ( cadr DominoStackAndNodeIndex ) )
         ( list ( nth ( cadr DominoStackAndNodeIndex ) ( DominoStackGetDominoList ( car DominoStackAndNodeIndex ) ) ) t ) ) ) )

(defun DominoStackGetSymbolFromDominoStackAndNodeIndex ( DominoStackAndNodeIndex )
  (cond (
         ( equal ( cadr DominoStackAndNodeIndex ) t )
         ( DominoStackGetTopSymbol ( car DominoStackAndNodeIndex ) ) )
        (
         ( equal ( cadr DominoStackAndNodeIndex ) nil )
         ( DominoStackGetBottomSymbol ( car DominoStackAndNodeIndex ) ) )
        ( 
         ( numberp ( cadr DominoStackAndNodeIndex ) )
         ( DominoGetBottomSymbol ( nth ( cadr DominoStackAndNodeIndex ) 
                                            ( DominoStackGetDominoList ( car DominoStackAndNodeIndex ) ) ) ) ) ) )


(defun DominoStackPrintDomino ( Domino )
  ( printf "  %L\n" ( list ( DominoGetName Domino )
                           ( DominoGetBottomSymbol Domino )
                           ( DominoGetTopSymbol Domino )
                           ( DominoGetParity Domino )
                           ;( tableToList Domino )
                           ) )
 )

(defun DominoPrintDominoList ( DominoList )
  ( mapcar 
    (lambda ( Domino )
      ( DominoStackPrintDomino Domino ) )
    DominoList ) )
                          
(defun DominoStackPrintDominoStack ( DominoStack )
  ( printf "domino stack: %s\n" ( DominoStackGetKey DominoStack ) )
  ( DominoPrintDominoList ( DominoStackGetDominoList  DominoStack ) ) )

(defun DominoStackPrintDominoStackSet ( DominoStackSet )
  ( printf "domino stack set ( size %d) \n" ( length DominoStackSet ) )
  ( mapcar 
    (lambda ( DominoStack )
      ( DominoStackPrintDominoStack DominoStack ) )
    ( mapcar `cadr ( tableToList DominoStackSet ) ) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; real functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun DominoReverseDominoList ( DominoList )
  ( foreach Domino DominoList 
            ( setarray Domino `parity 
                       ( null ( arrayref Domino `parity ) ) ) )
  ( reverse DominoList ) )

;first one on bottom, 2 flip bits 
(defun DominoStack ( DominoStack1 DominoStack2 NodeIndex1 NodeIndex2 DominoStackConstructor )
  (let (
        ;reverse the first if we're rotating ( we want this side on top )
        ;or if the bottom side matches 
        ( Reverse1 ( or ( numberp NodeIndex1 ) ( null NodeIndex1 ) ) )
        ;opposite of above
        ( Reverse2 ( and ( not ( numberp NodeIndex2 ) ) NodeIndex2 ) )
        ( RotatedDominoList1
          ( ListRotate ( DominoStackGetDominoList DominoStack1 ) NodeIndex1 ) )
        ( RotatedSplitDominoLists2 ( ListSplit ( DominoStackGetDominoList DominoStack2 ) NodeIndex2 ) ) )
    ;( print ( list 
    ;          RotatedDominoList1
    ;          RotatedSplitDominoLists2 ) )
    (let (
          ;if they're both reversed leave the first alone, reverse the 2nd and swap the two splits
          ;if first is reversed then reverse it, leave 2nd alone
          ;if second is reversed, leave first alone
          ;if neither reversed, leave thm alone
          ( OrientedDominoList1
            (if Reverse1 
                  ( DominoReverseDominoList RotatedDominoList1 )
              RotatedDominoList1 ) )
          ( OrientedSplitDominoLists2 
            (if Reverse2
                (if ( null ( car RotatedSplitDominoLists2 ) )
                    ( list ( DominoReverseDominoList ( car RotatedSplitDominoLists2 ) )
                           ( DominoReverseDominoList ( cadr RotatedSplitDominoLists2 ) ) )
                  ( list ( DominoReverseDominoList ( cadr RotatedSplitDominoLists2 ) )
                         ( DominoReverseDominoList ( car RotatedSplitDominoLists2 ) ) ) )
             RotatedSplitDominoLists2  ) ) )
      ( apply
        DominoStackConstructor
        ( list ( append
                 ( car OrientedSplitDominoLists2 )
                 ( append 
                   OrientedDominoList1
                   ( cadr OrientedSplitDominoLists2 ) ) )
               ( list DominoStack1
                      DominoStack2 )
               ) ) ) ) )

;( greaterp ( DominoCalculateHeight S )

(defun DominoStackRemove ( DominoStack DominoStackSet )
  ( remove ( DominoStackGetKey DominoStack ) DominoStackSet ) )

(defun DominoStackAdd ( DominoStack DominoStackSet )
  ( setarray DominoStackSet ( DominoStackGetKey DominoStack ) DominoStack ) )

(defun DominoStackMerge ( DominoStackSet 
                          DominoStackAndNodeIndex1
                          DominoStackAndNodeIndex2
                          DominoStackConstructor )
  ( DominoStackRemove ( car DominoStackAndNodeIndex1 ) DominoStackSet )
  ( DominoStackRemove ( car DominoStackAndNodeIndex2 ) DominoStackSet )
  (let (
        ( MergedDomino ( DominoStack 
                         ( car DominoStackAndNodeIndex1 )
                         ( car DominoStackAndNodeIndex2 ) 
                         ( cadr DominoStackAndNodeIndex1 )
                         ( cadr DominoStackAndNodeIndex2 )
                          DominoStackConstructor ) ) )
    ( DominoStackAdd MergedDomino DominoStackSet ) )
  t
)

(defun DominoStackGetFirstAcceptableDominoStackAndNodeIndex ( DominoStackSet MergeTestFunc SymbolMatchFunc )
  "Returns the first pair of (superstack,side) pairs that match according to SymbolMatchFunc.  A node is t/nil for top/bottom or an integer representing the index into internal nodes."
  (let (
        ( FirstDominoStackAndNodeIndex nil )
        ( SecondDominoStackAndNodeIndex nil )
        ( DominoStackList ( mapcar `cadr ( tableToList DominoStackSet ) ) ) )

    ;find first stack
    ( exists
      DominoStack
      DominoStackList
      ;that is a ring
      ( and
        ( apply
          SymbolMatchFunc
          ( list ( DominoStackGetTopSymbol DominoStack )
                 ( DominoStackGetBottomSymbol DominoStack ) ) )
        ;and has a point in the ring
        ( exists 
          NodeIndex 
          ( CounterFillList 
            0 
            ( difference 
              ( length 
                ( DominoStackGetDominoList 
                  DominoStack ) ) 1 )
            1 )
          (let (
                ( DominoStackAndNodeIndex
                  ( list DominoStack NodeIndex ) ) )
            ;such that there is another stack
            ( and
              ( exists 
                OtherDominoStack 
                DominoStackList
                ;with a side(t nil) or point internal
                ( exists
                  OtherNodeIndex 
                  ( cons 
                    t
                    ( CounterFillList
                      0 
                      ( difference
                        ( length 
                          ( DominoStackGetDominoList
                            OtherDominoStack ) ) 1 )
                      1 ) )
                  
                  (let (
                        ( OtherDominoStackAndNodeIndex
                          ( list OtherDominoStack OtherNodeIndex ) ) )
                    ;such that the 2 stacks match at those points
                    ( and  
                      ( not 
                        ( equal 
                          ( car DominoStackAndNodeIndex )
                          ( car OtherDominoStackAndNodeIndex ) ) )
                      ( apply
                        MergeTestFunc
                        ( list DominoStackAndNodeIndex 
                               OtherDominoStackAndNodeIndex ) )
                      ( setq SecondDominoStackAndNodeIndex
                             OtherDominoStackAndNodeIndex ) ) ) ) )
              ( setq FirstDominoStackAndNodeIndex
                     DominoStackAndNodeIndex ) ) ) ) ) )
    
    ;if no ring matches, try the same with non-rings
    (if ( not ( and FirstDominoStackAndNodeIndex SecondDominoStackAndNodeIndex ) )
        ( exists
          DominoStack
          DominoStackList
          ( exists NodeIndex ( list t nil )
                   (let (
                         ( DominoStackAndNodeIndex ( list DominoStack NodeIndex ) ) )
                     ( and 
                       ( exists
                         OtherDominoStack
                         DominoStackList
                         ( exists 
                           OtherNodeIndex
                           ( list t nil )
                           (let (
                                 ( OtherDominoStackAndNodeIndex
                                   ( list OtherDominoStack OtherNodeIndex ) ) )
                             ( and     
                               ( not 
                                 ( equal 
                                   ( car DominoStackAndNodeIndex )
                                   ( car OtherDominoStackAndNodeIndex ) ) )
                               ( apply 
                                 MergeTestFunc
                                 ( list DominoStackAndNodeIndex 
                                        OtherDominoStackAndNodeIndex ) )
                               ( setq SecondDominoStackAndNodeIndex 
                                      OtherDominoStackAndNodeIndex ) ) ) ) )
                       ( setq FirstDominoStackAndNodeIndex 
                              DominoStackAndNodeIndex ) ) ) ) ) )
    ;return the 2 stack/side paris that match
    ( list 
      FirstDominoStackAndNodeIndex
      SecondDominoStackAndNodeIndex ) ) )

(defun DominoStackMergeFirstAcceptablePair ( DominoStackSet 
                                             MergeTestFunc
                                             DominoStackConstructor
                                             SymbolMatchFunc
                                                         )
  (let (
        ( MergePair 
          ( DominoStackGetFirstAcceptableDominoStackAndNodeIndex 
            DominoStackSet
            MergeTestFunc
            SymbolMatchFunc
             ) ) )
    (when ( and ( car MergePair ) ( cadr MergePair ) )
      ( DominoStackMerge
        DominoStackSet
        ( car MergePair )
        ( cadr MergePair )
        DominoStackConstructor
         ) ) ) )

(defun DominoStackMergeDominoStackSet ( DominoStackSet
                                        MergeTestFuncInfoList
                                        DominoStackConstructor
                                        SymbolMatchFunc
                                        BVerbose )
  
  ( foreach MergeTestFuncInfo MergeTestFuncInfoList
            (while 
                ( DominoStackMergeFirstAcceptablePair 
                  DominoStackSet
                  (lambda ( DominoStackAndNodeIndex1 DominoStackAndNodeIndex2 )
                    ( apply ( car MergeTestFuncInfo ) 
                            ( append 
                              ( list DominoStackAndNodeIndex1 DominoStackAndNodeIndex2 )
                              ( cadr MergeTestFuncInfo ) ) ) )
                  DominoStackConstructor
                  SymbolMatchFunc )

              (when BVerbose
                ( printf
                  "Verbose:\n"
                  ( DominoStackPrintDominoStackSet
                    DominoStackSet )
                  ) ) ) ) )


(defun DominoStackCutRings ( DominoStackSet
                             PreferredCutSymbols
                             IsSymbolExternalToDominoStackFunc
                             SymbolMatchFunc
                             DominoStackConstructor )
  ( foreach Key DominoStackSet
            (let (
                  ( DominoStack ( arrayref DominoStackSet Key ) ) )
              (let (
                    ( NewDominoStack 
                      ( DominoStackCutRing 
                        DominoStack
                        PreferredCutSymbols
                        IsSymbolExternalToDominoStackFunc
                        SymbolMatchFunc
                        DominoStackConstructor ) ) )

                ( DominoStackRemove DominoStack DominoStackSet )
                ( DominoStackAdd NewDominoStack DominoStackSet )
) ) ) )

(defun DominoStackCutRing ( DominoStack
                            PreferredCutSymbols
                            IsSymbolExternalToDominoStackFunc
                            SymbolMatchFunc
                            DominoStackConstructor )
  "Re-orders a dominostack so that PreferredCutSymbols(power nets) or nets external to the stack appear on the top/botttom"
  (if ( apply SymbolMatchFunc ( list  ( DominoStackGetTopSymbol DominoStack )
                                      ( DominoStackGetBottomSymbol DominoStack ) ) )
      (let (
            ( DominoStackList ( DominoStackGetDominoList DominoStack ) ) )
        (let ( 
              ( CutPoint nil )
              ( CutSymbol nil ) )
          ( for CurrPoint 0 ( difference ( length DominoStackList ) 1 )
                (let (
                      ( DominoStackAndNodeIndex ( list DominoStack CurrPoint ) ) )
                  (let (
                        ( CurrSymbol 
                          ( DominoStackGetSymbolFromDominoStackAndNodeIndex DominoStackAndNodeIndex ) ) )
                    (cond (
                           ( and ( not
                                   ( exists PreferredCutSymbol PreferredCutSymbols
                                            ( apply SymbolMatchFunc 
                                                    ( list CutSymbol PreferredCutSymbol ) ) ) )
                                 ( or                                   
                                   ( exists PreferredCutSymbol PreferredCutSymbols
                                            ( apply SymbolMatchFunc 
                                                    ( list  CurrSymbol PreferredCutSymbol ) ) )
                                   ( apply
                                     IsSymbolExternalToDominoStackFunc
                                     ( list CurrSymbol
                                            DominoStack 
                                            ) ) ) )
                           ( setq CutSymbol CurrSymbol )
                           ( setq CutPoint CurrPoint ) ) ) ) ) )
          (if CutPoint
              (let ( 
                    ( NewDominoStackList ( ListRotate DominoStackList CutPoint ) ) )
                (let (
                      ( NewDominoStack ( apply
                                         DominoStackConstructor
                                         ( list NewDominoStackList
                                                ( list DominoStack ) )
                                         ) ) )
                  NewDominoStack ) )
            DominoStack ) ) )
    DominoStack ) )


