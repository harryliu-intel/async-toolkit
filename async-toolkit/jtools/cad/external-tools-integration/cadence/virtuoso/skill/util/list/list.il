
					;Non-recursive function to process a list and store
					;list of the results from processing
					;each element in the list.

(defun ListApplyFuncToListAndAccumulateResults ( List ApplyFunc ApplyFuncParams )
  ( mapcar
    '(lambda
       ( Element )
       ( apply
	 ApplyFunc
	 ( cons
	   Element
	   ApplyFuncParams ) ) )
    List ) )


(defun ListApplyFuncToListAndAccumulateNonNilResults ( List ApplyFunc ApplyFuncParams )
  ( setof
    Ele
    ( ListApplyFuncToListAndAccumulateResults
      List
      ApplyFunc
      ApplyFuncParams
      )
    ( not ( null Ele ) ) ) )

(defun ListApplyFuncToListAndAccumulateResult ( List ApplyFunc ApplyFuncParams InitialResult )
  (let (
	( CurrentResult InitialResult )
	)
    ( foreach
      Item
      List
      ( setq
	CurrentResult
	( apply
	  ApplyFunc
	  ( cons
	    Item
	    ( cons
	      CurrentResult
	      ApplyFuncParams ) ) ) ) )
    CurrentResult ) )
      

(defun ListFindBestNonRedundantElements ( List RedundantFunc CrappinessCompareFunc )
  (let (
        ( SortedElements ( sort ( copy List ) CrappinessCompareFunc ) ) )
    ( ListFindBestNonRedundantElementsPreSorted
      SortedElements
      RedundantFunc ) ) )

(defun ListFindBestNonRedundantElementsPreSorted ( SortedElements RedundantFunc )
  (let (
        ( ElementsToKeep nil )        
        ( Table ( makeTable `bla nil ) ) )
    (while SortedElements
      (if ( not ( apply RedundantFunc ( list ( car SortedElements ) ( append ElementsToKeep ( cdr SortedElements ) ) ) ) )
          ( setq ElementsToKeep ( cons ( car SortedElements ) ElementsToKeep ) ) )
      ( setq SortedElements ( cdr SortedElements ) ) )
    ElementsToKeep ) )
             

(defun ListGreedilyFindMinimalElementsWithSortFuncAndExclusionPredicate ( List
                                                                          SortFunc
                                                                          ExclusionPredicate )
  (let (
        ( MinimalElements nil )
        ( SortedElements ( sort
                           ( copy List )
                           SortFunc ) ) )
    (while SortedElements
      (let (
            ( MinimalElement  ( car SortedElements ) ) )
        ( setq MinimalElements ( tconc MinimalElements MinimalElement ) )
        ( setq
          SortedElements 
          ( setof 
            Element
            SortedElements
            ( and
              ( not ( equal Element MinimalElement ) )
              ( not ( apply ExclusionPredicate ( list MinimalElement Element ) ) ) ) ) ) ) )
    ( car MinimalElements ) ) )

(defun ListGreedilyFindMinimalElementsWithValueFuncAndExclusionPredicate ( List
                                                                           ValueFunc
                                                                           ExclusionPredicate )
  ( ListGreedilyFindMinimalElementsWithSortFuncAndExclusionPredicate 
    List
    (lambda ( x y ) 
      ( lessp
        ( apply ValueFunc ( list x ) )
        ( apply ValueFunc ( list y ) ) ) )
    ExclusionPredicate ) )


(defun ListMap ( Func List 
                      @key
                      ( PreFirst (lambda ( x ) x ) )
                      ( PostFirst (lambda ( x ) x ) )
                      ( PreLast (lambda ( x ) x ) )
                      ( PostLast (lambda ( x ) x ) )
                      )
  (let (
        ( First t )
        ( Last nil )
        ( Ret nil ) )
    (while List
      (let (
            ( Arg ( car List ) ) )
      ( setq 
        Ret
        ( tconc 
          Ret
          (cond (
                 First
                 ( apply 
                   PostFirst
                   ( list 
                     ( apply
                       Func 
                       ( list 
                         ( apply 
                           PreFirst
                           ( list Arg ) ) ) ) ) ) )
                (
                 Last
                 ( apply 
                   PostLast
                   ( list 
                     ( apply
                       Func 
                       ( list 
                         ( apply 
                           PreLast
                           ( list Arg ) ) ) ) ) ) )
                (
                 ( apply
                   Func 
                   ( list Arg ) ) ) ) ) )
          ( setq List ( cdr List ) )
          ( setq Last ( null ( cdr List ) ) ) )  )
    ( car Ret ) ) )

(defun ListGroup ( Size List )
  (let (
        ( Ret nil )
        ( Group nil )
        ( I 0 ) )
    (while List 
      ( setq Group ( tconc Group ( car List ) ) )
      ( setq List ( cdr List ) )
      (when ( or ( null List )
                 ( equal ( setq I ( mod ( plus I 1 ) Size ) ) 0 ) )
        ( setq Ret ( tconc Ret ( car Group ) ) )
        ( setq Group nil ) )
      )
    ( car Ret ) ) )

(defun ListFillList ( FillFunc N )
 (let (
       ( RetList nil ) )
   ( for
     I
     1
     N 
     ( setq RetList ( tconc RetList ( apply FillFunc ( list I ) ) ) ) )
   ( car RetList ) ) )

(defun ListRemoveLastNElements ( List N )
  (let (
        ( RetList ( reverse List ) ) )
    ( for
      I
      1
      N 
      ( setq RetList ( cdr RetList ) ) )
    ( reverse RetList ) ) )

(defun ListFindMean ( List ElementToValueFunc )
  ( quotient 
    ( ListFindSum List ElementToValueFunc )
    ( length List ) ) )

(defun ListFindSum ( List ElementToValueFunc )
  ( ListApplyFuncToListAndAccumulateResult 
    List         
    (lambda ( Element CurrentResult )
      (let (
            ( Value (if ElementToValueFunc
                        ( apply ElementToValueFunc ( list Element ) )
                      Element ) ) )
        ( plus Value CurrentResult ) ) )
    nil
    0.0 ) )
                      
(defun ListFindMinimum ( List ElementToValueFunc )
  ( cadr ( ListFindMinimumElementValuePair List ElementToValueFunc ) ) )

(defun ListFindMinimumElement ( List ElementToValueFunc )
  ( car ( ListFindMinimumElementValuePair List ElementToValueFunc ) ) )

(defun ListFindMinimumElementValuePair ( List ElementToValueFunc )
  ( ListApplyFuncToListAndAccumulateResult 
    List 
    ( lambda ( Element CurrentResult ) 
      (let (
            ( Value (if ElementToValueFunc
                        ( apply ElementToValueFunc ( list Element ) )
                      Element ) ) )
        (if ( or ( null ( cadr CurrentResult ) )
                 ( lessp Value ( cadr CurrentResult ) ) )
            ( list Element Value )
          CurrentResult ) ) )
    nil
    ( list nil nil ) ) )

(defun ListFindBestElement ( List CompareFunc Params )
  ( ListApplyFuncToListAndAccumulateResult 
    List
    (lambda ( Element CurrentResult )
      (if ( apply CompareFunc ( list Element CurrentResult ) ) 
          Element CurrentResult ) )
    Params
    ( car List ) ) )

(defun ListFindMaximum ( List ElementToValueFunc )
  ( cadr ( ListFindMaximumElementValuePair List ElementToValueFunc ) ) )

(defun ListFindMaximumElement ( List ElementToValueFunc )
  ( car ( ListFindMaximumElementValuePair List ElementToValueFunc ) ) )

(defun ListFindMaximumElementValuePair ( List ElementToValueFunc )
 ( ListApplyFuncToListAndAccumulateResult 
    List 
    (lambda ( Element CurrentResult ) 
      (let (
            ( Value (if ElementToValueFunc
                        ( apply ElementToValueFunc ( list Element ) )
                      Element ) ) )
        (if ( or ( null ( cadr CurrentResult ) )
                 ( geqp Value ( cadr CurrentResult ) ) )
            ( list Element Value )
          CurrentResult ) ) )
    nil
    ( list nil nil ) ) )

(defun ListFindIndex ( List Element )
  (let (
        ( Index 0 ) )
    (while ( and 
             ( car List )
             ( not ( equal Element ( car List ) ) ) )
      ( setq Index ( plus Index 1 ) )
      ( setq List ( cdr List ) ) )
    (if ( equal ( car List ) Element )
        Index
      nil ) ) )
                 
;Non-recursive function that finds an element in list and returns it.
;Calles the PredicateFunc for each element with element as the
;first parameter and the PredicateFuncParams as the subsequent parameters.
;If PredicateFunc returns t then the passed element is returned from this function.
;NOTE: A nil element cannot be returned.
(defun ListFindElement ( List PredicateFunc PredicateFuncParams ) 
  ( car
    ( exists
      Element
      List
      ( apply
	PredicateFunc
	( cons
	  Element
	  PredicateFuncParams ) ) ) ) )

(defun ListRemoveDuplicates ( List CompareFunc CompareFuncParams )
  (let (
	 ( Ret nil )
	 ( CurrList List ) )
    (while CurrList
      (let (
            ( CurrEle ( car CurrList ) ) )
	( setq CurrList ( cdr CurrList ) )
	(unless ( ListFindElement
		  ( car
		    Ret
		    )
		  (lambda
		    ( CurrFindEle EleToFind CompareFunc CompareFuncParams )
		    ( apply
		      CompareFunc
		      ( cons
			CurrFindEle
			( cons
			  EleToFind
			  CompareFuncParams ) ) ) )
		  ( list
		    CurrEle
		    CompareFunc
		    CompareFuncParams
		    )
		  )
	  ( setq
	    Ret
	    ( tconc
	      Ret
	      CurrEle ) ) ) ) )
    ( car
      Ret ) ) )

(defun ListRemoveDuplicatesNoTableElements ( List )
  (let (
	( RemoveDupTable ( makeTable "RemoveDuplicatesTable" nil ) )
	( Result nil ) )
    ( foreach
      Element
      List
      (unless ( arrayref
		RemoveDupTable
		Element )
        ( setq Result ( tconc Result Element ) )
        ( setarray RemoveDupTable Element t ) ) )    
    ( car Result ) ) )

(defun ListRemoveDuplicatesFromStringList ( StringList )
  ( ListRemoveDuplicatesNoTableElements
    StringList ) )

(defun ListDifference ( Set SubSet CompareFunc CompareFuncParams )
  ( ListApplyFuncToListAndAccumulateNonNilResults
    Set
    (lambda
      ( SetEle CompareFunc CompareFuncParams )
      (unless ( ListFindElement
		SubSet
		(lambda
		  ( SubSetEle SetEle CompareFunc CompareFuncParams )
		  ( apply
		    CompareFunc
		    ( cons
		      SubSetEle
		      ( cons
			SetEle
			CompareFuncParams ) ) ) )
		( list
		  SetEle
		  CompareFunc
		  CompareFuncParams ) )
	SetEle ) )
    ( list
      CompareFunc
      CompareFuncParams ) ) )

(defun ListSymmetricDifferenceNoTableElements ( SetA SetB )
  ( ListUnionNoTableElements
    ( ListDifferenceNoTableElements SetA SetB )
    ( ListDifferenceNoTableElements SetB SetA ) ) )

(defun ListDifferenceNoTableElements ( Set SubSet )
  (let (
	( SubSetTable ( makeTable "ListDifferenceSubSetTable" nil ) ) )
    ( foreach 
      Element
      SubSet
      ( setarray
	SubSetTable
	Element
	t ) )
    
    ( ListApplyFuncToListAndAccumulateNonNilResults
      Set
      (lambda
	( Element SubSetTable )
	(unless ( arrayref SubSetTable Element )
	  Element ) )
      ( list
	SubSetTable ) ) ) )
  
(defun ListStringListDifference ( StringSet StringSubSet )
  ( ListDifferenceNoTableElements StringSet StringSubSet ) )

(defun ListSameElementsNoTableElements ( List1 List2 )
   (let (
         ( ElementTable ( makeTable `element nil ) ) )
     ( foreach
       Element
       List1 
       ( setarray ElementTable Element t ) )
     ( and 
       ( equal ( length List1 ) ( length List2 ) )
       ( forall
         Element
         List2                  
         ( arrayref ElementTable Element ) ) ) ) )
	
;non number does nothing ( same as 0 )
;number n rotates so n is beginning of list	  
(defun ListRotate ( List Value )
  (let (
        ( StartEnd ( ListSplit List Value ) ) )
    ( append ( cadr StartEnd ) ( car StartEnd ) ) ) )
        
(defun ListSplit ( List Value )
 (cond (
         ( integerp Value )
         (let (
               ( StartList nil )
               ( EndList nil )
               ( CurrPos 0 ) )
           ( foreach
             Element
             List 
             (if ( lessp CurrPos Value )
                 ( setq StartList ( tconc StartList Element ) )
               ( setq EndList ( tconc EndList Element ) ) )
             ( setq CurrPos ( plus CurrPos 1 ) ) )
           ( list ( car StartList ) ( car EndList ) ) ) )
       ( 
        ( list nil List ) ) ) )


(defun ListSplitOnPredicate ( List Predicate )
  (when ( listp List )
    (let (
          ( ListOfLists nil )
          ( CurrList nil ) )
      (while List
        ( setq CurrList ( tconc CurrList ( car List ) ) )
        (when ( or
                ( null ( cdr List ) )
                ( apply Predicate ( list ( car List ) ( cadr List ) ) ) )
          ( setq ListOfLists ( tconc ListOfLists ( car CurrList ) ) )
          ( setq CurrList nil ) )
        ( setq List ( cdr List ) ) )
      ( car ListOfLists ) ) ) )


;n^2 so we can have table elements
(defun ListIntersect ( List1 List2 )
  ( setof 
    Element
    List1
    ( exists OtherElement List2 ( equal Element OtherElement ) ) ) )

;tables not allowed
(defun ListUniqNoTableElements ( List )
  ( ListUnionNoTableElements List List ) )

(defun ListUnionNoTableElements ( List1 List2 )
  (let (
        ( ElementTable ( makeTable `element nil ) ) )
    ( foreach 
      Element
      List1 
      ( setarray ElementTable Element t ) )
    ( foreach
      Element
      List2 
      ( setarray ElementTable Element t ) )
    ( mapcar ( lambda ( Pair ) ( car Pair ) )
             ( tableToList ElementTable ) ) ) )

(defun ListIntersectionNoTableElements ( List1 List2 )
  (let (
        ( ElementTable ( makeTable `element nil ) ) )
    ( foreach
      Element List1
      ( setarray ElementTable Element t ) )
    ( setof Element List2 ( arrayref ElementTable Element ) ) ) )

(defun ListNonDestructiveMapCan ( Func 
                                  @rest Args )
  (let (
        ( Result nil ) )
    ( foreach
      List
      ( apply `mapcar ( cons Func Args ) )
      ( setq Result ( append List Result ) ) )
    Result ) )

;FuncInfoList is a list of pairs ( function, arglist )...returns non-nil if all return non-nil
;You can, of course, use a lambda function instead
(defun ListAndFuncs ( FuncInfoList )
  ( forall
    FuncInfo
    FuncInfoList
    ( apply
      ( car FuncInfo )
      ( cadr FuncInfo ) ) ) )

  
;all subsets of Set of size N
(defun ListGetSubsets ( Set N )
  (let (
        ( Vector ( listToVector Set ) ) )
    ( mapcar
      (lambda ( SubsetOfZ )
        ( mapcar 
          (lambda ( K )
            ( arrayref Vector ( difference K 1 ) ) )
          SubsetOfZ ) )
      ( ListSubsetsOfZ
        ( length Set )
        K ) ) ) )

(defun ListSubsetsOfZ ( N K )
  ( ListSubsetsOfZInner N K nil nil ) )

(defun ListSubsetsOfZInner ( N K S T )
  (cond (
         ( equal K 0 )
         ( setq T ( cons S T ) ) )
        (
         t
         (when ( lessp K N )
           ( setq T 
                  ( ListSubsetsOfZInner
                    ( difference N 1 )
                    K
                    S
                    T ) ) )
         ( setq T
                ( ListSubsetsOfZInner  
                  ( difference N 1 )
                  ( difference K 1 )
                  ( cons N S )
                  T
                  ) ) ) )
  T )


;removes any repertive successive elements
(defun ListUniq ( List )
  (let (
        ( Ret ( tconc nil ( car List ) ) )
        ( LastElement ( car List ) ) )
    ( foreach
      Element 
      ( cdr List )
      (if ( not ( equal Element LastElement ) )
          ( setq Ret ( tconc Ret Element ) ) )
      ( setq LastElement Element ) )
    ( car Ret ) ) )


;returns a pair, ( passing,  failing )
(defun ListFilter ( List
                    Predicate
                    PredicateArgs )
  (let (
        ( PassList nil  )
        ( FailList nil ) )
    ( foreach
      Element
      List
      (if ( apply Predicate ( cons Element PredicateArgs ) )
          ( setq PassList ( tconc PassList Element) )
        ( setq FailList ( tconc FailList Element) ) ) )
    ( list ( car PassList ) ( car FailList ) ) ) )

(defun ListFilterByIntersection ( List1
                                  List2 )
  (let (
        ( ElementTable ( makeTable `element nil ) ) )
    ( foreach
      Element
      List2
      ( setarray ElementTable Element t ) )
    ( ListFilter 
      List1
      (lambda ( x ) ( arrayref ElementTable x ) )
      nil ) ) )

(defun ListFindElementsInList ( ListToFindElementsIn
                                ElementsToFind
                                CompareFunc
                                CompareFuncParams )
  (let (
        ( Ret nil )
        ( CurrElementsToFind ElementsToFind ) )
    ( foreach
      Element
      ListToFindElementsIn
      (let (
            ( CopyOfCurrElementsToFind CurrElementsToFind )
            ( NewListOfElementsToFind nil ) )
        (while CopyOfCurrElementsToFind
          (let (
                ( CurrElementToFind ( car CopyOfCurrElementsToFind ) ) )
            ( setq CopyOfCurrElementsToFind ( cdr CopyOfCurrElementsToFind ) )
            (if ( apply
                  CompareFunc
                  ( cons
                    Element
                    ( cons
                      CurrElementToFind
                      CompareFuncParams ) ) )
                ( setq Ret ( cons ( list Element CurrElementToFind ) Ret ) )
              ( setq
                NewListOfElementsToFind
                ( cons CurrElementToFind NewListOfElementsToFind ) ) ) ) )
            ( setq CurrElementsToFind NewListOfElementsToFind ) ) )
    Ret ) )

(defun ListFlatten ( List )
  (let (
        ( Ret nil )
        ( Path nil )
        )
    (while ( or List Path )
      (cond (
             ( null List )
             ( setq List ( car Path ) )
             ( setq Path ( cdr Path ) )
             )
            (
             t
             ( setq Element ( car List ) )
             ( setq List ( cdr List ) )
             (cond (
                    
                    ( listp Element )
                    ( setq Path ( cons List Path ) )
                    ( setq List Element ) )
                   (
                    t
                    ( setq Ret ( tconc Ret Element ) )
                    ) ) ) ) )
    ( car Ret )
    ) )

(defun ListAppend ( @rest Lists )
  (let (
        ( Ret nil ) )
  ( foreach List ( reverse Lists )
            ( setq Ret ( append List Ret ) ) )
  Ret
) )


(defun IsInList ( Obj List )
  (member Obj List)!=nil
  )

(defun RemoveFromList ( Obj List )
 (let (newlist)
  (foreach item List
    (when Obj!=item 
      newlist = (cons item newlist)
    )
  )
  newlist
 )
)

