; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun TableCopy ( Table )
  (let (
        ( NewTable ( makeTable `bla ( arrayref Table `unbound ) ) ) )
    ( foreach Key Table 
              ( setarray NewTable Key ( arrayref Table Key ) ) )
    NewTable ) )

(defun TableCreateFromPairs ( Pairs )
  (let (
        ( Table ( makeTable `foo nil ) ) )
    ( foreach Pair Pairs
              ( setarray Table ( car Pair ) ( cadr Pair ) ) )
    Table ) )

(defun TableGetKeyList ( Table )
  ( mapcar `car ( tableToList Table ) ) )

(defun TableGetValueList ( Table )
  ( mapcar `cadr ( tableToList Table ) ) )

(defun TableGetEntryList ( Table )
  ( tableToList Table ) )

;Gets all elements x in SourceList such that there is an element y in CorrespondingList such that
; ( SourceKeyFunc x ) = ( CorrespondingKeyFunc y )
(defun TableGetCorrespondingList ( SourceList CorrespondingList SourceKeyFunc CorrespondingKeyFunc )
  (let (
        ( SourceTable ( makeTable `src nil ) ) )
    ( foreach Element SourceList
              ( setarray SourceTable ( apply SourceKeyFunc ( list Element ) ) Element ) )
    ( mapcar 
      ( lambda ( CorrespondingElement )
        ( arrayref SourceTable ( apply CorrespondingKeyFunc ( list CorrespondingElement ) ) ) )
      CorrespondingList ) ) )
       
