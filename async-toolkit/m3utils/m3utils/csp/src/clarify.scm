; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (replace-array-sizes of with)
  ;; given an array type (possibly iterated) of
  ;; replace its extents with the extents of with
  ;; but leave the base type untouched

  (if (array-type? of)
      (make-array-type (array-extent with)
                       (replace-array-sizes (array-elemtype of)
                                            (array-elemtype with)))
      of
      )
  )


(define (clarify-type formal-type actual-type actual)
  ;;
  ;; this computes the type of the copy-in object for a function parameter
  ;; for arrays, it takes the element type from the function formal
  ;; but the extent from the actual
  ;;
  ;; haven't done it for structs yet. (and combinations of structs and arrays)
  ;;

  (if (literal? actual)
      formal-type  ;; if the actual is a literal, just return the formal type
  
      (case (car actual)

        ((id)
         (dis "clarify-type reached an id " actual dnl
              "clarify-type actual-type   " actual-type dnl
              "clarify-type formal-type   " formal-type dnl)
         
         (cond ((array-type? formal-type)
                (replace-array-sizes formal-type actual-type)
                formal-type)

               (else formal-type)))
        
        ((array-access)
         (if (not eq? 'array (car actual-type))
             (error "array access of not an array : " actual " of " actual-type)
             (clarify-type formal-type
                           (caddr actual-type)
                           (cadr actual))))
        
        (else (error "formal-type " formal-type " not compatible with actual " actual

                     )))))

