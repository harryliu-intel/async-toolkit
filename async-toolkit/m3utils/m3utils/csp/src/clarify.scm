(define (replace-array-sizes of with)
  ;; given an array type (possibly iterated) of
  ;; replace its extents with the extents of with
  ;; but leave the base type untouched

  (if (array-type? of)
      (make-array-type (get-array-extent with)
                       (replace-array-sizes (get-array-elem-type of)
                                            (get-array-elem-type with)))
      of
      )
  )


(define (clarify-type formal-type actual-type designator)
  ;;
  ;; this computes the type of the copy-in object for a function parameter
  ;; for arrays, it takes the element type from the function formal
  ;; but the extent from the actual
  ;;
  ;; haven't done it for structs yet. (and combinations of structs and arrays)
  ;;
  (case (car designator)

    ((id)
     (dis "clarify-type reached an id " designator dnl
          "clarify-type actual-type   " actual-type dnl
          "clarify-type formal-type   " formal-type dnl)

     (if (array-type? formal-type)
         (replace-array-sizes formal-type actual-type)
         formal-type))

    ((array-access)
     (if (not eq? 'array (car actual-type))
         (error "array access of not an array : " designator " of " actual-type)
         (clarify-type formal-type
                       (caddr actual-type)
                       (cadr designator))))

    (else (error "formal-type " formal-type " not compatible with designator " designator

           ))))

