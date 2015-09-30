;;
;; $Id: set.scm,v 1.1 2009/04/14 07:52:06 mika Exp $
;;

(require-modules "hashtable")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-set make-hash-table)
  (let ((hashtable (make-hash-table))
        (res '()))
    (set! res 
          (lambda (message . args)
            (case message
              ((insert!)  (let ((result (res 'member? (car args))))
                            (if (not result)
                                (hashtable 'add-entry! (car args)))
                            result))

              ((member?) (not (eq? (hashtable 'retrieve (car args))
                                   '*hash-table-search-failed*)))
              ((delete!)  
               (let ((result (res 'member? (car args))))
                 (hashtable 'delete-entry! (car args))
                 result))

              ((size)     (length (hashtable 'keys)))
              ((keys clear! display)     (hashtable message))
              (else (error "Unknown message " message))
              )))
    res))

(define (make-string-set size)
	(make-set (lambda() (make-string-hash-table size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-symbol-hash-table size)
  (define (symbol-hash s) 
    (modulo (accumulate + 
												0 
												(map char->integer 
														 (string->list (symbol->string s)))) 
						size))

  (make-hash-table size symbol-hash))


(define (make-symbol-set size)
	(make-set (lambda() (make-symbol-hash-table size))))

