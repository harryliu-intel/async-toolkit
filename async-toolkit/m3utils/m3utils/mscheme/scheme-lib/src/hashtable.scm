;; $Id$ 
;; 
;; Scheme hash tables
;;
;; from John David Stone <stone@math.grin.edu>
;;
;; http://www.cs.grinnell.edu/~stone/events/scheme-workshop/hash-tables.html
;; 

(require-modules "basic-defs")

(define make-hash-table
  (lambda (size hash-function)
    (let ((table (make-vector size '())))
      (lambda (message . args)
        (case message

          ((add-entry!)
           (let* ((key (car args))
                  (index (hash-function key)))
             (vector-set! table
                          index
                          (cons (cons key (cadr args))
                                (vector-ref table index)))))

          ((retrieve)
           (let* ((key (car args))
                  (index (hash-function key)))
             (cond ( (assoc key (vector-ref table index))  (cdr (assoc key (vector-ref table index))))
                   (else '*hash-table-search-failed*))))

          ((delete-entry!)
           (let* ((key (car args))
                  (index (hash-function key)))
             (let loop ((bucket (vector-ref table index))
                        (so-far '()))
               (cond ((null? bucket)
                      (vector-set! table index (reverse so-far)))
                     ((equal? key (caar bucket))
                      (loop (cdr bucket) so-far))
                     (else
                      (loop (cdr bucket)
                            (cons (car bucket) so-far)))))))

          ((display)
           (do ((index 0 (+ index 1)))
               ((= index size))
             (let ((bucket (vector-ref table index)))
               (if (not (null? bucket))
                   (begin
                     (display "Bucket #")
                     (display index)
                     (display ": ")
                     (display bucket)
                     (newline))))))

          ((clear!)
           (do ((index 0 (+ index 1)))
               ((= index size))
             (vector-set! table index '()))))))))

(define (make-string-hash-table size)
	(define (string-hash s) 
		(modulo (accumulate + 0 (map char->integer (string->list s))) size))

	(make-hash-table size string-hash))