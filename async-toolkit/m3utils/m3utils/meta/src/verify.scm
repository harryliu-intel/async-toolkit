;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Timing verification of metastable elements using pathalyze.
;; This code has hopefully been superseded by the adjoint optimizer
;; code (if that is the right expression).
;;
;; Copyright 2011 Fulcrum Microsystems.  All rights reserved.
;; Author: Mika Nystrom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (v) (load "verify.scm"))

(define (make-verification-task primitive-type slow-path fast-path start-dir)
  ;; (make-verification-task 
  ;;   "lib.metastable.primitive.probe.PRIMITIVE_PROBE_WAIT_CORE"
  ;;   '("Q.e" . "Q.0")  ;; slow path is from Q.e to Q.0
  ;;   '("L.e" . "L.0")  ;; fast path is from L.e to L.0
  ;; )
  ;; request verification that slow path is slower than fast path in
  ;; all instances
  `((primitive-type . ,primitive-type)
    (slow-path      . ,slow-path)
    (fast-path      . ,fast-path)
    (start-dir      . ,start-dir)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-spec typename prefix start-dir slow-path fast-path . p)
  
  (dis "(print-spec " (stringify typename) " "
                      (stringify prefix) " "
                      (stringify start-dir) " "
                      (stringify slow-path) " "
                      (stringify fast-path) ") " dnl)


  (let ((q (if (null? p) p (car p)))
        (c (case start-dir ((Up) "+") ((Down) "-") (else (error)))))

  (define (dis-list lst dir first)
    (if (null? lst) 
        (dis ";" dnl q)
        (begin (dis dnl " "
                    (if first "(" "")
                    prefix (car lst) 
                    (if first ")" "")
                    dir q) 
               (dis-list (cdr lst)
                         (cond ((string=? dir "+") "-")
                               ((string=? dir "-") "+")
                               (else (error))) #f))))

    (dis "cell " typename ";"                                           dnl q)
    (dis                                                                dnl q)
    (dis "path long ="                                                      q)
    (dis-list slow-path c #t)
    (dis                                                                dnl q)
    (dis "path short ="                                                      q)
    (dis-list fast-path c #t)
    (dis                                                                dnl q)
    (dis "param margin = 0.9*min(long) - (max(short) + max_slew(short));" dnl q)
   )
  #t
)

(define (print-script typename target-dir-name . p)
  (let ((q (if (null? p) p (car p))))
    (dis "#!/bin/sh" dnl q)
    (dis "cd /home/user/mnystrom/meta/meta/scripts" dnl q)
    (dis "echo " target-dir-name dnl q)
    (dis "date" dnl q)
    (dis ". ../src/settings.sh" dnl q)

;; want this here but illegal:
;; --define=chip.alta.port.attribute.PortAttributes.allowPhantom:true 

    (dis "fulcrum pathalyze --max-heap-size=${JAVAMEM} --corner=tt --voltage=0.9 --temp=125 --cast-dir=/mnt/fulcrum/alta/mnystrom/p4/hw-main/cast --spec-dir=/mnt/fulcrum/alta/mnystrom/p4/hw-alta/layout/tsmc65/spec --lve-dir=/mnt/fulcrum/alta/lve/lve --output-dir=/home/user/mnystrom/meta/meta/output/tt/0.9V/125C/"target-dir-name" --routed --root-subtype='"typename"' "target-dir-name".spec" dnl q)
    (dis "date" dnl q)
    )
  #t
  )
    
(define (print-error typename err . p)
  (let ((q (if (null? p) p (car p))))
    (dis "ERROR "typename " " err dnl q)))

(define (verify-task-instance task instance)

  (dis "verify-task-instance: " instance dnl)
  (define (make-global-name name) (string-append instance "." name))

  (define (path-fanin path) 
    (map canonical-name
         (find-fanin-path (make-global-name (cdr path))
                          (make-global-name (car path)))))

  (let* (  (slow-path       (cdr (assoc 'slow-path task)))
           (fast-path       (cdr (assoc 'fast-path task)))
           (start-dir       (cdr (assoc 'start-dir task)))
           (slow-path-nodes (path-fanin slow-path))
           (fast-path-nodes (path-fanin fast-path))  
           (super           (common-supercell 
                             (append slow-path-nodes fast-path-nodes)))

           (slow-in-super   
						(if (equal? super "") 
								slow-path-nodes
								(elim-prefix (string-append super ".") slow-path-nodes)))

           (fast-in-super   
						(if (equal? super "") 
								fast-path-nodes
								(elim-prefix (string-append super ".") fast-path-nodes)))

           (super-type      (get-instance-type super))
           (all-supers      (get-ascending-parent-types instance))

;; the below is unnecessary when we are going through every instance
;; to begin with! (it was coded before we considered the wiring outside
;; the cell...)
;;           (routed-supers   (iterate-till-all-routed 
;;
;;                ;; in the real code we seed with routed-type?
;;                ;; but we have to test with #f as well, to get super-super
;;                ;; and test that we get the correct naming in that case.
;;                              `((,(routed-type? super-type) ,super-type)))
;;                             ;; `((#f ,super-type)))
           (routed-super    (routed-super-instance (list super)))
           (routed-supers   (list (cons (not (not routed-super))
                                        (cons 
                                         (get-instance-type (car routed-super))
                                         (cadr routed-super)))))
                            
           )



    `(;;(slow ,slow-path-nodes) 
      ;;(fast  ,fast-path-nodes) 
      (super ,super)
      (slow-in-super . ,slow-in-super)
      (fast-in-super . ,fast-in-super)
      (super-type    ,super-type)
      (routed-supers ,routed-supers)
      (start-dir     . ,start-dir)
      (all-supers    . ,all-supers)
      )
    
    
))                    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-task-instances task)
  (let ((instances (get-all-matching-instances 
                    (cdr (assoc 'primitive-type task)))))
    instances))

(define a-task 
  (make-verification-task  
   "lib.metastable.primitive.probe.PRIMITIVE_PROBE_WAIT_CORE" 
   '("L.e" . "Q.0") 
   '("L.e" . "L.0") 
   'Up
   )
)

(define da0A-task
  (make-verification-task  
   "lib.metastable.primitive.dual_arbiter.PRIMITIVE_DUAL_ARBITER_CORE" 
   '("_A.2" . "x.1")     ;; slow path
   '("_A.2" . "L[0].0")  ;; fast path
   'Up
   )
)

(define da1A-task
  (make-verification-task  
   "lib.metastable.primitive.dual_arbiter.PRIMITIVE_DUAL_ARBITER_CORE" 
   '("_A.2" . "x.0")     ;; slow path
   '("_A.2" . "L[1].0")  ;; fast path
   'Up
   )
)

(define da0x-task
  (make-verification-task  
   "lib.metastable.primitive.dual_arbiter.PRIMITIVE_DUAL_ARBITER_CORE" 
   '("xv" . "x.1")     ;; slow path
   '("xv" . "L[0].0")  ;; fast path
   'Down
   )
)

(define da1x-task
  (make-verification-task  
   "lib.metastable.primitive.dual_arbiter.PRIMITIVE_DUAL_ARBITER_CORE" 
   '("xv" . "x.0")     ;; slow path
   '("xv" . "L[1].0")  ;; fast path
   'Up
   )
)

;; following just for testing some search algorithms
(define f-task  
  (make-verification-task  
   "lib.metastable.primitive.filter.FILTER"
   '("_D.0" . "D.0") 
   '("_D.1" . "D.1") 
   'Up
   )
)

(define all-real-tasks (list a-task da0A-task da1A-task da0x-task da1x-task))

;;(define a-task-instance (car (get-task-instances a-task))) 

(define (f-task-instance n) (nth (get-task-instances f-task) n)) 

(define (filter-out-tag tag lst)
  (filter (lambda(x) (not (eq? (car x) tag))) lst))

(define (cartesian-tag-product old-tag new-tag res)
  ;; take an instance result and multiply it out
  (let ((old-tag-values (cadr (assoc old-tag res)))
        (non-match (filter-out-tag old-tag res)))
    
    (map (lambda(x)(cons (cons new-tag x) non-match))
         old-tag-values)))

(define debug-without-paths '())
(define debug-supers '())
(define debug-super '())
(define debug-res '())

(define (canonicalize-instance-result res)
  (set! debug-res res)
  (let ((all-supers (cdr (assoc 'all-supers res)))
        (without-paths  (cartesian-tag-product 
                        'routed-supers 
                        'routed-super
                        (filter-out-tag 'all-supers
                          (filter-out-tag 'super-type 
                            (filter-out-tag 'super res))))))

    (set! debug-without-paths without-paths)

    (let* (( supers (map (lambda(wp)(caddr (assoc 'routed-super wp))) 
                        without-paths) )

           ( ascending-lists
             (map (lambda(super)
                    (set! debug-supers supers)
                    (set! debug-super  super)
                    (truncate-list 
                     (lambda(x)(equal? (car x) super)) all-supers)) 
                  supers) )
           )
      (map (lambda(s wp)(cons (cons 'all-supers s) wp)) 
           ascending-lists
           without-paths))))
      

(define (in-range? n range)
  (and (>= n (car range)) (<= n (cadr range))))

(define (byte-in-hex byte)
  (let ((s (number->string byte 16)))
    (cond ((= (string-length s) 1) (string-append "0" s))
          ((= (string-length s) 2) s)
          (else (error s)))))

(define (convert-to-filename str)
  (let* ((ok-chars '(("a"  "z") 
                     ("A"  "Z") 
                     ("0"  "9") 
                     ("_"  "_") 
                     ("."  ".")))
         (ok-ranges (map (lambda(cr)
                           (map char->integer
                                (map car 
                                     (map string->list cr))))
                         ok-chars))
         (start-chars (string->list str)))
    (define (do-one-char c)
      (let ((ascii (char->integer c)))
        (if (mem? in-range? ascii ok-ranges) 
            c 
            (string-append "%" 
                           (let ((s (number->string ascii 16)))
                             (cond ((= (string-length s) 1) (string-append "0" s))
                                   ((= (string-length s) 2) s)
                                   (else (error s))))))))
                                 

    (apply string-append (map do-one-char start-chars))))

(define (filter-out-chars chars str)
  (list->string
   (filter (lambda(c)(not (member? c chars))) (string->list str))))


(define (make-output-filename instance-result)
  (let ((s (caddr (assoc 'routed-super instance-result)))
        (no-s (map cdr (filter-out-tag 'routed-super instance-result))))
    (string-append (convert-to-filename s) 
                   "="
                   (convert-to-filename 
                    (filter-out-chars '(#\( #\) #\") 
                                      (stringify no-s))))))

;; version using SRC fingerprint

(define (make-output-filename instance-result)
  (let ((s (caddr (assoc 'routed-super instance-result)))
        (fp (Fingerprint.FromText (stringify instance-result))))

    (string-append (convert-to-filename s) 
                   "."
                   (apply string-append
                          (map byte-in-hex 
                               (map cdr (cdr (assoc 'byte fp))))))))


(define (write-results instance-result)
  (let* ((verify-type (caddr (assoc 'routed-super instance-result)))
         (fn  (make-output-filename instance-result))
         (base-fn (string-append "/home/user/mnystrom/meta/meta/scripts/"
                                 fn)))
           
    (if (not (cadr (assoc 'routed-super instance-result)))
        (let*  ((error-fn  (string-append base-fn ".ERROR"))
                (error-wr  (filewr-open error-fn)))
          (print-error verify-type 
                       (string-append "NO ROUTING FOR CELL: " 
                                      (stringify instance-result)) 
                       error-wr))
        (let*
            ((script-fn     (string-append base-fn ".script"))
             (spec-fn       (string-append base-fn ".spec"))
             (meta-fn       (string-append base-fn ".meta"))
             (script-wr     (filewr-open script-fn))
             (spec-wr       (filewr-open spec-fn))
             (meta-wr       (filewr-open meta-fn))
             (start-dir     (cdr (assoc 'start-dir     instance-result)))
             (slow-in-super (cdr (assoc 'slow-in-super instance-result)))
             (fast-in-super (cdr (assoc 'fast-in-super instance-result)))
             (prefix        
              (let ((p  (cdddr (assoc 'routed-super instance-result))))
                (if (null? p) "" (string-append p "."))))
             )

          (print-script verify-type 
                        fn
                        script-wr)
          (wr-close script-wr)

          (print-spec verify-type 
                      prefix 
                      start-dir 
                      slow-in-super 
                      fast-in-super 
                      spec-wr)
          (wr-close spec-wr)
          
          (print-meta instance-result 
                      meta-wr)
           
          (wr-close meta-wr)
          ))))

(define (print-meta canon-result . p)
  (let ((q (if (null? p) p (car p))))
    (dis "routed-container" dnl 
         (stringify (caddr (assoc 'routed-super canon-result))) dnl dnl q)

    (let ((all-supers (cdr (assoc 'all-supers canon-result))))
      (dis "relative-instance-name " dnl
           (stringify (cdar (tail 1 all-supers))) dnl dnl q)
      (dis "type-hierarchy" dnl
           (stringify (map car all-supers)) dnl q))

)
#t)


(define (verify-instances-results task instances)
	(dis "instances " (stringify instances) dnl dnl)
  (let* ((i-rs      (map (lambda(i)(verify-task-instance task i)) instances))
         (c-i-rs    (map canonicalize-instance-result i-rs))
         
         (bag (apply append c-i-rs))
         (set (uniq equal? bag)))

    (dis "i-rs "      (stringify i-rs)      dnl dnl)
    (dis "c-i-rs "    (stringify c-i-rs) dnl dnl)

    set))

(define (verify-instance-results task)
  (dis "TASK " (cdr (assoc 'primitive-type task)) dnl)
  (let* ((instances (get-task-instances task)))
    (verify-instances-results task instances)))

(define (verify-all task)
  (map write-results (verify-instance-results task)))

(define (get-type-or-parent-type instance)
  (car (iter-parent-type (list instance))))

(define (get-ascending-parent-types instance)
	(reverse (cons (cons (Name.Format top-name) instance)
				(reverse
				(let ((prefixes (all-prefixes instance)))
					(filter car ;; get rid of names that have no type (i.e. dot in component)
									(reverse (map cons (map get-instance-type prefixes) 
																(map cond-strip-leading-dot 
																		 (map (lambda(p)(elim-1-prefix p instance)) prefixes))
																))))))))

(define (cond-strip-leading-dot str)
  (let ((lst (string->list str)))
    (if (null? lst) 
        ""
        (list->string
         (if (equal? (car lst) #\.) (cdr lst) lst)
))))

(define (truncate-list here? lst)
  (cond ((null?    lst)          (error "not found " here?))
        ((here?   (car lst))      (list (car lst)))
        (else (cons (car lst) (truncate-list here? (cdr lst))))))

(define pw-core-types 
  (matching-instantiated-types 
   "lib.metastable.primitive.probe.PRIMITIVE_PROBE_WAIT_CORE"))

(define (get-type-type-parents types)
  (uniq string=? (apply append (map get-all-parent-types types))))