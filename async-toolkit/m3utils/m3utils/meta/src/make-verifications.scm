(load "meta.scm")
(load "verify.scm")
(load "dir2.scm")

(define prim-task
  (make-verification-task
   "lib.metastable.primitive.dual_arbiter.PRIMITIVE_DUAL_ARBITER" 
   
   ;; in this context the fanin path is just used to push the routed cell
   ;; up so we are sure we are looking in the correct cell definition
   ;; for delays

   '("L[0].e" . "L[0].0") 
   '("L[1].e" . "L[1].0")
   'Up
   )
)
   
(define todo (verify-instance-results prim-task))

(define debug-task-instance '())

(define (map-to-er-verification task-instance)
  (set! debug-task-instance task-instance)
  (let* ((ip         (car (tail 1 (assoc 'all-supers task-instance))))
         (my-type    (car (cadr   (assoc 'all-supers task-instance))))
         (a-top-name (car    (get-type-instances (car ip)))))
  `(,(car ip)
    ,a-top-name
    ,(cdr ip)
    ,my-type)))

(define (write-verification-scm task-instance verification-name port)
  (dis (stringify `(define routed-super ',(map-to-er-verification task-instance))) dnl port)
  (dis `(define the-verification ',(eval verification-name)) dnl port)
#t)

(define (write-verification-scr task-instance fn-base p)
  (dis "#!/bin/sh" dnl p)
  (dis "cd /home/user/mnystrom/meta/meta/src" dnl p)
  (dis "echo " fn-base dnl p)
  (dis "date" dnl p)
  (dis "hostname" dnl p)
  (dis "/home/user/mnystrom/meta/meta/AMD64_LINUX/meta -dir " top-dir " -top " (escape-for-unix (Name.Format top-name)) " -dsim -routed -defscm problem-definition-file " (escape-for-unix (string-append "\"" fn-base ".scm\"")) " go.scm exit" dnl p)
)


(define (process-verification task-instance verification-name)
  (let* ((v (eval verification-name))
         (fp (Fingerprint.FromText (stringify (cons task-instance v))))
         (ip         (car (tail 1 (assoc 'all-supers task-instance))))
         (fn-base (string-append 
                   (convert-to-filename (car ip)) "."
                   (apply string-append
                          (map byte-in-hex 
                               (map cdr (cdr (assoc 'byte fp)))))))
         (scm-fn  (string-append fn-base ".scm"))
         (scr-fn  (string-append fn-base ".script")))
    
    (dis "writing " scm-fn "..." dnl)
    (let ((scm-wr (filewr-open (string-append "../scripts/" scm-fn))))
      (write-verification-scm task-instance verification-name scm-wr)
      (wr-close scm-wr)
      )

    (dis "writing " scr-fn "..." dnl)
    (let ((scr-wr (filewr-open (string-append "../scripts/" scr-fn))))
      (write-verification-scr task-instance fn-base scr-wr)
      (wr-close scr-wr)
      )
))


(load "dual-arbiter-verifications.scm")

    
(define (process-all-verifications)
  (map 
   (lambda(vt)
     (process-verification vt 'x0a2-verification-0)
     (process-verification vt 'x0a2-verification-1)
     (process-verification vt 'x0a0-verification-0)
     (process-verification vt 'x1a1-verification-0)
     )
   todo))
                   