
(dis "*********************************************************************" dnl)
(dis "********************                             ********************" dnl)
(dis "********************   dir2.scm   JOB STARTING   ********************" dnl)
(dis "********************                             ********************" dnl)
(dis "*********************************************************************" dnl)


(define canonicalizer  
  (new-modula-object 'Circuit.Canonicalizer 
                     `(canon . ,(lambda(self n)(canonicalize n)))))

(dis "====================  BUILDING PRS  ====================" dnl)
(define prs 
  (Circuit.Build dsim-types 
                 (get-dsim-type (cadddr routed-super)) 
                 full-instance-name 
                 tm 
                 modula-global-set 
                 modula-ignore-set
                 canonicalizer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; note prs is built BEFORE the timing model

;; get all nodes and their aliases

(define debug-all-typed-prefixes '())

(define (rebuild-timing-model)
  (dis "====================  REBUILDING TIMING MODEL  ====================" dnl)

  (dis "enumerating nodes..." dnl)
  (define all-nodes (enumerate-set 'NameSet
                                   (Circuit.ScanForNames  
                                    (modula-type-op 'PRS.T 
                                                    'get-field prs 'rules))))

  (dis "mapping aliases ..." dnl)
  (define all-aliases 
    (map (lambda(cn)
           (begin (dis cn dnl)
                  (if (member? cn globals)
                      (list cn)
                      (all-node-aliases cn))))
         (map Name.Format all-nodes)))


  (dis "extracting prefixes ..." dnl)
  (define all-my-prefixes
    (uniq string=? (cons "" (apply append (map all-prefixes (apply append all-aliases))))))

  (define all-my-typed-prefixes
    (filter car (map (lambda(pfx)(cons (get-instance-type pfx) pfx)) all-my-prefixes)))

	(set! debug-all-typed-prefixes all-my-typed-prefixes)

  (dis "searching for directives ..." dnl)
  (define (attempt-directives-parse typed-prefix)
    (let* ((paths (lve-pns (car typed-prefix)))
           (rds (filter (lambda(x) x)(map MetaUtils.FileRdOpenOrFalse paths))))
      (map (lambda(rd)
             (dis "parsing directives for " (stringify typed-prefix) dnl)
             (Directives.Parse rd 
                               modula-global-set
                               (Name.ParseText (cdr typed-prefix))
                               directives-tbl)
             (Rd.Close rd))
           rds)))

  (map attempt-directives-parse all-my-typed-prefixes)  

  ;; actually build timing model

  (define alias-tbl
    (map (lambda(x)(map Name.ParseText x)) all-aliases))

  (begin
    (dis "Assembling timing model..." dnl)
    (modula-type-op 'Circuit.CktTimingModel
                    'call-method
                    tm
                    'init
                    (list directives-tbl 
                          (MetaUtils.ListListToAliasTbl 
                           alias-tbl))))
  (dis "====================  TIMING MODEL COMPLETE  ====================" dnl)
  #t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; close environment
(load "close-env.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-epoch (ArithP.NewPair (ArithR.TheEpoch) 
                                      (ArithR.NewConstant default-slew)))

(define start-time    (ArithP.NewPair (ArithR.NewLiteral 't0)
                                      (ArithR.NewConstant default-slew)))
  

(dis "====================  CLOSING ENVIRONMENT  ====================" dnl)

(close-environment)

(rebuild-timing-model)


(define (reset)
  (PRSimulate.Set prs (Name.ParseText "_RESET") #f default-epoch)
  (PRSimulate.Cycle prs (Scenario.Forever) *min-mult*)
)

(define scenarios (make-scenarios))

(define (go)
  (PRSimulate.Set prs (Name.ParseText "_RESET") #t start-time)
  (PRSimulate.Cycle prs scenarios *min-mult*)
)


