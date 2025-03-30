;;
;; cspc.scm
;;
;; CSP "compiler" (still in quotes for now)
;;
;; Author : Mika Nystrom <mika.nystroem@intel.com>
;; March, 2025
;;
;; General strategy: Java has been enhanced to emit scheme-compatible
;; S-expressions.  We parse the S-expressions, convert to the Modula-3
;; objects, convert back to Scheme (the back and forth is mostly a
;; consistency check of our own code.
;;
;; The steps performed are:
;;
;; 1. desugaring
;;    a. x++ and x += c are desugared into x = x + 1 and x = x + c
;;    b. int x=5, y; are desugared to int x; x = 5; int y;
;;
;;
;;    uniquifying block variable names
;;
;;
;; 2. expression sequencing
;;    -- expression sequences need to be constructed for all expressions
;;    -- result should be three-address code (effectively)
;;
;; 3. function inlining
;;    -- functions need to be inlined per Verilog copy-in/copy-out semantics
;; 
;; (repeat steps 2 and 3 until fixed point -- or sequence functions first)
;;
;; 4. type evaluation
;;    -- the type of every three-address operation to be elucidated
;;       (both input and output)
;;
;; 5. block extraction
;;    -- sequential code blocks between suspension points to be computed
;;
;; 6. liveness computation
;;    -- liveness of temporaries to be computed: local to block or
;;       visible across blocks?
;;
;; 7. code generation
;;
;;
;; PROCESS SUSPENSION AND REACTIVATION
;; ===================================
;;
;; A basic design element is how to deal with suspended processes.
;; A process can suspend in three or four ways:
;;
;; i  - on an if statement (including a "wait" [X] = [X -> skip])
;; 
;; ii - on a receive X?_
;;
;; iii- partial suspension on re-joining after a comma A,B;C
;;
;; iv - sleeping for a "wait(n)" statement (not really suspension)
;;
;; The most interesting and tricky is case i- an if statement.  An
;; if can only be suspended on probes, since there are no global
;; variables shared between processes (other than point-to-point channels).
;; (This is a fundamental design feature of CSP.)
;;
;; Since only probes can cause a suspended if to wake up, we can
;; evaluate an if by thinking of it as a fork in the code that, based on
;; the result of evaluating all the guards, picks one branch and executes
;; that or else suspends waiting for another update.  The if is thus
;; registered as the waiter on all the probes that appear in its guards.
;;
;; We can draw the further conclusion from this discussion that an if
;; statement without probes in the guards can be implemented without
;; any suspend/release actions (as a simple if...then...else
;; in the manner of Pascal or C).
;;
;; What remains is to discuss the implementation of "else".  "else" is
;; an addition to CSP made by Fulcrum.  There are three ways of considering
;; implementing else:
;;
;; i   - we could implement else simply as "true" and evaluate the guards
;;       always in textual sequence.  The problem with this is that we
;;       would find it difficult to detect whether guards were multiply
;;       true, if we cared to do that.
;;
;; ii  - we could implement else as a syntactic negation of the disjunction
;;       of all previous guards, but this would be inefficient.
;;
;; iii - we could implement else as its own special expression object.
;;       I think this is the approach we will choose.
;;
;;
;; UNDECLARED VARIABLES
;; ====================
;;
;; Undeclared variables appearing anywhere in the process text are
;; treated as if they were declared as signed integers (CSP "int"
;; type) at the beginning of the process (and initialized to zero).
;;
;; There is a trap here regarding the CSP integer types:
;;
;; int     -- infinite-precision, signed integer
;;            (representing a number from Z)
;;
;; int(N)  -- N-bit precision UNSIGNED integer, in the range 0..2^N-1
;;
;; sint(N) -- N-bit precision SIGNED integer in two's complement, in the
;;            range -2^(N-1) .. 2^(N-1) - 1
;;
;; Note that "int" is more like a "sint(N)" than it is like an "int(N)"
;;
;;
;; FUNCTION-CALL INLINING
;; ======================
;;
;; Function calls have to be inlined.  This is not just for efficiency,
;; but because functions can block, on sends, receives, and selections.
;;
;; 
;; EXPRESSION UNFOLDING/SEQUENCING
;; ===============================
;;
;; We need to unfold expressions so they can be evaluated sequentially
;; in a form of 3-address code.  This will allow the intermediate types
;; of the expressions to be derived, which is needed for code generation.
;;
;; Expression unfolding is complicated by a few things.
;;
;; 1. It interacts in a nasty way with function calls and function inlining
;;
;; 2. Receive expressions
;;
;; 3. Loop expressions
;;
;;
;; REPRESENTATIONS
;; ===============
;;
;; The Java S-expression generator generates a parse tree basically
;; replicating the Java AST types from the com/avlsi/csp/ast directory.
;; These types contain quite a bit of syntactic sugar.  We desugar the
;; tree in a seemingly roundabout way: convert the tree to Modula-3
;; types using the CspAst interface, then dump out a new S-expression
;; by calling the .lisp() method of the CspSyntax interface.  Part of
;; the reason to do this is to avail ourselves of the strict typechecking
;; of Modula-3 but also to allow future, faster implementations, closer
;; to machine language than the very flexible but likely quite slow
;; code that we have here in the Scheme environment.
;;
;;
;; GENERALLY HAIRY STUFF
;; =====================
;;
;; Lots of things, really, but one of the trickiest is the parallel loop
;;
;; <,i:lo..hi: S(i)>;
;;
;; This parallel loop is especially difficult because the amount of
;; parallelism needed for implementation is known only at execution
;; time.  Otherwise, the block structure of the program, including
;; branching and joining of the locus/loci of control, is known at
;; compile time.  But the parallel loop introduces dynamic
;; parallelism.
;;
;; The sequential loop can be desugared to a regular do loop.  The
;; variable scoping rules imply that the dummy index can be declared
;; locally to the do loop.  Note that this means we need to introduce
;; some sort of block for variable declarations.  Ho hum...
;;
;; Loop expressions can be desugared during expression unfolding.
;;
;;
;; The multiple steps listed above interact in a way that requires
;; them to be called multiple times until a fixpoint is reached, which
;; ought to be the finished program, ready for code generation.
;;
;; Well, let's hope it works!

(require-modules "basic-defs" "m3" "hashtable" "display" "symbol-append.scm")


;;            } else if (name.equals("string")) {
;;               preamble.add(createVarStatement(temp, new StringType()));
;;            } else if (name.equals("print") || name.equals("assert") ||
;;                       name.equals("cover") || name.equals("unpack")) {
;;                noReturn = true;
;;            } else if (name.equals("pack")) {
;;                preamble.add(createVarStatement(temp,
;;                                                new TemporaryIntegerType())); 
;;            }

;; also see com/avlsi/csp/util/RefinementResolver.java


(define *the-example* "parents_p2") ;; this is the test file

(define (identity x) x)

(define *big-m1* (BigInt.New -1))
(define *big-1*  (BigInt.New  1))
(define *big-tc* (rttype-typecode *big-1*))

(define special-functions     ;; these are special functions per Harry
  '(string print assert cover pack unpack))

(define (caddddr x) (car (cddddr x)))

(define (load-csp fn)

  (let* ((p   (open-input-file fn))
         (res (read-big-int    p)))
    (close-input-port p)
    res
    )
  )

;; this stuff is really experimental.
(define *cell*        '())

(define *data*        '())
(define *cellinfo*    '())

(define funcs       '())
(define structs     '())
(define refparents  '())

(define declparents '()) ;; this is a special thing for env blocks

(define inits       '())
(define text        '())


;;
;; pull apart the structure of a CSP process as generated by the Java
;;

(define (get-funcs       proc) (nth proc 1))
(define (get-structs     proc) (nth proc 2))
(define (get-refparents  proc) (nth proc 3))
(define (get-declparents proc) (nth proc 4))
(define (get-inits       proc) (nth proc 5))
(define (get-text        proc) (nth proc 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; close the process
;;
;; this is intended to work on and return the raw, unconverted,
;; not-yet-desugared s-expressions generated by the Java front-end.
;;

(define (close-text proc)
  ;; find the correct program text for this process
  (let loop ((p proc))
    (let ((this-text (get-text p)))
      (if (not (null? this-text))
          this-text
          (car (map loop (get-refparents p)))))))

(define (filter-not filter-pred)
  (lambda(x)(not (filter-pred x))))
                         
(define (make-sequence . x)
  (cons 'sequence (filter (filter-not null?) x)))

(define (merge-all getter appender proc)
  ;; recursively merge "something" from my parents and me 
  (let* ((my-stuff (getter proc))
         (my-parents (append (get-declparents proc) (get-refparents proc)))
         (their-stuff (apply appender
                             (map (lambda(p)(merge-all getter appender p))
                                  my-parents))))
    (appender their-stuff my-stuff)))

(define *the-text* #f)
(define *the-structs* #f)
(define *the-funcs* #f)
(define *the-inits* #f)
(define *the-initvars* #f)

(define *the-func-tbl* #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; function stuff
;;

(define (is-function? func)
  (and (pair? func) (eq? 'function (car func))))

(define (check-is-function func)
  (if (not (is-function? func))
      (error "not a function : " func)))

(define (get-function-name func)
  (check-is-function func)
  (cadr func))

(define (get-function-params func)
  (check-is-function func)

  ;; function decls are unconverted, so desugar declarators
  (map CspDeclarator.Lisp
       (map convert-declarator
            (apply append (caddr func)))))

(define (get-function-return func)
  (check-is-function func)
  (cadddr func))

(define (get-function-text func)
  (check-is-function func)

  ;; function decls are unconverted, so desugar here
  (desugar-stmt (caddddr func)))

(define (get-function-captures func)
  ;; list of captured identifiers of this function
  (let ((param-captures (map cadr (map cadr (get-function-params func)))))
    (if (null? (get-function-return func))
        param-captures
        (cons (get-function-name func) param-captures))))

(define (uniqify-function-text func sfx)
  (let loop ((cp   (get-function-captures func))
             (text (get-function-text func)))
    (if (null? cp)
        text
        (loop (cdr cp)
              (rename-id text
                         (car cp)
                         (symbol-append (car cp) sfx))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-object-hash-table elem-namer lst)
  (let ((res (make-hash-table 10 Atom.Hash)))
    (map (lambda(elem)(res 'add-entry! (elem-namer elem) elem)) lst)
    res
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (loaddata! nm)
  (begin
    (set! *cell*  (load-csp (string-append nm ".scm")))

    (set! *data* (cadr *cell*))       ;; the CSP code itself
    (set! *cellinfo* (caddr *cell*))  ;; the CAST ports

    (switch-proc! *data*)
    )
  )

(define xxx #f)

(define (switch-proc! data)
  (map
   (lambda (sym)
     (let ((entry
            (apply (eval (symbol-append 'get- sym)) (list data))))
       (dis "sym = " sym dnl)
       (dis "entry = " entry dnl)
       (eval (list 'set! sym 'entry))
       ))
   '(funcs structs refparents declparents inits text)
   )
  
  (map
   (lambda (sym)
     (dis (string-append "(length " (symbol->string sym) ") = ")
          (eval (list 'length sym)) dnl))
   '(funcs structs refparents declparents inits text))

  (set! *the-text* (simplify-stmt (desugar-stmt (close-text data))))
  (set! *the-funcs* (merge-all get-funcs append data))
  (set! *the-structs* (merge-all get-structs append data))


  ;; some problem here: why can't I desugar-stmt right here?
  (set! *the-inits*   (simplify-stmt
                       (desugar-stmt
                        (merge-all get-inits make-sequence data))))

  (set! *the-initvars* (find-referenced-vars *the-inits*))

  (set! *the-func-tbl* (make-object-hash-table get-function-name *the-funcs*))
  'ok
    
  )

(define (deep-copy x)
  (if (pair? x)
      (cons (deep-copy (car x)) (deep-copy (cdr x)))
      x))

(define (skip) )

(define (spaces n)
  (if (= n 0) "" (string-append " " (spaces (- n 1)))))

    
(define (pp-depth x n nsp)
  (if (list? x)
      (begin
        (dis (spaces nsp) "(" dnl)
        (map (if (= n 0)
                 (lambda (c) (dis (spaces (+ 4 nsp)) (stringify c) dnl))
                 (lambda (c) (pp-depth c (- n 1) (+ nsp 4))))
             x)
        (dis (spaces nsp) ")" dnl))
      (dis (spaces nsp) (stringify x) dnl)
      )
  )

(define (pp lst . n) ;; pretty-print
  (pp-depth lst (if (null? n) 1 (car n)) 0)
  #t)

(define (make-var name type)
  (cons name type))

(define (make-var-hash-table size)
  (make-hash-table size (lambda(var)(Atom.Hash (car var)))))

(define *bad-s* #f)
(define *bad-last* #f)

(define *s* #f)
(define *last* #f)
(define *var-s* #f)

(define (init-seq type)
  (let ((res (obj-method-wrap (new-modula-object type) type)))
    (obj-method-wrap (res 'init 10) type)))

(define *last-var* #f)

(define (convert-dir dir)
  (case dir
    ((none) 'None)
    ((in)   'In)
    ((out)  'Out)
    ((inout) 'InOut)
    (else (error "convert-dir : unknown direction : " dir))
    )
  )

(define (convert-range r)
  (if (or (not (pair? r))
          (not (eq? (car r) 'range)))
      (error "not a range : " r)
      `((min . ,(convert-expr (cadr r)))
        (max . ,(convert-expr (caddr r))))))

(define (convert-type type)
  (cond
   ((eq? type 'boolean) (CspAst.BooleanType))
   ((eq? type 'string)  (CspAst.StringType))
   ((pair? type)
    (case (car type)
      ((array)       (CspAst.ArrayType (convert-range (cadr type))
                                       (convert-type (caddr type))))
      ((channeltype) (CspAst.ChannelType (cadr type) (convert-dir (caddr type))))
      ((integer)     (let ((isConst (cadr type))
                           (isSigned (caddr type))
                           (dw       (cadddr type))
                           (interval (caddddr type)))
                       
                       (CspAst.IntegerType
                        isConst
                        isSigned
                        (not (null? dw))
                        (if (null? dw) 0 (BigInt.ToInteger dw))
                        (not (null? interval))
                        (if (null? interval)
                            '(() ())
                            (list (car interval) (cadr interval))))))
      
      
      ((node-array)  (CspAst.NodeType #t (caddr type) (convert-dir (cadr type))))
      ((node)        (CspAst.NodeType #f 1 (convert-dir (cadr type))))
      ((structure)   (CspAst.StructureType (cadr type)
                                           (symbol->string (caddr type))))
      (else (error "convert-type : unknown type (pair) : " type))
      )
    )
    
   (else (error "convert-type : unknown type : " type))
   )
  )

;;
;; make a few routines to initialize default values of various types.
;; should generate a sequence of statements to initialize a variable of
;; the given type.  Note that structures may have nonzero initial values.
;;

(define *last-decl* #f)

(define (convert-declarator decl)
  (set! *last-decl* decl)
;;  (dis "convert-declarator : " decl dnl)
  (case (car decl)
    ((decl)
     (let ((ident (cadr decl))
           (type  (caddr decl))
           (dir   (cadddr decl))
           (expr  (caddddr decl)))
       (CspAst.Declarator
        (if (or (not (pair? ident))
                (not (eq? (car ident) 'id)))
            (error "convert-declarator : unexpected identifier in declarator : " ident)
            (cadr ident))
        (convert-type type)
        (convert-dir  dir))
       ))
    ((decl1)
     (let ((ident (cadr decl))
           (type  (caddr decl))
           (dir   (cadddr decl)))
       (CspAst.Declarator
        (if (or (not (pair? ident))
                (not (eq? (car ident) 'id)))
            (error "convert-declarator : unexpected identifier in declarator : " ident)
            (cadr ident))
        (convert-type type)
        (convert-dir  dir))
       ))
    (else (error "convert-declarator : dont know declarator : " decl))
    )
  )

(define (convert-struct-declarator decl)
  (set! *last-decl* decl)
  (dis "convert-declarator : " decl dnl)
  (let ((ident (cadr decl))
        (type  (caddr decl))
        (dir   (cadddr decl))
        (expr  (caddddr decl)))
    (CspAst.StructDeclarator
     (if (or (not (pair? ident))
             (not (eq? (car ident) 'id)))
         (error "convert-declarator : unexpected identifier in declarator : " ident)
         (cadr ident))
     (convert-type type)
     (if (null? expr) '() (convert-expr expr))
     (convert-dir  dir))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cadadadr x) (cadr (cadadr x)))
(define (cdaadadr x) (cdar (cadadr x)))
(define (caadadr x) (car (cadadr x)))
(define (caddadr x) (caddr (cadr x)))

(define (get-decl1-id d)
  (cadadr d))

(define (get-decl1-type d)
  (caddr d))

(define (get-decl1-dir d)
  (cadddr d))
  
(define (check-var1 s)
  (if (not (and (eq? 'var1 (stmt-type s)) (eq? 'id (caadadr v1))))
      (error "malformed var1 : " s)))

(define (get-var1-decl1 s)
  (check-var1 s)
  (cadr s))

(define (get-var1-id s)
  (get-decl1-id (get-var1-decl1 s)))

(define (get-var1-type s)
  (get-decl1-type (get-var1-decl1 s)))

(define (convert-var-stmt s)
  (set! *last-var* s)

  (let ((decls (cadr s))
        (decl  (caadr s)))

    (if (not (= 1 (length decls)))
        (error "convert-var-stmt : need to convert single var decl : " s))

    ;; check whether it's an original declarator and if so if it
    ;; has an initial value
    (let ((var-result     (CspAst.VarStmt (convert-declarator decl)))
          (init-val       (caddddr decl)))

      ;; XXX what we should do here is insert a default initialization,
      ;; for objects that do not have explicit initialization.
      ;;
      ;; that initialization should depend on the type of the object
      ;;
      ;; trivial initialization is used for boolean, int, and string
      ;;
      ;; but (slightly) non-trivial initialization can be used for structs
      
      (if (and (eq? (car decl) 'decl) (not (null? init-val)))
          (let ((assign-result
                 (convert-stmt (list 'assign (cadr decl) init-val)))
                (seq (init-seq 'CspStatementSeq.T)))
            (seq 'addhi var-result)
            (seq 'addhi assign-result)
            (CspAst.SequentialStmt (seq '*m3*)))

          var-result
          )
      )
    )
  )

(define (convert-var1-stmt s)
  (let ((decl  (cadr s)))
    (CspAst.VarStmt (convert-declarator decl))
    )
  )

(define (flatten-var-stmt s)
  ;; this desugars the var stmts to var1 stmts
  ;; (one decl per statement)
  (let ((the-sequence 
         (cons 'sequence
               (map
                (lambda(d)(list 'var (list d) (caddr s)))
                (cadr s)))))
    the-sequence
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compound-stmt? s)
  (and (list? s)
       (not (null? s))
       (or (eq? (car s) 'parallel) (eq? (car s) 'sequence))))

(define (guarded-stmt? s)
  (and (list? s)
       (not (null? s))
       (or (eq? (car s) 'if)
           (eq? (car s) 'nondet-if)
           (eq? (car s) 'do)
           (eq? (car s) 'nondet-do))))

(define (simplify-stmt s)
  (cond ((compound-stmt? s) (simplify-compound-stmt s))
        ((guarded-stmt? s) (simplify-guarded-stmt s))
        (else s)))

(define (simplify-guarded-stmt s)
  (cons (car s)
        (map
         (lambda(gc)
           (let ((guard   (car gc))
                 (command (cadr gc)))
             (list guard (simplify-stmt command))))
         (cdr s))))
        
(define (simplify-compound-stmt s)
  (let loop ((p    (cdr s))
             (res  (list (car s))))

    (if (null? p)

        (reverse res) ;; base case

        (let ((next (simplify-stmt (car p))))

          (if (and (compound-stmt? next)
                   (eq? (car next) (car s)))

              (begin
;; same type of statement, just splice in the args
;;                    (dis " splicing next  : " next dnl)
;;                    (dis " splicing cdr next : " (cdr next) dnl)
                
                (loop (cdr p)
                      (append (reverse (cdr next)) res))
                )
              
              (loop (cdr p) (cons next res))))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; visitors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the type visitor doesn't actually get at structure types
;; because structure types are defined in a separate space to the
;; text of the CSP process.

(define (prepostvisit-type t
                           stmt-previsit stmt-postvisit
                           expr-previsit expr-postvisit
                           type-previsit type-postvisit)

  (define (type tt) (prepostvisit-type tt
                                       stmt-previsit stmt-postvisit
                                       expr-previsit expr-postvisit
                                       type-previsit type-postvisit))


  (define (continue)
    (if (and (list? t) (eq? 'array (car t)))
        (list 'array (cadr t) (type (caddr t)))
        t
        )
    )
  

  (let ((pre (type-previsit t)))
    (cond ((eq? pre #f) t)
          ((and (pair? pre) (eq? 'cut (car pre))) (cdr pre))
          (else (type-postvisit (continue)))))
  )
  
(define (prepostvisit-declarator d 
                         stmt-previsit stmt-postvisit
                         expr-previsit expr-postvisit
                         type-previsit type-postvisit)
  (if (not (equal? 'decl1 (car d)))
      (error "prepostvisit-declarator : not a desugared declarator : " d))

   (let ((ident (cadr d))
         (type  (caddr d))
         (dir   (cadddr d)))
     (list (car d)
           ident
           (prepostvisit-type type 
                              stmt-previsit stmt-postvisit
                              expr-previsit expr-postvisit
                              type-previsit type-postvisit)
           dir)
     )
   )
  

(define (prepostvisit-var1-stmt s
                         stmt-previsit stmt-postvisit
                         expr-previsit expr-postvisit
                         type-previsit type-postvisit)
  (list 'var1 (prepostvisit-declarator (cadr s)
                                       stmt-previsit stmt-postvisit
                                       expr-previsit expr-postvisit
                                       type-previsit type-postvisit))
)

(define vs #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stmt-type stmt)
  (if (eq? stmt 'skip)
      'skip
      (car stmt)))

(define (prepostvisit-stmt s
                           stmt-previsit stmt-postvisit
                           expr-previsit expr-postvisit
                           type-previsit type-postvisit)
  ;; first run stmt-previsit
  ;; if it returns #f, stop recursing
  ;; if it returns (cut . XXX) return XXX
  ;; else run (stmt-postvisit (prepostvisit-stmt ...))

  (define (stmt ss)(prepostvisit-stmt
                    ss
                    stmt-previsit stmt-postvisit
                    expr-previsit expr-postvisit
                    type-previsit type-postvisit
                    ))
    
  (define (expr x)(prepostvisit-expr
                   x
                   stmt-previsit stmt-postvisit
                   expr-previsit expr-postvisit
                   type-previsit type-postvisit
                   ))

  (define (continue)
    (if (eq? s 'skip)
        s
        (begin
          (if (not (pair? s))
              (begin
                (set! *bad-s* s)
                (set! *bad-last* last)
                (error "Not a statement : " s dnl "last : " last)))
          
          (let ((kw   (car s))
                (args (cdr s))
                )
            (cons kw
                  (case kw
                    ((sequence parallel) (filter filter-delete (map stmt args)))
                    
                    ((assign)            (list
                                          (expr (car args)) (expr (cadr args))))
                    
                    ((loop increment decrement var)
                     (error "visit-stmt : need to desugar : " kw))
                    
                    ((var1)
                     (cdr (prepostvisit-var1-stmt
                           s

                           stmt-previsit stmt-postvisit
                           expr-previsit expr-postvisit
                           type-previsit type-postvisit
                           )))
                    
                    ((recv send)
                     (list (expr (car args)) (expr (cadr args))))
                    
                    ((do if nondet-if nondet-do)
                     (set! vs s)
                     (map (lambda(gc)(list (expr (car gc)) (stmt (cadr gc))))
                          args))
                    
                    ((eval) (list (expr (car args))))
                    
                    (else (set! *bad-s* s)
                          (set! *bad-last* last)
                          (error "visit-stmt : unknown statement " s))
                    )))
          )
        )
    )
 
    
  (let ((pre (stmt-previsit s)))
    (cond ((eq? pre #f) s)
          ((and (pair? pre) (eq? 'cut (car pre))) (cdr pre))
          (else (stmt-postvisit (continue)))))
  )
  
(define (filter-delete stmt) ;; use to filter out 'delete
  (not (eq? 'delete stmt)))

(define (prepostvisit-expr x
                           stmt-previsit stmt-postvisit
                           expr-previsit expr-postvisit
                           type-previsit type-postvisit)
  
  (define (expr x)(prepostvisit-expr x
                                     stmt-previsit stmt-postvisit
                                     expr-previsit expr-postvisit
                                     type-previsit type-postvisit))

  (define (continue)
    (if (pair? x)
        (let ((kw (car x))
              (args (cdr x)))
          (cons kw
                (case kw
                  ((id) args)
                  
                  ;; unary/binary ops
                  ((probe array-access - not
                    + / % * == != < > >= <= & && | || ^ == << >> ** ) ;; | )
                   (map expr args))
                  
                  ((apply)
                   (cons (car args) (map expr (cdr args))))
                  
                  ((member-access structure-access)
                   (list (expr (car args)) (cadr args))
                   )
                  
                  (else (error "visit-expr : unknown keyword " kw " : " x ))
                  ) ;; esac
                
                );; snoc
          
          );;tel
        x ;; not a pair
        );;fi
    );;enifed

  (let ((pre (expr-previsit x)))
    (cond ((eq? pre #f) x)
          ((and (pair? pre) (eq? 'cut (car pre))) (cdr pre))
          (else (expr-postvisit (continue)))))

  );;enifed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; short forms for postvisit only
;;

(define (visit-stmt s stmt-visitor expr-visitor type-visitor)
  (prepostvisit-stmt s
                     identity stmt-visitor
                     identity expr-visitor
                     identity type-visitor))

(define (visit-expr s stmt-visitor expr-visitor type-visitor)
  (prepostvisit-expr s
                     identity stmt-visitor
                     identity expr-visitor
                     identity type-visitor))

(define (visit-type t stmt-visitor expr-visitor type-visitor)
  (prepostvisit-type t
                     identity stmt-visitor
                     identity expr-visitor
                     identity type-visitor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-diff lst0 lst1)
  (filter (lambda(x)(not (member? x lst1))) lst0))

(define (set-union lst0 lst1)
  (uniq eq? (append lst0 lst1)))

(define (set-intersection lst0 lst1)
  (filter (lambda(x)(member? x lst1)) lst0))
  
(define (multi lst) ;; multiply defined items in a list
  (let loop ((res '())
             (p lst))
    (cond ((null? p) (uniq eq? res))
          ((member? (car p) (cdr p))
           (loop (cons (car p) res) (cdr p)))
          (else (loop res (cdr p))))
    )
  )

(define (count-in item lst)
  (let loop ((res 0)
             (p lst))
    (cond  ((null? p) res)
           ((eq? item (car p)) (loop (+ res 1) (cdr p)))
           (else (loop res (cdr p))))
    )
  )


(define (find-ids lisp)
  (define ids '())
  
  (define (expr-visitor x)
;;     (dis "expr : " x dnl)

     (if (and (pair? x) (eq? 'id (car x)))
         (if (not (member? (cadr x) ids))
             (set! ids (cons (cadr x) ids))))
     x
  )

  (visit-stmt lisp identity expr-visitor identity)
  ids
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *result* #f)
(define *undeclared* #f)

;; we can update the inits to only those that we actually need
(define *filtered-inits* #f)
(define *filtered-initvars* #f)

(define (analyze-program prog cell-info initvars)
  (let* ((ports      (caddddr cell-info))
         (portids    (map cadr ports))
         (textids    (find-referenced-vars prog))
         (globalids  (set-intersection textids initvars))
         (unused-globalids
                     (set-diff initvars globalids))
         (undeclared (set-diff
                      (find-undeclared-vars prog portids) globalids))
         (declnames  (find-declaration-vars prog))
         (multiples  (multi declnames))
         )

    ;; we should consider *the-inits* here.  We don't need to declare
    ;; anything that *the-inits* declares.
    (dis "portids    : " portids dnl)
    (dis "textids    : " textids dnl)
    (dis "globalids  : " globalids dnl)
    (dis "undeclared : " undeclared dnl) (set! *undeclared* undeclared)
    (dis "decls      : " declnames dnl)
    (dis "multiples  : " multiples dnl)

    (cond ((not (null? multiples))
           (begin
             (dis dnl "uniqifying..." dnl dnl)
             (analyze-program (uniqify-stmt prog) cell-info)))

          (else (set! *result* prog) 'ok)
          )
    )
    
  )




(define (find-referenced-vars stmt)
  (find-ids stmt))

(define (find-undeclared-vars stmt portids)
  (let* ((used (find-referenced-vars stmt))
         (declared (find-declared-vars stmt)))
    (set-diff (set-diff used declared) portids)))
               

(define (find-var1-stmts lisp)
  (define var1s '())

  (define (stmt-visitor s)
    ;;(dis "stmt : " s dnl)
    
    (if (and (pair? s) (eq? 'var1 (car s)))
        (set! var1s (cons s var1s)))

    s
    )

  (visit-stmt lisp stmt-visitor identity identity)
  var1s
)

(define (find-declared-vars lisp) ;; set of declared vars
  (uniq eq? (map cadr (map cadadr (find-var1-stmts lisp)))))

(define (find-declaration-vars lisp) ;; multiset of declarations
  (map cadr (map cadadr (find-var1-stmts lisp))))

(define (rename-id lisp from to)
  (define (expr-visitor x)
     (if (and (pair? x) (eq? 'id (car x)) (eq? from (cadr x)))
         (list 'id to)
         x
         )
   )
  
  (define (stmt-visitor s)
    ;; the only place that an identifier appears outside of expressions
    ;; is in "var1" statements -- and in function decls
    (if (and (pair? s) (eq? 'var1 (car s)) (equal? `(id ,from) (cadadr s)))
        `(var1 (decl1 (id ,to) ,@(cddadr s)))
        s
        )
    )

  (visit-stmt lisp stmt-visitor expr-visitor identity)
)

(define (count-declarations of stmt)
  (count-in of (find-declaration-vars stmt)))

(define (uniqify-one stmt id tg)

  (define (visitor s) 
    (if (= (count-declarations id s) 1)
        (cons 'cut
              (rename-id s id (symbol-append id '- (tg))))
        stmt))

  (if (>= 1 (count-declarations id stmt))
      (error "not defined enough times : " id " : in : " stmt))

  (prepostvisit-stmt stmt
                     visitor  identity
                     identity identity
                     identity identity)
)
                     
(define (uniqify-stmt stmt)
  (let ((tg    (make-name-generator "uniq"))
        (names (multi (find-declaration-vars stmt))))
    (let loop ((p names)
               (s stmt))
      (if (null? p)
          s
          (begin
;;            (dis "uniqifying " (car p) dnl)
            (loop (cdr p) (uniqify-one s (car p) tg)))))
    )
  )
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-identity x)
  (dis (stringify x) dnl)
  x
  )

(define (print-identity-and-referenceable x)
  (dis (stringify x) " referenceable: " (directly-referenceable? x) dnl)
  x
  )

(define (identifier? x)
  (if (and (pair? x) (eq? 'id (car x)))) x #f)

(define (literal? x)
  (not (pair? x)))

(define (directly-referenceable? x)
  ;; is x an expression that's implementable as the RHS of a 3-address
  ;; instruction?
  ;;
  ;; in other words, can x be used as an operand in a basic
  ;; operation, and can it be used as a function argument,
  ;; and can it be used as a channel to be passed to a channel operation?
  ;;
  ;; there are some oddities with array indexing:
  ;;
  ;; I believe if x is an array-indexing operation, it can be implemented
  ;; as long as every array index is a constant or an identifier.
  ;;
  ;; otherwise, x must be a constant or an identifier itself

  ;; XXX not done
  
  (dis "directly-referenceable? " x dnl)
  
  (if (pair? x)
      (let ((kw (car x))
            (args (cdr x)))
        (case kw
          ((id) #t)

          ((array-access)
           (and (identifier? (car args))
                (directly-referenceable? (cadr args))))

          ((member-access structure-access)
           (directly-referenceable? (car args)))

          (else #f)))

      #t
      )
  )

(define (make-referenceable x temp-generator)
  (if (directly-referenceable? x)
      x
      (let ((new-var (temp-generator)))
        (list 'id new-var))))

;; this code isnt quite right
;; it should only be applied to the arguments of say assign.


;; (sequentialize-one
;;          '(assign (id a) (+ (id a) (id b)))
;;           (make-name-generator "t"))

(define (binary-expr? x)
  (and
   (pair? x)
   (= 3 (length x))
   (binary-op? (car x))))

(define (binary-op? op)
  (case op
    ((+ / % * == != < > >= <= & && ^ == << >> ** array-access | || ; |
        )
     #t)
    (else #f)))

(define (unary-expr? x)
  (and
   (pair? x)
   (= 2 (length x))
   (unary-op? (car x))))

(define (unary-op? op)
  (case op
    ((not -) #t)
    (else #f)))
            

(define (sequentialize-one s tg) 
  (if (eq? s 'skip)
      'skip
      (let ((kw   (car s))
            (args (cdr s)))
        (case kw
          ((assign) (list 'assign
                          (car args) ;; don't do lvalues yet...?
                          (make-referenceable (cadr args) tg)
                          )
           )
          
          (else s))
        
        )))
               

(define (sequentialize-stmt s)
  (let ((tg (make-name-generator "t")))
    (visit-stmt s
                sequentialize-one
                identity
                identity)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-name-generator rootstring)
  ;;
  ;; return a temporary-name-generator
  ;; a procedure of zero arguments, which generates incrementing temp names
  ;;
  (let ((root                rootstring)
        (counter              0))
    (lambda()
      (let ((res (string->symbol(string-append root (stringify counter)))))
        (set! counter (+ counter 1))
        res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           
(define (convert-stmt s . last)
  (set! *s* s)
  (set! *last* last)
;;  (dis "CONVERT : " s dnl)

  (if (eq? s 'skip)

      ;; special case for skip, only token allowed on its own
      (CspAst.SkipStmt)

      ;; not a skip -- handle it properly
      (begin
        (if (not (pair? s))
            (begin
              (set! *bad-s* s)
              (set! *bad-last* last)
              (error "Not a statement : " s dnl "last : " last)))
        
        (let ((kw (car s))
              (args (cdr s))
              )
;;          (dis "kw is " kw dnl)
          
          (case kw
            
            ((sequence parallel) ;; sequential and parallel composition
             (let ((seq (init-seq 'CspStatementSeq.T)))
               (map (lambda(ss)
                      (seq 'addhi (convert-stmt ss s)))
                    args
                    )
               ((if (eq? kw 'sequence)
                    CspAst.SequentialStmt
                    CspAst.ParallelStmt)
                (seq '*m3*))
               
               ))

            ((assign) ;; simple assignment
             (CspAst.AssignmentStmt (convert-expr (car args))
                                    (convert-expr (cadr args))
                                )
             )

            ((assign-operate) ;; desugar to assign

             (error "not yet done" dnl)
             
             (let ((transformed (list 'assign
                                   (cadr args)
                                   (list
                                    (car args)
                                    (cadr args)
                                    (caddr args)))))
;;               (dis "transform " s " -> " transformed dnl)
               (convert-stmt transformed s)))

            ;; the next two are just syntactic sugar
            ;;
            ;; we desugar them here, so the rest of the code doesn't have
            ;; to handle them.

            ((increment) ;; desugar to assign-operate
             (convert-stmt (list 'assign-operate '+ (car args) *big-1*)
                           s)
             )

            ((decrement) ;; desugar to assign-operate
             (convert-stmt (list 'assign-operate '- (car args) *big-1*)
                           s)
             )

            ((var) ;; desugar to var1
             (if (= 1 (length (cadr s)))
                 
                 (convert-var-stmt s)
                 
                 (convert-stmt (flatten-var-stmt s) s))
             )

            ((var1) ;; this is a simplified declaration
             (convert-var1-stmt s))

            ((recv) (CspAst.RecvStmt (convert-expr (car args))
                                     (convert-expr (cadr args))))

            ((send) (CspAst.SendStmt (convert-expr (car args))
                                     (convert-expr (cadr args))))

            ((do if nondet-if nondet-do)
             (let ((seq (init-seq 'CspGuardedCommandSeq.T)))
               (map (lambda (gc)
                      (seq 'addhi
                           (CspAst.GuardedCommand

                            (let* ((guard (car gc)) ;; convert -1 to #t
                                   (expr-guess 
                                    (convert-expr guard)))
                              (if (and (= *big-tc* (rttype-typecode guard))
                                       (BigInt.Equal *big-m1* guard))
                                  (CspAst.BooleanExpr #t)
                                  expr-guess))
                            
                            (convert-stmt (cadr gc) s))))
                    args)

               (let ((maker
                      (case kw
                        ((do) CspAst.DetRepetitionStmt)
                        ((if) CspAst.DetSelectionStmt)
                        ((nondet-do) CspAst.NondetRepetitionStmt)
                        ((nondet-if) CspAst.NondetSelectionStmt)
                        )))

                 (maker (seq '*m3*)))
               ))

            ((eval) ;; this is ONLY used for function evaluations
             (CspAst.ExpressionStmt (convert-expr (car args))))
            
            (else (set! *bad-s* s)
                  (set! *bad-last* last)
                  (error "convert-stmt : unknown statement " s))
            )
          )
        )
      ))

(define *last-x* #f)

(define *all-x* '())

(define *rest* #f)

(define *last-a* #f)

(define (convert-expr x)
  (set! *last-x* x)
  (set! *all-x* (cons x *all-x*))

;;  (dis "EXPR : " x dnl)

  (cond ((null? x) (error "convert-expr : x is null"))

        ((BigInt.IsT x)  (CspAst.IntegerExpr x))

        ((string? x) (CspAst.StringExpr x))

        ((eq? x 'else) (CspAst.ElseExpr))

        ((eq? x #t) (CspAst.BooleanExpr #t))

        ((eq? x #f) (CspAst.BooleanExpr #f))

        ((pair? x)

         ;; a pair...
         (case (car x)
           ((probe) (CspAst.ProbeExpr (convert-expr (cadr x))))

           ((array-access) (CspAst.ArrayAccessExpr (convert-expr (cadr x))
                                                   (convert-expr (caddr x))))

           ((id) (CspAst.IdentifierExpr (cadr x)))

           ((member-access) (CspAst.MemberAccessExpr (convert-expr (cadr x))
                                                     (caddr x)))
           ((structure-access) (CspAst.StructureAccessExpr (convert-expr (cadr x))
                                                     (caddr x)))

           ((apply)
            (let ((fx     (convert-expr (cadr x)))
                  (rest   (cddr x))
                  (argseq (init-seq 'CspExpressionSeq.T)))

              (set! *rest* rest)
              (map
               (lambda(a)
                 (set! *last-a* a)
                 (argseq 'addhi (convert-expr a)))
               rest)
              (CspAst.FunctionCallExpr fx (argseq '*m3*))
              )
            )

           ((not) (CspAst.UnaExpr 'Not (convert-expr (cadr x))))

           ((-)
            ;; - is special

            (if (null? (cddr x))
                (CspAst.UnaExpr 'Neg (convert-expr (cadr x)))
                (CspAst.BinExpr 'Sub (convert-expr (cadr x))
                                     (convert-expr (caddr x)))))

           ((+ / % * == != < > >= <= & && | || ^ == << >> **)
            (CspAst.BinExpr
             (case (car x)
               ((+) 'Add)
               ((/) 'Div)
               ((%) 'Rem)
               ((*) 'Mul)
               ((==) 'EQ)
               ((!=) 'NE)
               ((<) 'LT)
               ((>) 'GT)
               ((>=) 'GE)
               ((<=) 'LE)
               ((&) 'And)
               ((&&) 'CondAnd)
               ((|) 'Or)
               ((||) 'CondOr)
               ((^) 'Xor)
               ((<<) 'SHL)
               ((>>) 'SHR)
               ((**) 'Pow)
               (else (error " BinExpr " (car x))))
             (convert-expr (cadr x))
             (convert-expr (caddr x))))
           
           (else (error "convert-expr : unknown keyword " (car x) " : " x ))
           )
         )
        
        (else (error "dunno that type " x) )
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (desugar-stmt stmt)
  ((obj-method-wrap (convert-stmt stmt) 'CspSyntax.T) 'lisp))

(define (desugar-prog p)
  (if (not (and (pair? p) (eq? (car p) 'csp)))
      (error (string-append "Not a CSP program : " p)))

      (desugar-stmt (close-text p))
  )

(define (reload) (load "cspc.scm"))

(loaddata! *the-example*)
;;(loaddata! "arrays_p1")
;;(loaddata! "functions_p00")


(define a (BigInt.New 12))

;;(define csp (obj-method-wrap (convert-prog data) 'CspSyntax.T))
(define lispm1 (close-text *data*))
(define lisp0 (desugar-prog *data*))

(set-rt-error-mapping! #f)

(define lisp1 (simplify-stmt lisp0))
(define lisp2 (simplify-stmt ((obj-method-wrap (convert-stmt lisp1) 'CspSyntax.T) 'lisp)))
(define lisp3 (simplify-stmt ((obj-method-wrap (convert-stmt lisp2) 'CspSyntax.T) 'lisp)))
(define lisp4 (simplify-stmt ((obj-method-wrap (convert-stmt lisp3) 'CspSyntax.T) 'lisp)))

(if (not (equal? lisp1 lisp4)) (error "lisp1 and lisp4 differ!"))

(define (do-analyze)
   (analyze-program lisp1 *cellinfo* *the-initvars*))
