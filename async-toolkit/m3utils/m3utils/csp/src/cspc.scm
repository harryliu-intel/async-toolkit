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

(require-modules "basic-defs" "m3" "hashtable" "set"
                 "display" "symbol-append.scm" "clarify.scm")


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


(define (identity x) x)

(define (bigint? x)
  (cond ((null? x) #f)
        ((pair? x) #f)
        ((= *big-tc* (rttype-typecode x)) #t)
        (else #f)))
      

(define special-functions     ;; these are special functions per Harry
  '(string
    print
    assert
    cover   // what's this?
    pack
    unpack 
    ))

(define nonblocking-intrinsics
  '(string print pack unpack))

(define (caddddr x) (car (cddddr x)))
(define (cadaddr x) (car (cdaddr x)))

(define (load-csp fn)

  (let* ((p   (open-input-file fn))
         (res (read-big-int    p)))
    (close-input-port p)
    res
    )
  )


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; function stuff
;;

(define (function? func)
  (and (pair? func) (eq? 'function (car func))))

(define (check-is-function func)
  (if (not (function? func))
      (error "not a function : " func)))

(define (get-function-name func)
  (check-is-function func)
  (cadr func))

(define (get-function-formals func)
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

(define (get-function-interfaces func)
  ;; list of captured identifiers of this function
  (let ((param-captures (map cadr (map cadr (get-function-formals func)))))
    (if (null? (get-function-return func))
        param-captures
        (cons (get-function-name func) param-captures))))

(define *uft-cp* #f) 

(define (uniqify-function-text func sfx cell-info initvars)

  ;;
  ;; the identifiers referenced by functions can be put in three classes
  ;;
  ;; 1. local variables
  ;; 2. interface variables (parameters and return value)
  ;; 3. captured globals
  ;;
  ;; in CSP, captured globals are just CAST constants and channel
  ;; identifiers all else are either locals or interface variables
  ;;
  ;; When uniqifying a function (in preparation for inlining), we
  ;; rename all the locals and interfaces.  Captured globals are kept
  ;; as originally named.
  ;;
  ;; This code doesn't generate the function prolog or epilog.
  ;; That needs to be done elsewhere.
  ;;
  
  (let*((intf-vars          (get-function-interfaces func))
        (body-vars          (find-referenced-vars (get-function-text func)))
        (body-var-captures  (set-intersection body-vars initvars))
        (body-chan-captures (set-intersection body-vars (get-port-ids cell-info)))
        (rename-ids         (set-diff (set-union body-vars
                                                 intf-vars)
                                      (set-union body-var-captures
                                                 body-chan-captures))))
    
  (dis "uniqify-function-text " (get-function-name func) " ====> " dnl)
  (dis "uniqify-function-text intf-vars          : " intf-vars dnl)
  (dis "uniqify-function-text body-vars          : " body-vars dnl)
  (dis "uniqify-function-text body-var-captures  : " body-var-captures dnl)
  (dis "uniqify-function-text body-chan-captures : " body-chan-captures dnl)
  (dis "uniqify-function-text rename-ids         : " rename-ids dnl)


  (let loop ((cp   rename-ids)
             (text (get-function-text func)))
    (if (null? cp)
        text
        (loop (cdr cp)
              (rename-id text
                         (car cp)
                         (symbol-append (car cp) sfx)))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; structs
;;

;; struct-decl is a definition of a struct type
(define (struct-decl? struct-decl)
  (and (pair? struct-decl)
       (eq? 'structure-decl (car struct-decl))))

(define (check-is-struct-decl struct-decl)
  (if (not (struct-decl? struct-decl))
      (error "not a struct-decl : " struct-decl)))

(define (get-struct-decl-name struct-decl)
  (check-is-struct-decl struct-decl)
  (cadr struct-decl))

(define (get-struct-decl-fields struct-decl)
  (check-is-struct-decl struct-decl)
  
  ;; struct decls are unconverted, so desugar declarators
  (map CspDeclarator.Lisp
       (map convert-declarator
            (apply append (caddr struct-decl)))))

(define (get-struct-decl-field-type struct-decl fld)
  (define (recurse p)
    (cond ((null? p) #f)
          ((equal? (cadar p) `(id ,fld)) (caddar p))
          (else (recurse (cdr p)))))

  (recurse (get-struct-decl-fields struct-decl)))

;; struct is a reference to a struct type (an instance declaration)
(define (struct? struct)
  (and (pair? struct)
       (eq? 'structure (car struct))))

(define (check-is-struct struct)
  (if (not (struct? struct))
      (error "not a struct : " struct)))

(define (get-struct-name struct)
  (check-is-struct struct)
  (caddr struct))

(define (get-struct-const struct)
  (check-is-struct struct)
  (cadr struct))

                         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (atom-hash atom)
  (if (null? atom) (error "atom-hash : null atom")
      (Atom.Hash atom)))

(define (make-object-hash-table elem-namer lst)
  (let ((res (make-hash-table 100 atom-hash)))
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

  (set! *the-funcs* (remove-duplicate-funcs
                      (merge-all get-funcs append data)))

  (set! *the-structs* (map cadr (merge-all get-structs append data)))

  (set! *the-inits*   (remove-duplicate-inits
                       (simplify-stmt
                        (desugar-stmt
                         (merge-all get-inits make-sequence data)))))

  (set! *the-initvars* (find-referenced-vars *the-inits*))

  (set! *the-func-tbl* (make-object-hash-table get-function-name *the-funcs*))
  (set! *the-struct-tbl* (make-object-hash-table get-struct-decl-name *the-structs*))

  (set! lisp0 (desugar-prog data))
  (set! lisp1 (simplify-stmt lisp0))

  'ok
    
  )

(load "visit.scm")

(define rdips #f)
(define rdi-vars #f)
(define rdi-inits #f)

(define (remove-duplicate-inits init-stmts)
  
  ;; I am doing it this way because the way the system is coded, init
  ;; statements can be inherited from multiple parents, because of
  ;; multiple inheritance of attribute cells.  These means that
  ;; attribute declarations and initializations may be inherited
  ;; through multiple paths and thereby appear multiply.
  
  (let ((vars  (make-symbol-set 100))
        (inits (make-designator-set 10000)))

    (set! rdi-vars vars)
    (set! rdi-inits inits)

    (define (previsitor s)
      (set! rdips s)
      (let ((st (get-stmt-type s)))
        (cond ((eq? st 'var1)
               (if (vars 'member? (get-var1-id s))
                   (begin
                     ;;(dis "delete " s dnl)
                     'delete)
                   (begin (vars 'insert! (get-var1-id s)) s)))
              ((eq? st 'assign)
               (if (inits 'member? (get-assign-designator s))
                   (begin
                     ;;(dis "delete " s dnl)
                     'delete)
                   (begin (inits 'insert! (get-assign-designator s)) s)))

              (else s))))

    (prepostvisit-stmt init-stmts
                       previsitor identity
                       identity   identity
                       identity   identity)

    ))

(define (remove-duplicate-funcs func-list)
  ;; funcs are duplicated (multiplicated) for the same reason that inits are
  (let ((names (make-symbol-set 100)))
    (filter
     (lambda(fd)(not (names 'insert! (get-function-name fd))))
     func-list)))

;; why aren't structs multiplicated?

(define (deep-copy x)
  (if (pair? x)
      (cons (deep-copy (car x)) (deep-copy (cdr x)))
      x))

(define (skip) )

(load "pp.scm")


(define (make-var name type)
  (cons name type))

(define (make-var-hash-table size)
  (make-hash-table size (lambda(var)(atom-hash (car var)))))

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
      (error "convert-range : not a range : " r)
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
                        (if (null? dw) '() (convert-expr dw))
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
  (if (not (and (eq? 'var1 (get-stmt-type s)) (eq? 'id (caadadr s))))
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

(define (check-assign-stmt s)
  (if (or (not (pair? s)) (not (eq? 'assign (car s))))
      (error "check-assign-stmt : not an assign : " s)))

(define *last-assign* #f)

(define (get-assign-designator s)
  (check-assign-stmt s)
  (cadr s))
  
(define (get-assign-id s)
  (set! *last-assign* s)
  (get-designator-id (get-assign-designator s)))

(define (get-assign-lhs a) (cadr a))

(define (get-assign-rhs a) (caddr a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-assignop-op x) (cadr x))
(define (get-assignop-lhs x) (caddr x))
(define (get-assignop-increment x) (cadddr x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *ddd* #f)

(define (get-designator-id x)
  ;; pull the identifier out of a designator
  (set! *ddd* x)
  (cond ((eq? 'id (car x)) (cadr x))
        ((eq? 'bits (car x)) (get-designator-id (cadr x)))
        ((eq? 'array-access (car x)) (get-designator-id (cadr x)))
        ((eq? 'member-access (car x)) (get-designator-id (cadr x)))
        (else #f)))

(define (hash-designator d)
  ;; hash a designator
  (cond ((eq? 'id (car d)) (atom-hash (cadr d)))
        ((eq? 'bits (car d)) (atom-hash (cadr d)))
        ((eq? 'array-access (car d))
         (+ (* 2 (hash-designator (cadr d)))
            (if (bigint? (caddr d)) (* 57 (BigInt.ToLongReal (caddr d))) 511)))
        ((eq? 'member-access (car d)) (* 3 (hash-designator (cadr d))))
        (else (error "hash-designator : not done yet"))))

(define (make-designator-hash-table size)
  (make-hash-table size hash-designator))

(define (make-designator-set size)
  (make-set (lambda()(make-designator-hash-table size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "simplify.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; visitors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the type visitor doesn't actually get at structure types
;; because structure types are defined in a separate space to the
;; text of the CSP process.

(define (array-type? t) (and (pair? t) (eq? 'array (car t))))

(define (get-array-extent      at)  (cadr at))
(define (get-array-elem-type   at)  (caddr at))

(define (make-array-type extent elem-type)
  `(array ,extent ,elem-type))


(define (prepostvisit-type t
                           stmt-previsit stmt-postvisit
                           expr-previsit expr-postvisit
                           type-previsit type-postvisit)

  (define (type tt) (prepostvisit-type tt
                                       stmt-previsit stmt-postvisit
                                       expr-previsit expr-postvisit
                                       type-previsit type-postvisit))


  (define (continue)
    (if (and (pair? t) (eq? 'array (car t)))
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

(define (get-stmt-type stmt)
  (if (eq? stmt 'skip)
      'skip
      (car stmt)))


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

(define (find-applys lisp)
  (define applys '())

  (define (expr-visitor x)
;;    (dis (stringify x) dnl)
    (if  (apply? x)
        
        (begin
;;          (dis "found " (stringify x) dnl)
          (set! applys (cons x applys)))
        x)
    )

  (visit-stmt lisp identity expr-visitor identity)
  applys
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *analyze-result* #f)
(define *unused-globals* #f)
(define *undeclared* #f)

;; we can update the inits to only those that we actually need
(define *filtered-inits* #f)
(define *filtered-initvars* #f)

(define (make-var1 decl)
  `(var1 ,decl)) 

(define (make-var1-decl sym type)
  ;; vars don't have a direction
  (make-var1 (make-decl sym type 'none)))

(define (make-decl sym type dir)
  `(decl1 (id ,sym) ,type ,dir))

(define *default-int-type*  '(integer #f #f () ()))

(define *single-bit-type*  `(integer #f #f ,*big-1* ()))

(define (make-default-var1 sym)
  ;; make a default (per CSP rules) variable declaration
  (make-var1 (make-decl sym *default-int-type* 'none)))

(define (predeclare prog undeclared)
   (cons 'sequence (append (map make-default-var1 undeclared) (list prog))))

(define *last-anal* #f)

(define (get-ports cell-info)
  (caddddr cell-info))

(define (get-port-ids cell-info)
  (map cadr (get-ports cell-info)))

(define (analyze-program prog cell-info initvars)
  (let* ((ports      (get-ports    cell-info))
         (portids    (get-port-ids cell-info))
         (textids    (find-referenced-vars prog))
         (globalids  (set-intersection textids initvars))
         (unused-globalids
                     (set-diff initvars globalids))
         (undeclared (set-diff
                      (find-undeclared-vars prog portids) globalids))
         (declnames  (find-declaration-vars prog))
         (multiples  (multi declnames))
         )

    (set! *last-anal* prog)

    ;; we should consider *the-inits* here.  We don't need to declare
    ;; anything that *the-inits* declares.
    (dis "portids    : " portids dnl)
    (dis "textids    : " textids dnl)
    (dis "globalids  : " globalids dnl)
    (dis "undeclared : " undeclared dnl) (set! *undeclared* undeclared)
    (dis "decls      : " declnames dnl)
    (dis "multiples  : " multiples dnl)

    (cond ((not (null? multiples))
           (dis dnl "uniqifying..." dnl dnl)
           (analyze-program (uniqify-stmt prog)
                            cell-info
                            initvars))

          ((not (null? undeclared))
           (dis dnl "un-undeclaring..." dnl dnl)
           (analyze-program (predeclare prog undeclared)
                            cell-info
                            initvars)
           )

          (else (set! *analyze-result* prog)
                (set! *unused-globals* unused-globalids)
                *analyze-result*)
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

(load "rename.scm")

(define (count-declarations of stmt)
  (count-in of (find-declaration-vars stmt)))

(define *stop* #f)

(define (uniqify-one stmt id tg)

  ;; this de-duplicates a multiply declared variable
  ;; by renaming all the instances to unique names
  
  (define (visitor s)
;;    (dis "here" dnl)
    (let ((num-decls (count-declarations id s)))
;;      (dis "num-decls of " id " " num-decls " : " (stringify s) dnl)
      (if (= num-decls 1)
          (cons 'cut
                (rename-id s id (symbol-append id '- (tg 'next))))
          s)))

  (if (< (count-declarations id stmt) 2)
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

(define (filter-unused lisp unused-ids)
  ;; filter out var1 and assign statements from a program
  (dis "filtering unused ids : " unused-ids dnl)
  (define (visitor s)
    (case (get-stmt-kw s)
      ((var1)   (if (member (get-var1-id   s) unused-ids) 'delete s))
      ((assign) (if (member (get-assign-id s) unused-ids) 'delete s))
      (else s))
    )
  (visit-stmt lisp visitor identity identity))

(define (filter-used lisp used-ids)
  ;; filter in var1 and assign statements from a program
  ;; for debugging mainly
  (dis "filtering used ids : " used-ids dnl)
  (define (visitor s)
    (case (get-stmt-kw s)
      ((var1)   (if (member (get-var1-id   s) used-ids) s 'delete))
      ((assign) (if (member (get-assign-id s) used-ids) s 'delete))
      (else s))
    )
  (visit-stmt lisp visitor identity identity))

              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-stmt-kw stmt)
  (if (eq? stmt 'skip)
      'skip
      (if (pair? stmt)

          (let ((res (car stmt)))
            (if (member res '(loop increment decrement var))
                (error "get-stmt-kw : need to desugar : " stmt))
            res)
            
          (error "get-stmt-kw : not a statement : " stmt))))


(define frame-kws
  ;; keywords that introduce a declaration block
  '(
;;    sequence parallel   ;; these do NOT introduce a declaration block
    do if nondet-if nondet-do
       parallel-loop sequential-loop
       loop-expression
       ))




(define *a*     #f)
(define *syms*  #f)

(define *x2* #f)
(define *x3* #f)

(define (ident? x)
  (and (pair? x) (eq? (car x) 'id)))

(define (make-ident sym) (list 'id sym))

(define (literal? x)
  (or (boolean? x)
      (bigint? x)
      (string? x)))

(define (literal-type literal)
  (cond ((boolean? literal) 'boolean)
        ((bigint? literal)  *default-int-type*)
        ((string? literal)  'string)
        (else (error "literal-type : not a literal : " literal))))

(define (simple-operand? x)
  (or (literal? x)(ident? x)))

(define *rhs* #f)

(define *lhs* #f)

(define (handle-assign-array-rhs a syms tg)
  (let loop ((p   (get-assign-rhs a))
             (res '())
             (seq '())
             )
    (cond ((and (eq? (car p) 'array-access)
                (simple-operand? (caddr p)))
           
           (loop (cdr p) (cons (car p) res))))))

(define *has-ass* #f)

;; type of an expression

(define *boolean-ops*
  ;; these create booleans all on their own
  '(probe not == != < > >= <= && == || ) ;; | )
  )

(define *integer-ops*
  ;; these create integers all on their own
  '(- / % * << >> **))

(define *string-ops*
  ;; these create strings all on their own
  '())

(define *polymorphic-ops*
  ;; these create objects whose types depend on operands
  '(+ & ^ | ) ;; | )
  )

(define (op-zero-elem op type)
  ;; zero element for iterated ops
  (case op
    ((&& ==)    #t)
    ((||)       #f)
    ((-)        *big-0*)
    ((* / **)   *big-1*)
    ((+)
     (cond ((eq? type 'string)    "")
           ((integer-type? type)   *big-0*)
           (else (error "???op-zero-elem of : " op " : for type : " type))))

    ((&)
     (cond ((eq? type 'boolean)   #t)
           ((integer-type? type)  *big-m1*)
           (else (error "???op-zero-elem of : " op " : for type : " type))))
    
    ((^ |) ;; |)
     (cond ((eq? type 'boolean)   #f)
           ((integer-type? type)   *big-0*)
           (else (error "???op-zero-elem of : " op " : for type : " type))))
    (else (error "???op-zero-elem of : " op))))
    
(define (integer-type? t)
  (and (pair? t) (eq? 'integer (car t))))

(define (integer-expr? x syms func-tbl struct-tbl)
  (or (bigint? x)
      (if (pair? x)
          (cond
           ((eq? 'id (car x)) (eq? 'integer
                                   (car (retrieve-defn (cadr x) syms))))
           ((member (car x) *integer-ops*) #t)
           ((member (car x) *polymorphic-ops*)
            (integer-type? (derive-type (cadr x) syms func-tbl struct-tbl)))
           (else #f)
           )
          #f
          )))

(define (boolean-expr? x syms func-tbl struct-tbl)
  (or (boolean? x)
      (if (pair? x)
          (cond
           ((eq? 'id (car x)) (eq? 'boolean (retrieve-defn (cadr x) syms)))
           ((member (car x) *boolean-ops*) #t)
           ((member (car x) *polymorphic-ops*)
            (eq? 'boolean (derive-type (cadr x) syms func-tbl struct-tbl)))
           (else #f)
           )
          #f
          )))

(define (string-expr? x syms func-tbl struct-tbl)
  (or (string? x)
      (if (pair? x)
          (cond
           ((eq? 'id (car x)) (eq? 'string (retrieve-defn (cadr x) syms)))
           ((member (car x) *string-ops*) #t)
           ((member (car x) *polymorphic-ops*)
            (eq? 'string (derive-type (cadr x) syms func-tbl struct-tbl)))
           (else #f)
           )
          #f
          )))

(define (array-access? x)
  (and (pair? x)(eq? 'array-access (car x))))

(define (peel-array t)
  (if (or (not (pair? t))
          (not (eq? 'array (car t))))
      (error "not an array type " t)
      (caddr t)))

(define (array-accessee x)
  (if (not (array-access? x))
      (error "array-accessee : not an array access : " x)
      (cadr x)))

(define (member-access? x)
  (and (pair? x)(eq? 'member-access (car x))))

(define (member-accessee x)
  (if (not (member-access? x))
      (error "member-accessee : not a member access : " x)
      (cadr x)))

(define (member-accesser x)
  (if (not (member-access? x))
      (error "member-accesser : not a member access : " x)
      (caddr x)))

(define s-x  #f)
(define s-bt #f)
(define s-sd #f)

(define (derive-type x syms func-tbl struct-tbl)
  (cond  ((ident? x) (retrieve-defn (cadr x) syms))
         ((bigint? x) *default-int-type*)
         ((boolean? x) 'boolean)
         ((string? x) 'string)
         ((pair? x)
          (cond ((member (car x) *string-ops*) 'string)
                ((member (car x) *integer-ops*) *default-int-type*)
                ((member (car x) *boolean-ops*) 'boolean)
                ((member (car x) *polymorphic-ops*)
                 (derive-type (cadr x) syms func-tbl struct-tbl))

                ((apply? x)
                 (dis "derive-type of apply : " x dnl)
                 
                 (let* ((fnam (get-apply-funcname x))
                        (fdef (func-tbl 'retrieve fnam))
                        (failed (eq? fdef '*hash-table-search-failed*))
                        (res
                         (if failed *default-int-type* (get-function-return fdef))))
                   
                   (dis "derive-type of apply : result : " res dnl)
                   res
                   )
                 
                 ;;          (error)
                 )


                ((loopex? x)
                 ;; a bit tricky: a loopex is the only expression that
                 ;; also introduces a new symbol into the environment!
                 (derive-type (construct-loopex-binop x)
                              (make-loopex-frame x syms)
                              func-tbl
                              struct-tbl))
                
                ((array-access? x)
                 (peel-array
                  (derive-type (array-accessee x) syms func-tbl struct-tbl)))
                
                ((member-access? x)
                 (set! s-x x)
                 (let* ((base-type (derive-type
                                    (member-accessee x) syms func-tbl struct-tbl))
                        (struct-def (struct-tbl 'retrieve (get-struct-name base-type)))
                        (struct-flds (get-struct-decl-fields struct-def))
                        (accesser   (member-accesser x))
                        )
                   
                   (set! s-bt base-type)
                   (set! s-sd struct-def)
                   
                   (dis "member-access   x           : " x dnl)
                   (dis "member-access   base-type   : " (stringify base-type) dnl)
                   (dis "member-access   struct-def  : " (stringify struct-def) dnl)
                   (dis "member-access   struct-flds : " (stringify struct-flds) dnl)
                   
                   (if (not (symbol? accesser))
                       (error "member-access : not an accesser : " accesser))
                   
                   (get-struct-decl-field-type struct-def accesser)
                   )
                 )
                
                ((bits? x)
                 (if (equal? (get-bits-min x) (get-bits-max x))
                     *single-bit-type*
                     *default-int-type*))
                
                (else (error "derive-type : don't know type of " x))
                )
          )
         (else (error "derive-type : don't know type of " x)))
  )

  

(define (old-derive-type x syms func-tbl struct-tbl)
  ;; return the type of x in the environment given by syms
  ;;  (set! ttt (cons (cons x syms) ttt))

  (dis x dnl)
                 
  (cond  ((ident? x) (retrieve-defn (cadr x) syms))

         ((integer-expr? x syms func-tbl struct-tbl) *default-int-type*)

         ((boolean-expr? x syms func-tbl struct-tbl) 'boolean)

         ((string-expr? x syms func-tbl struct-tbl) 'string)

         ((apply? x)
          (dis "derive-type of apply : " x dnl)
          
          (let* ((fnam (get-apply-funcname x))
                 (fdef (func-tbl 'retrieve fnam))
                 (failed (eq? fdef '*hash-table-search-failed*))
                 (res
                  (if failed *default-int-type* (get-function-return fdef))))

            (dis "derive-type of apply : result : " res dnl)
            res
            )

;;          (error)
          )

         ((loopex? x)
          ;; a bit tricky: a loopex is the only expression that
          ;; also introduces a new symbol into the environment!
          (derive-type (construct-loopex-binop x)
                       (make-loopex-frame x syms)
                       func-tbl
                       struct-tbl))

         ((array-access? x)
          (peel-array
           (derive-type (array-accessee x) syms func-tbl struct-tbl)))

         ((member-access? x)
          (set! s-x x)
          (let* ((base-type (derive-type
                             (member-accessee x) syms func-tbl struct-tbl))
                 (struct-def (struct-tbl 'retrieve (get-struct-name base-type)))
                 (struct-flds (get-struct-decl-fields struct-def))
                 (accesser   (member-accesser x))
                 )

            (set! s-bt base-type)
            (set! s-sd struct-def)
            
            (dis "member-access   x           : " x dnl)
            (dis "member-access   base-type   : " (stringify base-type) dnl)
            (dis "member-access   struct-def  : " (stringify struct-def) dnl)
            (dis "member-access   struct-flds : " (stringify struct-flds) dnl)

            (if (not (symbol? accesser))
                (error "member-access : not an accesser : " accesser))

            (get-struct-decl-field-type struct-def accesser)
            )
          )

         ((bits? x)
          (if (equal? (get-bits-min x) (get-bits-max x))
              *single-bit-type*
              *default-int-type*))

         (else (error "derive-type : don't know type of " x))))


(define sss '())
(define ttt '())

(define (bits? x)
  (and (pair? x) (eq? 'bits (car x))))

(define (get-bits-expr x) (cadr x))
(define (get-bits-min x) (caddr x))
(define (get-bits-max x) (cadddr x))

(define (loopex? x)
  (and (pair? x) (eq? 'loop-expression (car x))))

(define (get-loopex-dummy x) (cadr x))
(define (get-loopex-range x) (caddr x))
(define (get-loopex-op    x) (cadddr x))
(define (get-loopex-expr  x) (caddddr x))

(define (construct-loopex-binop x)
  ;; take a loop-expression and construct a dummy binary operation
  ;; ---> this will have the same type as the loop expression
  `(,(get-loopex-op x) ,(get-loopex-expr x)  ,(get-loopex-expr x) ))

(define (make-loopex-frame loopex syms)
  ;; construct a frame for use inside a loopex
  (let* ((new-frame (make-hash-table 1 atom-hash))
         (new-syms  (cons new-frame syms)))
    (define-var! new-syms (get-loopex-dummy loopex) *default-int-type*)
    new-syms)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-loop-dummy x) (cadr x))
(define (get-loop-range x) (caddr x))
(define (get-loop-stmt  x) (cadddr x))

(load "handle-assign.scm")


;; (reload)(loaddata! "expressions_p") (run-compiler! *the-text* *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *eval-s* '())
(define *eval-syms* '())

;; some trickery here

(define (if-wrap stmt)
  ;; wrap a statement in (if true) to make it a block
  `(if (#t ,stmt)))

(define ha-a #f)
(define ha-at #f)
(define ha-ft #f)

(load "clarify.scm")
(load "inline.scm")
(load "symtab.scm")
(load "sensitivity.scm")
(load "selection.scm")          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (find-stmt oftype stmt)
  (let ((res #f))

    (define (visitor s)
      (if (or (eq? oftype s)
              (and (pair? s) (eq? oftype (car s))))
          (begin
            (set! res s)
            'cut
            )
          s))
    (visit-stmt stmt visitor identity identity)
    res
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define xx #f)
(define yy #f)
(define zz #f)
(define tt #f)

(define (xxxx) (run-one "arraytypes_p"))

(define (run-one nm)
  (reload)
  (loaddata! nm)
  (run-compiler! *the-text* *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*)
  (find-applys text2)
  (set! xx (inline-evals *the-inits* text2 *the-func-tbl* *the-struct-tbl* *cellinfo*))
  (set! yy (inline-evals *the-inits* xx    *the-func-tbl* *the-struct-tbl* *cellinfo*))
  (set! zz (inline-evals *the-inits* yy    *the-func-tbl* *the-struct-tbl* *cellinfo*))
  (set! tt (simplify-stmt zz))
  tt
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *retrieve-defn-failed-sym* #f)
(define *retrieve-defn-failed-syms* #f)

(define (retrieve-defn sym syms)
  (let loop ((p syms))
    (if (null? p)
        (begin
          (set! *retrieve-defn-failed-sym* sym)
          (set! *retrieve-defn-failed-syms* syms)
          (error "retrieve-var : sym not found : " sym)
          )
        
        (let ((this-result ((car p) 'retrieve sym)))
          (if (eq? this-result '*hash-table-search-failed*)
              (loop (cdr p))
              this-result)))))

(define (define-var! syms sym type)
  ((car syms) 'add-entry! sym type)
;;  (dis "define-var! " sym " : " type " /// frame : " ((car syms) 'keys) dnl)
  )

(define (get-symbols syms)
  (apply append (map (lambda(tbl)(tbl 'keys)) syms)))

(define (remove-fake-assignment to stmt)
  ;; change an assignment to "to" back to an eval

  (dis "remove-fake-assignment " to dnl)
  (dis "remove-fake-assignment " (stringify stmt) dnl)

  (define (visitor s)

    (dis "visiting " s dnl)
    
    (if (and (eq? (get-stmt-type s) 'assign)
             (ident? (get-assign-lhs s))
             (equal? `(id ,to) (get-assign-lhs s)))

        (let ((res (make-eval (get-assign-rhs s))))
          (dis "replacing " s " -> " res dnl)
          res)
        
        s)
    )
      
  (visit-stmt stmt visitor identity identity)
  
  )

(define (make-assign lhs rhs) `(assign ,lhs ,rhs))

(define (make-eval rhs) `(eval ,rhs))

(define (global-simplify the-inits the-text func-tbl struct-tbl cell-inf)
  (simplify-stmt the-text))

(define (remove-assign-operate the-inits the-text func-tbl struct-tbl cell-info)

  (define (visitor s)
    (if (and (eq? 'assign-operate (get-stmt-type s))
             (check-side-effects (get-assignop-lhs s)))
        (make-assign (get-assignop-lhs s)
                     `(,(get-assignop-op s)
                       ,(get-assignop-lhs s)
                       ,(get-assignop-increment s)))
        s)
    )

  (visit-stmt the-text visitor identity identity)
  )

(define (check-side-effects expr)
  ;; check whether an expression can have side effects
  ;; returns #t if the expression *definitely not* has side effects
  ;; returns #f if the expression *may* have side effects
  (define result #t)

  (define (visitor x)
    (if (pair? x)
        (case (car x)
          ((apply call-intrinsic recv-expression) (set! result #f)))))

  (visit-expr expr identity visitor identity)
  result
  )
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *lx* #f)


(define (generate-loop-statement-from-expression lhs rhs
                                                 syms func-tbl struct-tbl)
  (set! *lx* rhs)
  (let* ((loop-op    (get-loopex-op rhs))
         (loop-idx   (get-loopex-dummy rhs))
         (loop-range (get-loopex-range rhs))
         (loop-expr  (get-loopex-expr rhs))
         (loop-frame (make-loopex-frame rhs syms))
         (loop-type  (derive-type loop-expr loop-frame func-tbl struct-tbl))
         (zero-elem  (op-zero-elem loop-op loop-type)))
    `(sequence
       (assign ,lhs
               ,zero-elem )
       (sequential-loop ,loop-idx
                        ,loop-range
                        (assign-operate ,loop-op ,lhs ,loop-expr)))))

(define (remove-loop-expression s syms tg func-tbl struct-tbl)

  (if (and (eq? 'assign (get-stmt-type s))
           (check-side-effects (get-assign-lhs s)))
      
      (let ( (lhs (get-assign-lhs s))
             (rhs (get-assign-rhs s))
             )
        (if (loopex? rhs)
            (generate-loop-statement-from-expression lhs rhs
                                                     syms func-tbl struct-tbl)
            s))
      s)
  )

(define (make-binop op lst)
  ;; given a list of expressions, make binary op across them
  (cond ((null? lst) (error "make-binop of empty list"))
        ((not (list? lst)) (error "make-binop : not a list : " lst))
        ((= 1 (length lst)) (car lst))
        ((= 2 (length lst)) `(,op ,(car lst) ,(cadr lst)))
        (else `(,op ,(car lst) ,(make-binop op (cdr lst)))))
  )

(define nsgs '())

(define (get-guarded-gcs guarded)
  (cdr guarded))

(define (simple-guard? guard)
  (or (eq? 'else guard)
      (simple-operand? guard)))

(define (nonsimple-guards? s)
  (set! nsgs (cons s nsgs))
  ;; an if or do that has non-simple guards

  (let* ((gcs    (get-guarded-gcs s))
         (guards (map car gcs))
         (simple (map simple-guard? guards))
         (all-simple (eval (apply and simple))))
    (not all-simple)
    )
  )

(define rdr #f)

(define (remove-do the-inits prog func-tbl struct-tbl cell-info)
  (let ((tg (make-name-generator "remove-do")))

    (define (visitor s)
;;      (dis "s = " (stringify s) dnl)
      (let ((kw (get-stmt-type s)))
        (if (and (member kw '(do nondet-do))
                 (nonsimple-guards? s))
            
            ;; get rid of it
            (let* ((gcs   (cdr s))
                   (ndone  (tg 'next 'not-done-))
                   
                   (vars  (map (lambda (gc) (tg 'next)) gcs))
                   (grds  (map car gcs))  ;; guards
                   (cmds  (map cadr gcs)) ;; commands

                   (decls
                    (map (lambda(nm)(make-var1-decl nm 'boolean)) vars))
                   
                   (assigns
                    (map (lambda(v x)(make-assign `(id ,v) x))
                         vars
                         grds))

                   (g-list  (map make-ident vars))
                   
                   (or-expr (make-binop '| g-list)) ; |))

                   (ndone-decl  (make-var1-decl ndone 'boolean))
                   (ndone-assign (make-assign (make-ident ndone)
                                             or-expr))

                   (if-grds
                    (cons
                     (list `(not ,(make-ident ndone)) 'skip)
                     (map list g-list cmds)))

                   (the-if (cons 'if if-grds))

                   (the-body
                    `(sequence ,@decls ,@assigns ,ndone-assign ,the-if))

                   (the-loop
                    `(do (,(make-ident ndone) ,the-body)))

                   (res
                    `(sequence ,ndone-decl ,the-loop))
                   )

              ;; we could insert a check for two guards for
              ;; deterministic dos.

              (dis "remove-do : " (stringify s) dnl)
              (dis "remove-do : gcs   : " (stringify gcs) dnl)
              (dis "remove-do : ndone : " (stringify ndone) dnl)
              (dis "remove-do : vars  : " (stringify vars) dnl)
              (dis "remove-do : grds  : " (stringify grds) dnl)
              (dis "remove-do : cmds  : " (stringify cmds) dnl dnl)
              (dis "remove-do : ndone-assign : " (stringify ndone-assign) dnl)
              (dis "remove-do : the-if       : " (stringify the-if) dnl)
              (dis "remove-do : the-body     : " (stringify the-body) dnl)

              (dis "remove-do : the-loop     : " (stringify the-loop) dnl)

              (set! rdr res)

;;              (error)
              
              res
              )
            s
            ) ;; fi
        ) ;; tel
      )

    (visit-stmt prog visitor identity identity)
    )
  )

(define (simplify-if the-inits prog func-tbl struct-tbl cell-info)

  (define (visitor s)
    (if (member (get-stmt-type s) '(if nondet-if))
        (make-selection-implementation s func-tbl cell-info)
        s
        )
    )
  
  (visit-stmt prog visitor identity identity)
  )

(define (handle-eval s syms tg func-tbl struct-tbl)

  (dis "handle-eval " s dnl)

  (let* ((fake-var (tg 'next))
         (fake-assign (make-assign `(id ,fake-var) (cadr s)))
         (full-seq
          (handle-assign-rhs fake-assign syms tg func-tbl struct-tbl))
         (res (remove-fake-assignment fake-var full-seq)))

    (dis "handle-eval   s = " s dnl
         "handle-eval res = " res dnl)
         
    res
    )
  )

(load "dead.scm")  

(define the-passes (list
                    (list 'assign handle-access-assign)
                    (list 'recv   handle-access-recv)
                    (list 'send   handle-access-recv)
                    (list 'assign handle-assign-rhs)
                    (list 'eval   handle-eval)
                    (list 'global inline-evals)
                    (list 'global global-simplify)
                    (list 'global remove-assign-operate)
                    (list 'global remove-do)
                    (list 'global simplify-if)
                    (list 'assign remove-loop-expression)))

(define *the-pass-results* '())

(define (run-compiler! the-text cell-info the-inits func-tbl struct-tbl)

  (define syms '())

  (set! *the-pass-results* '())

  (define tg (make-name-generator "temp"))
  
  (set! *a*    '())
  (set! *syms* '())

  (dis dnl "=========  START  =========" dnl dnl) 
  
  (define initvars (find-referenced-vars the-inits))

  (dis "analyze program : " dnl)

  (define lisp (analyze-program the-text cell-info initvars))

  ;; (dead-code) went here

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define (loop2 prog passes)
    (cond ((null? passes) prog)

          (else
           (loop2
            (let ((the-pass (car passes)))
              (run-pass the-pass prog cell-info the-inits func-tbl struct-tbl)
              )
            (cdr passes)
            ))))

  (let loop ((cur-prog lisp)
             (prev-prog '()))
    (if (equal? cur-prog prev-prog)
        (begin
          (dis
           "******************************************************************************" dnl)
          (dis
           "********************                                      ********************" dnl)
          (dis
           "********************  COMPILER HAS REACHED A FIXED POINT  ********************" dnl)
          (dis
           "********************                                      ********************" dnl)
          (dis
           "********************       TRANSFORMATIONS COMPLETE       ********************" dnl)
          (dis
           "********************                                      ********************" dnl)
          (dis
           "******************************************************************************" dnl)
               (set! text2 cur-prog)
               'text2)
        
        (begin  ;; program not equal, must continue
          (dis "========= PROGRAM CHANGED" dnl)
          (loop (loop2 cur-prog the-passes) cur-prog))))
  )

(define (run-pass the-pass prog cell-info the-inits func-tbl struct-tbl)
  (dis "========= COMPILER PASS : " the-pass " ===========" dnl)

  (define syms '())

  (define tg (make-name-generator "temp"))
  
  (define (enter-frame!)
    (set! syms (cons (make-hash-table 100 atom-hash) syms))
;;    (dis "enter-frame! " (length syms) dnl)
    )

  (define (exit-frame!)
    ;;    (dis "exit-frame! " (length syms) " " (map (lambda(tbl)(tbl 'keys)) syms) dnl)

;;    (dis "exit-frame! " (length syms) dnl)
    (set! syms (cdr syms))
    )

  (define (stmt-pre0 s)
;;    (dis "pre stmt  : " (stringify s) dnl)
    (stmt-check-enter s)

    (case (get-stmt-kw s)
      ((assign) (handle-assign-rhs s syms tg func-tbl struct-tbl))

      ((eval) ;; this is a function call, we make it a fake assignment.
       (dis "eval!" dnl)
       
       (dis "===== stmt-pre0 start " (stringify s) dnl)
       
       (let* ((fake-var (tg 'next))
              (fake-assign (make-assign `(id ,fake-var) (cadr s)))
              (full-seq   (handle-assign-rhs fake-assign syms tg func-tbl struct-tbl))
              (res (remove-fake-assignment fake-var full-seq)))

         (dis "===== stmt-pre0 fake  " (stringify fake-assign) dnl)
         (dis "===== stmt-pre0 full  " (stringify full-seq) dnl)
         (dis "===== stmt-pre0 done  " (stringify res) dnl)
         res
       ))
         

      (else s)
      )
    )

  (define (make-pre stmt-type pass)
    (lambda(stmt)
      ;; this takes a "pass" and wraps it up so the symbol table is maintained
      (stmt-check-enter stmt)
      (if (eq? stmt-type (get-stmt-kw stmt))
          (pass stmt syms tg func-tbl struct-tbl)
          stmt)))
  
  (define (stmt-check-enter s)
    (if (member (get-stmt-kw s) frame-kws) (enter-frame!))
    (case (get-stmt-kw s)
      ((var1)
       (define-var! syms (get-var1-id s) (get-var1-type s))
       )

      ((loop-expression)
       (define-var! syms (get-loopex-dummy s) *default-int-type*)
       )

      ((parallel-loop sequential-loop)
       (define-var! syms (get-loop-dummy s) *default-int-type*))
         
      )
    )
  
  (define (stmt-post s)
;;    (dis "post stmt : " (get-stmt-kw s) dnl)
    (if (member (get-stmt-kw s) frame-kws) (exit-frame!))
    s
    )

  (let ((pass-result
         (cond ((member (car the-pass) '(assign eval recv send))
                
                (enter-frame!) ;; global frame
                
                ;; we should be able to save the globals from earlier...
                (dis "visiting initializations..." dnl)
                (prepostvisit-stmt 
                 the-inits
                 stmt-pre0 stmt-post
                 identity identity
                 identity identity)
                
                (dis "visiting program text..." dnl)
                
                (let ((res
                       (prepostvisit-stmt prog
                                          (make-pre
                                           (car the-pass)
                                           (cadr the-pass)) stmt-post
                                           identity                identity
                                           identity                identity)))
                  (exit-frame!)
                  res))
               ((eq? 'global (car the-pass))
                ((cadr the-pass) the-inits prog func-tbl struct-tbl cell-info)
                )
               
               (else (error "unknown pass type " (car the-pass)))
               )
         )
        )

    (set! *the-pass-results*
          (cons
           (list the-pass pass-result)
           *the-pass-results*))
                              
    pass-result
    )
  )

(define (find-pass proc)

  (define (recurse p)
    (cond ((null? p) #f)
          ((equal? proc (cadar p)) (car p))
          (else (recurse (cdr p)))))
  (recurse the-passes)
  )

(define (manual-pass proc prog)

  ;; use this to run a single pass on the program
  ;; 
  ;; e.g., (manual-pass inline-evals <program>)
  
  (let* ((pass (find-pass proc))
         (res (run-pass
               pass prog *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*)))

    (dis "============================  " (if (equal? prog res)
         "no change"
         "PROGRAM CHANGED"
         )
         dnl)
    
    res
    )
  )

(define (compile!)
  (run-compiler! *the-text*
                 *cellinfo*
                 *the-inits*
                 *the-func-tbl*
                 *the-struct-tbl*))


(define (mn) (make-name-generator "t"))

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

(define (make-referenceable x temp-generator) ;; BROKEN
  (if (directly-referenceable? x)
      x
      (let ((new-var (temp-generator 'next)))
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
    ((+ / % * == != < > >= <= & && ^ == << >> ** | || ; |
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

(define (apply? x)
  (and (pair? x) (eq? 'apply (car x))))

(define (call-intrinsic? x)
  (and (pair? x) (eq? 'call-intrinsic (car x))))

(define (get-apply-funcname x)
  (cadadr x))

(define (sequentialize-one s tg)   ;; BROKEN
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
               

(define (sequentialize-stmt s)   ;; BROKEN
  (let ((tg (make-name-generator "t")))
    (visit-stmt s
                sequentialize-one
                identity
                identity)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *name-counter* 0) ;; guarantees all the names are unique

(define (make-name-generator rootstring)
  ;;
  ;; return a temporary-name-generator
  ;; a procedure of zero arguments, which generates incrementing temp names
  ;;
  (let ((root                rootstring)
        (my-last-name        #f)
        )


    (define (make-symbol n)
      (string->symbol(string-append root (stringify n))))

    (lambda(cmd . x)
      (case cmd
        ((names)
         (let loop ((i (- *name-counter* 1))
                    (res '()))
           (if (= i -1)
               res
               (loop (- i 1) (cons (make-symbol i) res))))
               
         )

        ((last) my-last-name)
        
        ((next)
         (let* ((z (make-symbol *name-counter*))
                (res (if (null? x) z (symbol-append (car x) z))))
           (set! my-last-name res)
           (set! *name-counter* (+ *name-counter* 1))
           res))))))

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

            ((assign-operate) 
             (CspAst.AssignOperateStmt
              (convert-expr (cadr args))
              (convert-expr (caddr args))
              (convert-binop (car args))
              ))

            ((loop) ;; this is from the Java, convert to parallel or sequential
             (let ((idxvar (car args))
                   (range  (cadr args))
                   (sep-id (caddr args))
                   (stmt   (cadddr args)))

               ((cond ((BigInt.Equal sep-id *big-0*) CspAst.SequentialLoop)
                      ((BigInt.Equal sep-id *big-1*) CspAst.ParallelLoop)
                      (else (error "convert-stmt : unknown loop type " sep-id)))
                idxvar
                (convert-range range)
                (convert-stmt stmt s))))

            ((sequential-loop parallel-loop) ;; what we will compile
             (let ((idxvar (car args))
                   (range  (cadr args))
                   (stmt   (caddr args)))
               ((case kw
                  ((sequential-loop) CspAst.SequentialLoop)
                  ((parallel-loop) CspAst.ParallelLoop)
                  (else (error)))
                idxvar
                (convert-range range)
                (convert-stmt stmt s))
               ))

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

            ((recv)
             ;; null receive is OK (just completes the handshake)
             (CspAst.RecvStmt (convert-expr (car args))
                              (convert-expr-or-null (cadr args))))

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

(define (convert-binop sym)
  (case sym
    ((-) 'Sub) ;; unary op has a different name...
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
    ((|) 'Or) ;;|)
    ((||) 'CondOr)
    ((^) 'Xor)
    ((<<) 'SHL)
    ((>>) 'SHR)
    ((**) 'Pow)
    (else (error " BinExpr " (car x)))))

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

           ((apply call-intrinsic)
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


           ((loop-expression)
            (CspAst.LoopExpr (cadr x)
                             (convert-range (caddr x))
                             (convert-binop (cadddr x))
                             (convert-expr (caddddr x))
                             ))

           ((recv-expression)
            (CspAst.RecvExpr (convert-expr (cadr x))))
           
           ((peek)
            (CspAst.PeekExpr (convert-expr (cadr x))))

           ((bits)
            (let ((base (convert-expr (cadr x)))
                  (min  (convert-expr-or-null (caddr x)))
                  (max  (convert-expr (cadddr x))))
              (CspAst.BitRangeExpr base (if (null? min) max min) max)
              )
            )
           
           ((not) (CspAst.UnaExpr 'Not (convert-expr (cadr x))))

           ((-)
            ;; - is special

            (if (null? (cddr x))
                (CspAst.UnaExpr 'Neg (convert-expr (cadr x)))
                (CspAst.BinExpr 'Sub (convert-expr (cadr x))
                                     (convert-expr (caddr x)))))

           ((+ / % * == != < > >= <= & && | || ^ == << >> **) ;; |
            (CspAst.BinExpr
             (convert-binop (car x))
             (convert-expr (cadr x))
             (convert-expr (caddr x))))
           
           (else (error "convert-expr : unknown keyword " (car x) " : " x ))
           )
         )
        
        (else (error "dunno that type " x) )
        )
  )
  
(define (convert-expr-or-null x)
  (if (null? x) '() (convert-expr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (desugar-stmt stmt)
  ((obj-method-wrap (convert-stmt stmt) 'CspSyntax.T) 'lisp))

(define (desugar-prog p)
  (if (not (and (pair? p) (eq? (car p) 'csp)))
      (error (string-append "Not a CSP program : " p)))

      (desugar-stmt (close-text p))
  )

(define (reload) (load "cspc.scm"))

(define testx '
  (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ "mesh_forward" "a") "b") "a") "c") "b") "a") "c") "b") "a") "c")"b") "a") "c")
  )
