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
(require-modules "fold.scm")


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

(load "higher-order.scm")
(load "name-generator.scm")
(load "set-ops.scm")
(load "pp.scm")

(load "bigint.scm")
(load "loops.scm")
(load "visit.scm")
(load "simplify.scm")
(load "expr.scm")
(load "analyze.scm")
(load "rename.scm")
(load "expr.scm")
(load "type.scm")
(load "bits.scm")
(load "handle-assign.scm")
(load "clarify.scm")
(load "inline.scm")
(load "symtab.scm")
(load "sensitivity.scm")
(load "selection.scm")          
(load "do.scm")
(load "dead.scm")  
(load "fold-constants.scm")
(load "vars.scm")
(load "blocking.scm")
(load "globals.scm")
(load "convert.scm")
(load "ports.scm")
(load "codegen.scm")
(load "codegen-m3.scm")

(define *reload-name*   "cspc.scm")

(define (reload) (load *reload-name*))

(define special-functions     ;; these are special functions per Harry
  '(string
    print
    assert
    cover   // what's this?
    pack
    unpack 
    ))

(define nonblocking-intrinsics '(string print pack unpack))

(define (caddddr x) (car (cddddr x)))
(define (cadaddr x) (car (cdaddr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (loadfile reader fn)

  (let* ((p   (open-input-file fn))
         (res (reader    p)))
    (close-input-port p)
    res
    )
  )

(define (load-csp fn)
  (loadfile read-big-int fn)
  )

(define (read-importlist fn)
  (loadfile read fn)
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

(define (uniquify-function-text func sfx cell-info initvars)

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
  ;; When uniquifying a function (in preparation for inlining), we
  ;; rename all the locals and interfaces.  Captured globals are kept
  ;; as originally named.
  ;;
  ;; This code doesn't generate the function prolog or epilog.
  ;; That needs to be done elsewhere.
  ;;
  
  (let*((intf-vars          (get-function-interfaces func))
        (function-text      (get-function-text func))
        (body-vars          (find-referenced-vars function-text))
        (body-dummies       (get-all-dummies function-text))
        (body-var-captures  (set-intersection body-vars initvars))
        (body-chan-captures (set-intersection body-vars (get-port-ids cell-info)))
        (rename-ids         (set-diff (set-union body-vars
                                                 intf-vars)
                                      (set-union body-var-captures
                                                 body-chan-captures))))
    
  (dis "uniquify-function-text " (get-function-name func) " ====> " dnl)
  (dis "uniquify-function-text intf-vars          : " intf-vars dnl)
  (dis "uniquify-function-text body-vars          : " body-vars dnl)
  (dis "uniquify-function-text body-var-captures  : " body-var-captures dnl)
  (dis "uniquify-function-text body-chan-captures : " body-chan-captures dnl)
  (dis "uniquify-function-text rename-ids         : " rename-ids dnl)


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


(define (loaddata! . nma)
  ;; this ends up loading the program into *the-text*

  (let  ((nm
          (if (null? nma)
              *the-prog-name*
              (begin 
              
                (set! *the-prog-name* (car nma))
                (car nma)
                )
              )
          )
         )
  (begin
    (dis dnl "=========  LOADING PARSE TREE FROM " nm ".scm ..." dnl)

    (set! *cell*  (load-csp (string-append nm ".scm")))
    (set! *the-proc-type-name* (car *cell*))
    
    (set! *data* (cadr *cell*))       ;; the CSP code itself
    (set! *cellinfo* (caddr *cell*))  ;; the CAST ports

    (dis "=========  PARSE TREE LOADED SUCCESSFULLY " dnl dnl)

    (dis "=========  " *the-proc-type-name* dnl dnl)
    
    (switch-proc! *data*)
    )

  (dis   dnl go-grn-bold-term
         "=========  INITIAL SETUP COMPLETE : run (compile!) when ready" dnl
         reset-term dnl)
  )
  '*the-text*
  )

(define xxx #f)

(define *ascii-space* (car (string->list " ")))

(define (pad w . str)
  (Fmt.Pad (apply string-append str) w *ascii-space* 'Left))

(define (padr w . str)
  (Fmt.Pad (apply string-append str) w *ascii-space* 'Right))


(define (print-atomsize sym)
  (dis (pad  23 "Defined " sym) " : " )
  (dis (padr 10 (stringify (count-atoms (eval sym)))) " atoms" dnl))

(define (switch-proc! data)

  (print-atomsize '*data*)
  
  (map
   (lambda (sym)
     (let ((entry
            (apply (eval (symbol-append 'get- sym)) (list data))))
       (dis sym " ")
       (dis (count-atoms entry) " atoms" dnl)
       (eval (list 'set! sym 'entry))
       ))
   '(
;;     refparents
     )
   )
  
  (map
   (lambda (sym)
     (dis (string-append "(length " (symbol->string sym) ") = ")
          (eval (list 'length sym)) dnl))
   '(
     ;;     funcs structs refparents declparents inits text
     )
   )

  (set! *the-text* (simplify-stmt (desugar-stmt (close-text data))))

  (print-atomsize '*the-text*)
  

  (set! *the-funcs* (remove-duplicate-funcs
                      (merge-all get-funcs append data)))

  (print-atomsize '*the-funcs*)

  (set! *the-structs* (map cadr (merge-all get-structs append data)))

  (print-atomsize '*the-structs*)

  (set! *the-inits*   (remove-duplicate-inits
                       (simplify-stmt
                        (desugar-stmt
                         (merge-all get-inits make-sequence data)))))

  (print-atomsize '*the-inits*)

  (set! *the-initvars* (find-referenced-vars *the-inits*))

  (set! *the-func-tbl* (make-object-hash-table get-function-name *the-funcs*))

  (dis (*the-func-tbl* 'size) " functions loaded" dnl)
  
  (set! *the-struct-tbl* (make-object-hash-table get-struct-decl-name *the-structs*))

  (dis (*the-struct-tbl* 'size) " struct types loaded" dnl)

;;  (set! lisp0 (desugar-prog data))
;;  (set! lisp1 (simplify-stmt lisp0))

  'ok
    
  )

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

(define (count-atoms p)
  (cond ((null? p) 0)
        ((pair? p) (+ (count-atoms (car p)) (count-atoms (cdr p))))
        (else 1)))

(define (skip) )


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

(define (get-send-lhs a) (cadr a))
(define (get-send-rhs a) (caddr a))

(define (make-send lhs rhs) (list 'send lhs rhs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-assignop-op x) (cadr x))
(define (get-assignop-lhs x) (caddr x))
(define (get-assignop-increment x) (cadddr x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *ddd* #f)

(define (designator? x)
  ;; supported designators
  (or (ident? x) (bits? x) (array-access? x) (member-access? x)))

(define (get-designator-id x)
  ;; pull the identifier of the variable being modified out of a designator
  ;; can only be called on a designator
  (set! *ddd* x)
  (cond ((ident?         x)  (cadr x))
        ((bits?          x)  (get-designator-id (cadr x)))
        ((array-access?  x)  (get-designator-id (cadr x)))
        ((member-access? x)  (get-designator-id (cadr x)))
        (else (error "get-designator-id : not a designator : " x))))

(define (get-designator-depend-ids x)
  ;; put out all the identifiers needed to construct the designator,
  ;; other than the lvalue
  (cond ((ident?         x)  '())
        ((bits?          x)  (uniq eq?
                              (append
                               (get-designator-all-ids (caddr x))
                               (get-designator-all-ids (cadddr x)))))
         
        ((array-access?  x)  (get-designator-all-ids (caddr x)))
        ((member-access? x)  '())
        (else '())))

(define (get-designator-all-ids x) 
  ;; pull out all the identifiers used in a designator
  (uniq eq? (find-expr-ids x)))
  
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

(define vs #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-stmt-type stmt)
  (if (pair? stmt)
      (car stmt)
      'skip))

(define (find-stmt-ids lisp)
  ;; returns ids represented by program as a set-list
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

(define (find-expr-ids expr)
  (define ids '())
  
  (define (expr-visitor x)
;;     (dis "expr : " x dnl)

     (if (and (pair? x) (eq? 'id (car x)))
         (if (not (member? (cadr x) ids))
             (set! ids (cons (cadr x) ids))))
     x
  )

  (visit-expr expr identity expr-visitor identity)
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

(define (make-var1 decl) `(var1 ,decl)) 

(define (make-var1-decl sym type)
  ;; vars don't have a direction
;;  (dis "make-var1-decl " type dnl)
  (make-var1 (make-decl sym type 'none)))

(define (make-decl sym type dir)
  `(decl1 (id ,sym) ,type ,dir))

(define *default-int-type*  '(integer #f #f () ()))
(define *const-int-type*  '(integer #t #f () ()))

(define *single-bit-type*  `(integer #f #f ,*big1* ()))

(define (make-default-var1 sym)
  ;; make a default (per CSP rules) variable declaration
  (make-var1 (make-decl sym *default-int-type* 'none)))

(define (predeclare prog undeclared)
   (cons 'sequence (append (map make-default-var1 undeclared) (list prog))))

(define (get-ports cell-info)
  (caddddr cell-info))

(define (get-port-id pdef) (cadr pdef))
  
(define (get-port-ids cell-info)
  (map get-port-id (get-ports cell-info)))

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


(define (find-loop-indices lisp)
  (define idxs '())

  (define (stmt-visitor s)
    (cond ((member (get-stmt-type s) '(sequential-loop parallel-loop))
           (set! idxs (cons (cadr s) idxs)))

          ((member (get-stmt-type s) '(loop-expression))
           (set! idxs (cons (get-loopex-dummy s) idxs)))

          )
    )

  (visit-stmt lisp stmt-visitor identity identity)

  idxs
  )

(define *stop* #f)

(define uniquify-tg (make-name-generator "uniquify-temp"))

(define (uniquify-one stmt id tg)

  ;; this de-duplicates a multiply declared variable
  ;; by renaming all the instances to unique names
  
  (define (visitor s)
;;    (dis "here" dnl)
    (let ((num-decls (count-declarations id s)))
;;      (dis "num-decls of " id " " num-decls " : " (stringify s) dnl)
      (if (= num-decls 1)
          (cons 'cut
                (rename-id s id (symbol-append id '- (uniquify-tg 'next))))
          s)))

  (if (< (count-declarations id stmt) 2)
      (error "not defined enough times : " id " : in : " stmt))

  (prepostvisit-stmt stmt
                     visitor  identity
                     identity identity
                     identity identity)
)
                     
(define (uniquify-stmt stmt)
  (let ((tg    (make-name-generator "uniq"))
        (names (multi (find-declaration-vars stmt))))
    (let loop ((p names)
               (s stmt))
      (if (null? p)
          s
          (begin
;;            (dis "uniquifying " (car p) dnl)
            (loop (cdr p) (uniquify-one s (car p) tg)))))
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



(define sss '())
(define ttt '())


;; (reload)(loaddata! "expressions_p") (run-compiler *the-text* *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl cell-info*)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (find-stmts oftype stmt)
  (let ((res '()))

    (define (visitor s)
      (if (or (eq? oftype s)
              (and (pair? s) (eq? oftype (car s))))
          (begin
            (set! res (cons s res))
            s
            )
          s))
    (visit-stmt stmt visitor identity identity)
    (reverse res)
    )
  )

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
  (run-compiler *the-text* *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*)
  (find-applys text2)
  (set! xx (inline-evals *the-inits* text2 *the-func-tbl* *the-struct-tbl* *cellinfo*))
  (set! yy (inline-evals *the-inits* xx    *the-func-tbl* *the-struct-tbl* *cellinfo*))
  (set! zz (inline-evals *the-inits* yy    *the-func-tbl* *the-struct-tbl* *cellinfo*))
  (set! tt (simplify-stmt zz))
  tt
  )

(define (do-compile-m3! nm)
  (loaddata! nm)
  (compile!)
  (do-m3!)
  'ok
  )

(define (done-banner)
  (dis go-grn-bold-term (run-command "banner **DONE**") reset-term dnl))

(define (compile-m3! nm)
  (do-compile-m3! nm)
  (done-banner)
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
          (error "retrieve-defn : sym not found : " sym)
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

;;    (dis "visiting " s dnl)
    
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
  (fixpoint-simplify-stmt the-text))

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
          ((apply call-intrinsic recv-expression)

           ;; note that peek doesn't have side effects, but it may block

           (set! result #f)))))

  (visit-expr expr identity visitor identity)
  result
  )
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (simplify-if the-inits prog func-tbl struct-tbl cell-info)

  (define (visitor s)
    (if (member (get-stmt-type s) '(if nondet-if))
        (make-selection-implementation s func-tbl cell-info)
        s
        )
    )
  
  (visit-stmt prog visitor identity identity)
  )

(define handle-eval-tg (make-name-generator "handle-eval-temp"))

(define (handle-eval s syms vals tg func-tbl struct-tbl cell-info)

  (dis "handle-eval " s dnl)

  (let* ((fake-var (handle-eval-tg 'next))
         (fake-assign (make-assign `(id ,fake-var) (cadr s)))
         (full-seq
          (handle-assign-rhs fake-assign syms vals tg func-tbl struct-tbl cell-info))
         (res (remove-fake-assignment fake-var full-seq)))

    (dis "handle-eval   s = " s dnl
         "handle-eval res = " res dnl)
         
    res
    )
  )

(define (display-success-0)
  (dis go-grn-bold-term
       "******************************************************************************" dnl)
  (dis
   "********************                                      ********************" dnl)
  (dis
   "********************   INITIAL TRANSFORMATIONS COMPLETE   ********************" dnl)
  (dis
   "********************                                      ********************" dnl)
  (dis
   "******************************************************************************" reset-term dnl)
  )

(define (display-success-1)
  (dis go-grn-bold-term
       "******************************************************************************" dnl)
  (dis
   "********************                                      ********************" dnl)
  (dis
   "********************  COMPILER HAS REACHED A FIXED POINT  ********************" dnl)
  (dis
   "********************                                      ********************" dnl)
  (dis
   "********************  !!!!  SYNTAX TRANSFORMATIONS  !!!!  ********************" dnl)
  (dis
   "********************  !!!!        COMPLETE          !!!!  ********************" dnl)
  (dis
   "********************                                      ********************" dnl)
  (dis
   "******************************************************************************" reset-term dnl)
  )

(define (display-success-2)
  (dis go-grn-bold-term
       "******************************************************************************" dnl)
  (dis
   "*******************                                         ******************" dnl)
  (dis
   "*******************  CONSTANT BINDING AND FOLDING COMPLETE  ******************" dnl)
  (dis
   "*******************            (PHASE ONE)                  ******************" dnl)
  (dis
   "*******************                                         ******************" dnl)
  (dis
   "******************************************************************************" reset-term dnl)
  )

(define *the-pass-results* '())

(define (run-compiler the-passes the-text cell-info the-inits func-tbl struct-tbl)

  ;; n.b. that we can't introduce uniquify-loop-dummies inside here
  ;; because that (and only that) transformation unconditionally changes
  ;; the program.  Maybe we can fix it, but for now our solution is to
  ;; only run that transformation at the outside of the program.
  
  (define syms '())

  (set! *the-pass-results* '())

  (define tg (make-name-generator "passes-temp"))
  
  (set! *a*    '())
  (set! *syms* '())

  (dis dnl "=========  START  =========" dnl dnl) 
  
  (define initvars (find-referenced-vars the-inits))
  ;; should not be repeated over and over... not when we don't change the-inits.

  (dis "analyze program : " dnl)

  (define lisp (analyze-program the-text cell-info initvars))

  ;; (dead-code) went here

    (display-success-0)
  

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
          (display-success-1)
          cur-prog
          )
        
        (begin  ;; program not equal, must continue
          (dis *program-changed* dnl)
          (loop (loop2 cur-prog the-passes) cur-prog))))
  )

(define (run-pass the-pass prog cell-info the-inits func-tbl struct-tbl)
  (dis "========= COMPILER PASS : " the-pass " ===========" dnl)

  (define syms '())
  (define vals '())

  (define tg (make-name-generator "run-pass-temp"))
  
  (define (enter-frame!)
    (set! syms (cons (make-hash-table 100 atom-hash) syms))
    (set! vals (cons (make-hash-table 100 atom-hash) vals))
;;    (dis "enter-frame! " (length syms) dnl)
    )

  (define (exit-frame!)
    ;;    (dis "exit-frame! " (length syms) " " (map (lambda(tbl)(tbl 'keys)) syms) dnl)

;;    (dis "exit-frame! " (length syms) dnl)
    (set! syms (cdr syms))
    (set! vals (cdr vals))
    )

  (define (stmt-pre0 s)
;;    (dis "pre stmt  : " (stringify s) dnl)
    (stmt-check-enter s)

    (case (get-stmt-kw s)
      ((assign) (handle-assign-rhs s syms vals tg func-tbl struct-tbl cell-info))

      ((eval) ;; this is a function call, we make it a fake assignment.
;;       (dis "eval!" dnl)
       
       (dis "===== stmt-pre0 start " (stringify s) dnl)
       
       (let* ((fake-var (tg 'next))
              (fake-assign (make-assign `(id ,fake-var) (cadr s)))
              (full-seq   (handle-assign-rhs fake-assign syms vals tg func-tbl struct-tbl cell-info))
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
;;      (dis "here!" dnl)
      (stmt-check-enter stmt)
      (if (or (eq? stmt-type '*)
              (eq? stmt-type (get-stmt-kw stmt)))

          (begin
;;            (dis "make-pre stmt-type " stmt-type dnl)
;;            (dis "make-pre stmt      " stmt dnl)
            
            (pass stmt syms vals tg func-tbl struct-tbl cell-info)
            )
          
          stmt)))
  
  (define (stmt-check-enter s)
;;    (dis go-red-bold-term "stmt-check-enter " (if (pair? s) (car s) s) reset-term dnl)
    (if (member (get-stmt-kw s) frame-kws) (enter-frame!))
    (case (get-stmt-kw s)
      ((var1)
;;       (dis "define-var! " (get-var1-id s) dnl)
       (define-var! syms (get-var1-id s) (get-var1-type s))
       )

      ((assign)
       ;; don't handle arrays yet... hmm.
       (if (ident? (get-assign-lhs s))
           (define-var! vals (cadr (get-assign-lhs s)) (get-assign-rhs s)))
       )
      
      ((loop-expression)
       (define-var! syms (get-loopex-dummy s) *default-int-type*)
       )

      ((waiting-if)
       (let ((dummies
              (map get-waiting-if-clause-dummy
                   (get-waiting-if-clauses s))))

         (map (lambda(nm)(define-var! syms nm '(boolean #f))) dummies))
       )

      ((parallel-loop sequential-loop)
;;       (dis "defining loop dummy : " (get-loop-dummy s) dnl)
       (define-var! syms (get-loop-dummy s) *default-int-type*))
         
      )
    )
  
  (define (stmt-post s)
;;    (dis "post stmt : " (get-stmt-kw s) dnl)
    (if (member (get-stmt-kw s) frame-kws) (exit-frame!))
    s
    )

  (let ((pass-result
         (cond  ((eq? 'global (car the-pass))
                 ((cadr the-pass) the-inits prog func-tbl struct-tbl cell-info)
                 )

                (else
                
                 (enter-frame!) ;; global frame
                 
                 ;; record interface objects
                 (let ((ports (caddddr cell-info)))
                   (map (lambda(pd)
                          (let ((pnm (cadr pd)))
;;                            (dis "defining port " pnm " : " pd dnl)
                            (define-var! syms pnm pd)))
                        ports))
                                
                 
                 ;; we should be able to save the globals from earlier...
;;                 (dis "initializations..." dnl)

                 (prepostvisit-stmt 
                  the-inits
                  stmt-pre0 stmt-post
                  identity identity
                  identity identity)
                 
;;                 (dis "program text..." dnl)
;;                 (set! debug #t)
                 
                 (let ((res
                        (prepostvisit-stmt prog
                                           (make-pre
                                            (car the-pass)
                                            (cadr the-pass))        stmt-post
                                            identity                identity
                                            identity                identity)))
                   (exit-frame!)
;;                   (set! debug #f)

                   res))
               
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

  (recurse (append 
            *the-passes-2*
            *the-passes-3*
            *the-passes-4*
            *the-passes-5*
;;            *the-passes-6*
            ))
  )

;; Select Graphic Rendition
(define sgr (list->string (list (integer->char 27))))

(define go-grn-bold-term (string-append sgr "[32;1m"))
(define go-red-bold-term (string-append sgr "[31;1m"))
(define reset-term       (string-append sgr "[0m"))

(define *program-changed*
    (string-append
              go-red-bold-term
             "============================  PROGRAM CHANGED"
              reset-term
              ))

(define (manual-pass proc prog)

  ;; use this to run a single pass on the program
  ;; 
  ;; e.g., (manual-pass inline-evals <program>)
  
  (let* ((pass (find-pass proc))
         (res (run-pass
               pass prog *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*)))

    (dis (if (equal? prog res)
             "============================  no change"
             *program-changed*
             )
         dnl)
    
    res
    )
  )

(define (make-simple-pass stmt-xform)
  (lambda(the-inits stmt func-tbl struct-tbl cell-info)
    (stmt-xform stmt)))

(define (define-simple-pass sym)
  (define-global-symbol
    (symbol-append sym '-pass) (make-simple-pass (eval sym))))

(define-simple-pass 'sequentialize-nonblocking-parallels)
(define-simple-pass 'constantify-constant-vars)
(define-simple-pass 'simplify-stmt)
(define-simple-pass 'unfold-loop-ranges)
(define-simple-pass 'convert-waiting-ifs)
          
(define (compile1!)
  ;; unquify the loops before doing ANYTHING else
  (set! text1
        (uniquify-loop-dummies *the-text*))
  'text1
  )

(define *the-passes-2* (list
                        (list 'assign handle-access-assign)
                        (list 'recv   handle-access-recv)
                        (list 'send   handle-access-recv)
                        (list 'assign handle-assign-rhs)
                        (list 'send   handle-send-rhs)
                        (list 'eval   handle-eval)
                        (list 'global inline-evals)
                        (list 'global global-simplify)
                        (list 'global remove-assign-operate)
                        (list 'global remove-do)
                        (list 'global simplify-if)
                        (list 'global convert-waiting-ifs-pass)
                        (list 'global unfold-loop-ranges-pass)
                        (list 'assign remove-loop-expression)))

(define (compile2!)
  (set! text2
        (uniquify-loop-dummies
         (run-compiler *the-passes-2*
                       text1
                       *cellinfo*
                       *the-inits*
                       *the-func-tbl*
                       *the-struct-tbl*)))
  'text2
  )

(define *the-passes-3*
  `((*       ,fold-constants-*)
    )
  )

(define (compile3!)
  (set! *the-globals* (construct-globals-tbl *the-inits*))
  (set! text3
        (uniquify-loop-dummies
         (run-compiler  
          *the-passes-3*
          text2
          *cellinfo*
          *the-inits*
          *the-func-tbl*
          *the-struct-tbl*))
        )
  'text3
  )


(define *the-passes-4*
  ;; we need to insert assignments after initialization for
  ;; all var1s that aren't immediately followed by an initialization,
  ;; before we run constantification
  `((*       ,fold-constants-*)
    (global  ,constantify-constant-vars-pass)
    )
  )

(define (compile4!)
  (set! text4
        (uniquify-loop-dummies
         (run-compiler  
          *the-passes-4*

          ;; we patch the uninitialized uses:
          (patch-uninitialized-uses text3)
          
          *cellinfo*
          *the-inits*
          *the-func-tbl*
          *the-struct-tbl*))
        )
  'text4
  )

(define *the-passes-5*
  `((global          ,sequentialize-nonblocking-parallels-pass)
    (global          ,delete-unused-vars-pass)
    (global          ,simplify-stmt-pass)
    (sequential-loop ,unblock-loops)
;;    (*       ,fold-constants-*)
;;    (global  ,constantify-constant-vars-pass)
    )
  )

  
(define (compile5!)
    (set! text5
          (uniquify-loop-dummies
           (run-compiler
            *the-passes-5*
            text4
            *cellinfo*
            *the-inits*
            *the-func-tbl*
            *the-struct-tbl*)))
;;  (display-success-2)
  'text5
  )

(define fixpoint-simplify-stmt (make-fixpoint-func simplify-stmt))

(define (compile6!)
  (make-the-tables text5)
  (seed-integer-ranges!)
  (close-integer-ranges!)
  (propose-types!)
  (set! text6 text5)
  'text6
  )

(define (compile7!)
  (set! text7
        (fixpoint-simplify-stmt (insert-block-labels text6))
        (dis "=========  PROGRAM LABELS INSERTED" dnl)
        )
  'text7
  )

(define (compile8!)
  (set! text8
        (map fixpoint-simplify-stmt  (scan-stmt text7) ) )
  'text8
  )


(define (compile9!)
  (set! text9
        (remove-empty-blocks text8) )
        (dis "=========  PROGRAM BLOCKS GENERATED" dnl)
        (dis "---  BEGIN PROGRAM LISTING  ---" dnl)
        (map pp text9)
        (dis "---   END PROGRAM LISTING   ---" dnl)
  'text9
  )

(define (compile!)
  (compile1!)
  (compile2!)
  (compile3!)
  (compile4!)
  (compile5!)
  (compile6!)
  (compile7!)
  (compile8!)
  (compile9!)
  )


(define (write-text sym)
  (let* ((fn (string-append sym ".dmp"))
         (wr (FileWr.Open fn)))
    (dis (stringify (eval sym)) wr)
    (Wr.Close wr)
    )
  )

(define (write-object fn obj)
  (let* ((wr (FileWr.Open fn)))
    (dis (stringify obj) wr)
    (Wr.Close wr)
    )
  )
  


(define (mn) (make-name-generator "t"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-identity x)
  (dis (stringify x) dnl)
  x
  )

(define (identifier? x)
  (if (and (pair? x) (eq? 'id (car x)))) x #f)

(define (binary-expr? x)
  (and
   (pair? x)
   (= 3 (length x))
   (or (unary-op? (car x)) (binary-op? (car x)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (desugar-stmt stmt)
  ((obj-method-wrap (convert-stmt stmt) 'CspSyntax.T) 'lisp))

(define (desugar-prog p)
  (if (not (and (pair? p) (eq? (car p) 'csp)))
      (error (string-append "Not a CSP program : " p)))

      (desugar-stmt (close-text p))
  )

(define testx '
  (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ "mesh_forward" "a") "b") "a") "c") "b") "a") "c") "b") "a") "c")"b") "a") "c")
  )

