;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Scheme definitions for metastability analysis
;;
;;
;; Copyright 2011 Fulcrum Microsystems.  All rights reserved.
;; Author: Mika Nystrom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-modules "basic-defs" "display" "m3" "hashtable" "set" "mergesort")

;; set up data
(define type-tables (TypeTables.Make dsim-top dsim-types))

(set! type-instances (cdar  type-tables))
(set! instance-types (cdadr type-tables))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tail-component name)  
	(if (null? name) (error "tail-component of null name")
			(Name.Tail   name)))

(define (parent-component name)
  (if (equal? name (Name.Empty)) (error "no parent")
      (Name.Parent name)))

(define (have-parent? text) (not (equal? (Name.ParseCharsRef text) (Name.Empty))))

(define (mapconscar lst)
  (let ((c (car lst)))
    (map (lambda (x) (cons c x)) (cdr lst))))

(define (convert-m3-list-to-list type nl)
  (if (null? nl) 
      '()
      (cons (modula-type-op type 'get-field nl 'head)
            (convert-m3-list-to-list
						 type
             (modula-type-op type 'get-field nl 'tail)))))

(define (convert-namelist-to-list nl)
	(map Name.Format (convert-m3-list-to-list 'NameList.T nl)))
	

(define (convert-namelist-to-list nl)
  (if (null? nl) 
      '()
      (cons (Name.Format    
             (modula-type-op 'NameList.T 'get-field nl 'head))
            (convert-namelist-to-list
             (modula-type-op 'NameList.T 'get-field nl 'tail)))))

(define (get-field type value field)
  (modula-type-op type 'get-field value field))

(define (table-get tbl-type tbl key)
  ;; VAR param workaround
  (let ((a `(, key ())))
    (if (modula-type-op tbl-type 'call-method tbl 'get a)
        (cadr a)
        #f)))

(define (symbol-append . x)
  (string->symbol
   (apply string-append (map symbol->string x))))

(define (enumerate-set set-intf set)
  ;; enumerate a Modula-3 Set.ig instance
  (let ((iter (modula-type-op (symbol-append set-intf '.T) 
                              'call-method set 'iterate))
        (res '()))
    (let loop ((next '(())))
      (if (modula-type-op (symbol-append set-intf '.Iterator)
                          'call-method
                          iter
                          'next
                          next)
          (begin (set! res (cons (car next) res))
                 (loop '(())))
          res))))

(define (enumerate-tbl-keys tbl-intf tbl . max)
  ;; enumerate the keys of a Modula-3 Table.ig instance
  (let ((iter (modula-type-op (symbol-append tbl-intf '.T) 
                              'call-method tbl 'iterate))
        (res '()))
    (let loop ((n 0)(next '(()())))
      (if (and (or (null? max)(< n (car max)))
               (modula-type-op (symbol-append tbl-intf '.Iterator)
                               'call-method
                               iter
                               'next
                               next))
          (begin (set! res (cons (car next) res))
                 (loop (+ n 1) '(()())))
          res))))

(define (get-type-instances txtname)
  (let ((a `(,(Name.ParseCharsRef txtname) ())))
    (if (modula-type-op 'NameNameListTbl.T 'call-method type-instances 'get a)
        (convert-namelist-to-list (cadr a))

				(if (eq? (Name.ParseText txtname) top-name) (list "") '()) 
				;; check for top cell too

 )))

(define (get-parent n)
  (if (string? n)
      (Name.Format (parent-component (Name.ParseCharsRef n)))
      (Name.Format (parent-component n))))

(define (get-tail n)
  (Name.Format
   (if (string? n)
       (tail-component (Name.ParseCharsRef n))
       (tail-component n))))


(define (get-instance-type txtname)
  (if (or (equal? txtname "") (eq? txtname '%TOP%)) (Name.Format top-name)
      (let ((a `(,(Name.ParseCharsRef txtname) ())))
        (if (modula-type-op 'NameNameTbl.T 'call-method instance-types 'get a)
            (Name.Format (cadr a))
            #f))))

(define (nameget-instance-type name)
  (let ((a `(,name ())))
    (if (modula-type-op 'NameNameTbl.T 'call-method instance-types 'get a)
        (cadr a)
        #f)))

(define (mapunique op lst)
  (let ((r (map op lst))
        (s (make-string-hash-table 10)))
    (define (insert str) (s 'update-entry! str))
    (map insert r)
    (s 'keys)
    ))

(define (mapuniqual op lst)
  (let loop ((r (map op lst))
             (s '()))
    (cond ((null? r) s)
          ((member? (car r) s) (loop (cdr r) s))
          (else (loop (cdr r) (cons (car r) s))))))
    

(define (escape-for-unix str)

  (define (do-escape? c)
    (member? (char->integer c) '(40 41 34 32)))

  (define (helper lst)
    (cond ((null? lst) '())
          ((do-escape? (car lst)) 
           (cons #\\ (cons (car lst) (helper (cdr lst)))))
          (else (cons (car lst) (helper (cdr lst))))))

  (let ((l (string->list str)))
    (list->string (helper l))))

(define (routed-no-recurse cell) 
  ;; responds with cell name plus some whitespace if cell is routed
  (run-command "fulcrum cast_query --max-heap-size=2G --task=subcells --filter=\"directive=routed:true\" --cell=\"" (escape-for-unix cell) "\" --no-recurse --cast-path=/mnt/fulcrum/alta/mnystrom/p4/hw-main/cast:/mnt/fulcrum/alta/mnystrom/p4/hw-alta/layout/tsmc65/spec"))
    

(define (whitespace? c) (member? (char->integer c) '(9 10 32)))

(define (not-whitespace? c) (not (whitespace? c)))

(define (memoize f)
  (let ((ans '()))
    (lambda (x)
      (let ((old (assoc x ans)))
        (if  (not old)
            (let ((new (f x))) (set! ans (cons (cons x new) ans)) new)
            (cdr old))))))
                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; code for checking whether a certain type is "routed"

(define routed-type? '())

(let ((routed-helper?
       (lambda(cell) 
         (equal? (string-append cell) 
                 (list->string 
                  (filter not-whitespace? (string->list (routed-no-recurse cell))))))))

  (define routed-two? (memoize routed-helper?))
  
  (set! routed-type? 
        (lambda (cell) 
          (begin
            (dis "Checking whether " cell " is routed: ")
            (let ((res (routed-two? cell)))
              (dis (if res "YES" "NO") dnl)
              res)))))

;; new version using the data from the routed-set 
(define wrapped-routed-set (obj-method-wrap routed-set 'NameSet.T))

(define (routed-type? typetxt) 
  (wrapped-routed-set 'member (Name.ParseCharsRef typetxt)))
              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-parent-types type)
  (mapunique get-instance-type 
             (mapunique get-parent (get-type-instances type))))


;; the search routine

(define (search-routed-parents state)
  ;; state is a list of pairs ( type-name . sub-instance-name )

  (map search-one-parent state)
)

(define (iter-parent-type pair)
  (let ((par (car pair))
        (sub (cdr pair)))
             
    ;; sub is a sub-instance-name
    ;; par is a full parent instance name
    ;; evaluates to a pair: (instantiated-type . sub-instance-name)
    (let ((parent-type (get-instance-type par)))
      (if parent-type 
          (cons parent-type sub)
          (if (have-parent? par)
              (iter-parent-type 
               (cons (get-parent par)
                     (string-append (get-tail par) (if (null? sub) "" (string-append "." sub))) ))
              #f)))))


(define (search-one-parent s)
  (let ((done      (car   s))
        (type-name (cadr  s))
        (inst-name (caddr s)))
    (cond (done s)
          ((routed-type? type-name) (list #t type-name inst-name))
        
          (else 
           (let* ((instances        (get-type-instances type-name))
                  (parent-instances (map get-parent instances)))
             (map (lambda (sub par) (cons  (get-instance-type par)
                                           (get-tail sub)))
                  instances
                  parent-instances))))))

          
(define (find-sub-instances pair)
  ;; within the top-level cell, search for a given type and return
  ;; all its instances together with their instance names.
  ;; 
  ;; pass it: '(<type> <type-string>)
  ;; where <type-string> denotes the dotted path to the actual target
  ;; normally this would start null
  ;;
  ;; the return value is a list of dotted pairs of instance names
  ;; followed by a subcell name from there.
  
  (let ((target (car pair))
        (post   (cdr pair)))
    (map (lambda (instance) 
           (let ((p (get-parent instance)))
             (cons (if (string=? p "") '%TOP% p)
                   (string-append (get-tail instance)
                                  (if (null? post) ""
                                      (string-append "." post))))))
         (get-type-instances target))))
          


(define (all-parent-types type-sub)
  (mapuniqual iter-parent-type (find-sub-instances type-sub)))

(define (uniqify lst)
  (mapuniqual (lambda(x) x) lst))

(define (iterate-till-all-routed lst)
  ;;
  ;; lst is 
  ;; (<state-comp> .. <state-comp>)
  ;;
  ;; <state-comp> is
  ;; #t/#f type-name instance-name-within-type
  ;;
  ;; start with ((#f unrouted-type-name))
  ;; terminates with ((#t routed-type-name . instance-prefix) ... )
  ;;

;;  (dis "(iterate-till-all-routed '" (stringify lst) ")" dnl)

  (define (done? x)
    (or (car x)(string=? (cadr x) (Name.Format top-name))))

  (let ((unrouted (filter (lambda(x)(not (done? x))) lst))
        (routed   (filter (lambda(x)(     done? x))  lst)))
;;    (dis "(define unrouted '" (stringify unrouted) ")" dnl)
;;    (dis "(define routed   '" (stringify   routed) ")" dnl)
    (let ((curr
           (append routed 
                   (let ((parents 
                          (uniqify 
                           (apply append
                                  (map all-parent-types (map cdr unrouted))))))
                     (map cons (map (lambda(x)(routed-type? (car x))) parents) 
                          parents)))))
;;      (dis "current state: " curr dnl)
;;      (dis "current done:  " (map done? curr) dnl)
      (if (member? #f (map done? curr))
          (iterate-till-all-routed curr)
          curr)
)))

(define (routed-instance? i) 
  (cond ((get-instance-type i) => routed-type?)
        (else #f)))

(define (routed-super-instance i)
	;; pass it (<instance>) : returns (<instance> <rel-path>)
  (cond ((equal? (car i) "") 
				 (if (routed-type? (Name.Format top-name))
						 i
						 #f))

        ((routed-instance? (car i))  i)

        (else (routed-super-instance 
							 (list (get-parent (car i)) 
										 (let ((tail (get-tail (car i))))
											 (if (null? (cadr i)) 
													 tail
													 (string-append tail "." (cadr i)))))))))
																					 

;; DSIM stuff
        
(define (get-dsim-type txtname)
  (if (or (null? txtname) (equal? txtname (Name.Format top-name)))
      dsim-top
      (let ((a `(,(Name.ParseCharsRef txtname) ())))
        (if (modula-type-op 'NameRefTbl.T 'call-method dsim-types 'get a)
            (cadr a)
            #f))
      )
)

(define (matching-instantiated-types abstract-type)
  (map Name.Format 
       (MetaUtils.SetToList 
        (MetaUtils.MatchingRoots 
         (MetaUtils.NRKeysToSet dsim-types) (Name.ParseCharsRef abstract-type)
         )
        )
       ))

(define (nameseq-to-list seq)
	;;(if (null? seq) (error "null seq"))
  (let ((size (modula-type-op 'NameSeq.T 'call-method seq 'size))
        (res '()))
    (define (helper n) 
      (if (= n -1) 
          res
          (begin 
            (set! res 
                  (cons (Name.Format 
                         (modula-type-op 'NameSeq.T 'call-method seq 'get (list n)))
                        res ))
            (helper (- n 1)))))
    (helper (- size 1))))

    
(define (index-find x lst)
  (let loop ((l lst)
             (n 0))
    (cond ((null? l) #f)
          ((equal? (car l) x) n)
          (else (loop (cdr l) (+ n 1))))))
        
(define (indices-find x lst)
  (let loop ((l lst)
             (n 0)
             (res '()))
    (cond ((null? l)          res)
          ((equal? (car l) x) (loop (cdr l) (+ n 1) (cons n res)) )
          (else               (loop (cdr l) (+ n 1) res           )))))
  
(define (type-args typetxt)
  (let* ((dsim-type (get-dsim-type typetxt))
         (args      (modula-type-op 'Dsim.Define 'get-field dsim-type 'args)))
		(if (null? args) '() (nameseq-to-list args))))


(define (maptag tag lst)
  (map (lambda (x) (cons tag x)) lst))

(define (cdrfiltertag tag lst)
  (map cdr (filter (lambda (x) (eq? (car x) tag)) lst)))

(define debug-cn-typetxt '())
(define debug-cn-nodetxt '())

(define (characterize-node typetxt nodetxt)
	(set! debug-cn-typetxt typetxt)
	(set! debug-cn-nodetxt nodetxt)
  ;; characterize a node within a cell type
  ;; global characteristics have to be assembled by traversing 
  ;; the instantiation DAG
  ;; see the routine "search2" for how this code is used
  (let* ((dsim-type (get-dsim-type  typetxt))
         (node-name (Name.ParseCharsRef nodetxt))
         (argstxt   (type-args typetxt)))

    (append 
     (maptag 'rule-driver (get-node-rule-drivers typetxt nodetxt))
     (maptag 'rule-fanout (get-node-rule-fanouts typetxt nodetxt))
     (maptag 'subport (get-subcell-port-connections typetxt nodetxt))
     (maptag 'alias (local-aliases typetxt nodetxt))
     (maptag 'port  (indices-find nodetxt argstxt)))))

(define (nameseq-get seq i) 
  (modula-type-op 'NameSeq.T 'call-method seq 'get (list i)))

(define (get-port-name typetxt node-number)
  (let* ((dsim-type (get-dsim-type  typetxt))
         (args      (modula-type-op 'Dsim.Define 'get-field dsim-type 'args))
         (port      (nameseq-get args node-number)))
    (Name.Format port)))

(define (reflist-to-list rl)
  (if (null? rl) '() 
      (cons (modula-type-op 'RefList.T 'get-field rl 'head)
            (reflist-to-list
             (modula-type-op 'RefList.T 'get-field rl 'tail)))))

(define (reflist-to-list rl) (MetaUtils.RefListToList rl))

(define (get-decls typetxt)
  (let* ((dsim-type 
            (get-dsim-type  typetxt))
         (decls     
             (reflist-to-list
              (modula-type-op 'Dsim.Define 'get-field dsim-type 'decls))))
    decls))

(define (get-rules typetxt)
  (let* ((dsim-type 
            (get-dsim-type  typetxt))
         (dsim-body
            (modula-type-op 'Dsim.Define 'get-field dsim-type 'dsimBody))
         (rules     
             (reflist-to-list
              (if (null? dsim-body) '()
                  (modula-type-op 'Dsim.DsimBody 'get-field dsim-body 'rules)))))
    rules))

(define (get-decl-field d fld) 
  (modula-type-op 'Dsim.Decl 'get-field d fld))
                                
(define (get-rule-field d fld) 
  (modula-type-op 'Dsim.Rule 'get-field d fld))
                            
(define (analyze-conjunct c)
  (cons
   (modula-type-op 'Dsim.Conjunct 'get-field c 'sense)
   (Name.Format (modula-type-op 'Dsim.Conjunct 'get-field c 'input))))

(define analyze-conjunct '())

(define (analyze-rule rule)
  ;; slow -- mainly for debugging
  (let ((conjuncts    (map
                       analyze-conjunct
                       (reflist-to-list (get-rule-field rule 'conjuncts))))
        (target       (Name.Format (get-rule-field rule 'target)))
        (sense        (get-rule-field rule 'sense))
        (attrs        (get-rule-field rule 'attrs))
        (delay        (get-rule-field rule 'delay)))
    `((conjuncts . ,conjuncts) 
      (target    . ,target)
      (sense     . ,sense)
      (attrs     . ,attrs)
      (delay     . ,delay))))

(define (has-fanin? rule fanin-name)
  (let ((conjuncts (reflist-to-list (get-rule-field rule 'conjuncts))))
    (let loop ((p conjuncts))
      (cond ((null? p)                #f)
            ((eq? (modula-type-op 'Dsim.Conjunct 'get-field (car p) 'input)
                  fanin-name)         #t)
            (else                     (loop (cdr p)))))))

(define (has-fanout? rule fanout-name)
  (eq? fanout-name (get-rule-field rule 'target)))

(define (get-node-rule-drivers typetxt nodetxt)
  (filter (lambda(r)(has-fanout? r (Name.ParseCharsRef nodetxt)))
          (get-rules typetxt)))

(define (get-node-rule-fanouts typetxt nodetxt)
  (filter (lambda(r)(has-fanin? r (Name.ParseCharsRef nodetxt)))
          (get-rules typetxt)))

(define (get-rule-fanins rule)
  (map cdr (cdr (assoc 'conjuncts (analyze-rule rule)))))

(define (get-rule-fanout rule)
  (Name.Format (get-rule-field rule 'target)))

(define (get-node-node-fanins typetxt nodetxt)
  (uniq string=? 
        (apply append 
               (map get-rule-fanins (get-node-rule-drivers typetxt nodetxt)))))

(define (get-node-node-fanouts typetxt nodetxt)
  (uniq string=?
        (map get-rule-fanout (get-node-rule-fanouts typetxt nodetxt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-named-subcell typetxt subcelltxt)
  (let* ((nn         (Name.ParseCharsRef subcelltxt))
         (decls      (get-decls typetxt))
         (matches    (filter
                      (lambda (d) (eq? nn (get-decl-field d 'id))) decls)))
    (if (null? matches) #f
        (let ((match (car matches)))
          (list 
           (cons 'type (Name.Format 
                        (get-decl-field match 'type)))
           (cons 'args (nameseq-to-list 
                        (get-decl-field match 'args))))))))
  
(define (local-aliases typetxt nodetxt)
  ;; find all the local aliases in the named type of
  ;; the named node (in that type's name space)
  (let* ((dsim-type (get-dsim-type typetxt))
         (wires     (get-field 'Dsim.Define dsim-type 'wires))
         (alias-set (table-get 'NameRefTbl.T wires (Name.ParseCharsRef nodetxt))))
    (if (not alias-set) 
        (list nodetxt)
        (uniq string=? 
              (cons nodetxt 
                    (map Name.Format (enumerate-set 'NameSet alias-set)))))))

(define (search-port supercell-type subcell-instance port-number)
  ;; a node name in the supercell for the numbered port of the subcell
  (nth 
   (cdr (assoc 'args  (get-named-subcell supercell-type subcell-instance)))
   port-number))

(define (get-subcell-port-connections typetext nametext)
  (apply append 
   (map mapconscar
        (map (lambda(x)
               (cons (Name.Format (get-decl-field x 'id) )
                     (indices-find 
                      nametext 
                      (nameseq-to-list (get-decl-field x 'args)))) )
             (get-decls typetext)))))

(define (search-node nodetxt)
  ;; find the most local splitting of nodetxt 
  (let loop ((cell (Name.ParseCharsRef nodetxt))
             (node (Name.Empty)))
    (cond ((null? cell) #f)
          ((nameget-instance-type cell)
           (cons (Name.Format cell) (Name.Format  node)))
          (else (loop (parent-component                      cell)
                      (Name.Append    (tail-component cell)  
                                      node ))))))

(define (search-parent cell)
  (let loop ((parent        (get-parent       cell))
             (child         (get-tail cell)))
    (let      ((parent-type   (get-instance-type parent)))
      (cond ((and (null? parent-type) (null? parent)) #f)
            (parent-type (list parent parent-type child))
            (else (loop (get-parent parent)
                        (string-append (get-tail parent) "." child))))
           )))

(define (get-subcell-decl typetxt instancetxt)
  (let ((decls (get-decls typetxt))
        (iname (Name.ParseCharsRef instancetxt)))
    (let ((matching-decl 
           (car (filter 
                 (lambda (d) (eq? iname (get-decl-field d 'id))) 
                 decls))))
      matching-decl)))
  

(define (get-subcell-type typetxt instancetxt)
  (Name.Format 
   (get-decl-field (get-subcell-decl typetxt instancetxt) 'type)))

(define (get-subcell-port-binding typetxt instancetxt portid)
;;  (dis "(get-subcell-port-binding " (stringify typetxt) " " (stringify instancetxt) " " portid ")" dnl)
  (Name.Format
   (nameseq-get
    (get-decl-field (get-subcell-decl typetxt instancetxt) 'args)
    portid)))

(define globals '("GND" "Vdd" "RESET" "_RESET" "ERROR"))

(define (is-global? node) (member? node globals))
(define (not-global? node) (not (is-global? node)))

(define (globalize-node context node)
  (if (is-global? node) node (string-append context "." node)))

(define (has-parent-with-type? id)
  (not (equal? (get-parent id) "")))

(define (search2 nodetxt)
  ;; track down the global characteristics of a (globally named) node
  ;; including all ports, aliases, production rules, etc.
  (let ((so-far '()))

    (define (make-tag cell node) (cons cell node))

    (define (have-tagged? tag) (member? tag (map car so-far)))
    (define (new-tag? tag) (not (have-tagged? tag)))

    (define (save-result! tag result) 
      (let ((tagged (cons tag result))) (set! so-far (cons tagged so-far))))

    (define (recurse start-cell start-name)
      ;;(dis "(recurse " (stringify start-cell) " " (stringify start-name) ")" dnl)
        
      (let ( (start-type (get-instance-type start-cell))
             (tag        (make-tag start-cell start-name)) )

        (define (search-subport subport)
          (let* ((sub-instance (car subport))
                 (port-id      (cdr subport))
                 (subcell-type (get-subcell-type start-type sub-instance))
                 (subnodename  (get-port-name subcell-type port-id)))
            (recurse 
						 (if (equal? start-cell "") sub-instance (string-append start-cell "." sub-instance))
						 subnodename)))

        (define (search-superport portid)
          (let* ((super          (search-parent start-cell))
                 (super-instance (nth super 0))
                 (super-type     (nth super 1))
                 (my-name        (nth super 2))
                 (node-in-super  (get-subcell-port-binding 
                                  super-type my-name portid)))
            (recurse super-instance node-in-super)))
        
        (define (search-alias alias)
          (recurse start-cell alias))
        
        (if (new-tag? tag)
            (if (is-global? (get-tail start-name))
                (save-result! tag
                              `((GLOBAL . ,(get-tail start-name))))
                (begin
                  (let  ((local-data (characterize-node start-type start-name)))
                    (save-result! (make-tag start-cell start-name) local-data)
                    (map search-subport   (cdrfiltertag 'subport local-data))
                    (map search-superport (cdrfiltertag 'port    local-data))
                    (map search-alias     (cdrfiltertag 'alias   local-data))
                    ))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (let ((start (search-node nodetxt)))
			(if (or (not start) (string=? (cdr start) ""))
					#f
					(recurse  (car start) (cdr start))))

    so-far))

(define (analyze-conjunct c)
  (cons (Name.Format (get-field 'Dsim.Conjunct c 'input))
                     (get-field 'Dsim.Conjunct c 'sense)))

(define (analyze-rule rule)
  `( (conjuncts . ,(map analyze-conjunct 
                        (reflist-to-list 
                         (get-field 'Dsim.Rule rule 'conjuncts))))
     (target . ,(Name.Format (get-field 'Dsim.Rule rule 'target)))
     (sense . ,(get-field 'Dsim.Rule rule 'sense)) ))

(define (get-rule-fanout rule)
  (Name.Format (get-field 'Dsim.Rule rule 'target)))

(define (get-rule-fanins rule)
  (map (lambda(c)(Name.Format (get-field 'Dsim.Conjunct c 'input)))
       (reflist-to-list (get-field 'Dsim.Rule rule 'conjuncts))))
            
(define (alias-global-fanouts alias-characteristics)
  (let ((env-name (caar alias-characteristics)) ;; cell part of alias
        (rules    (cdrfiltertag 'rule-fanout alias-characteristics)))
    (uniq string=?
          (map (lambda(r) (globalize-node env-name (get-rule-fanout r)))
               rules))))


(define (alias-global-fanins alias-characteristics)
  (let ((env-name (caar alias-characteristics)) ;; cell part of alias
        (rules    (cdrfiltertag 'rule-driver alias-characteristics)))

    (map (lambda(n)(globalize-node env-name n))
         (uniq string=? (apply append (map get-rule-fanins rules))))))

(define (node-fans alias-op)
  ;; take a search op on an alias and turn it into a node-search op
  (lambda(node)
      (uniq string=? (apply append (map alias-op (search2 node))))))

(define node-fanouts (node-fans alias-global-fanouts))

(define node-fanins  (node-fans alias-global-fanins))

(define (node-real-fans alias-op)
  (lambda(x)(filter not-global? ((node-fans alias-op) x))))

(define node-real-fanouts (node-real-fans alias-global-fanouts))

(define node-real-fanins  (node-real-fans alias-global-fanins))

;; representation is as follows:
;; ((current-node intermediate-node ... start-node) ... )
;; every step expands length of lists (Ariadne's pearls)

(define (expand-one-list node-fans lst)
  (map (lambda(x)(cons x lst)) (node-fans (car lst))))

(define (iter-step node-fans state) 
  (apply append (map (lambda(l)(expand-one-list node-fans l)) state)))

(define max-iterate-steps 10)

(define (all-node-aliases node)
  (uniq string=?
        (map (lambda(c)(string-append (car c) "." (cdr c))) 
             (map car (search2 node)))))

(define (count-dots a)
  (length (filter (lambda(c)(eq? c #\.)) (string->list a))))

(define (fewer counter) (lambda (x y)(< (counter x)(counter y))))

(define (filter-min counter lst)
  (let ((tgt (apply min (map counter lst))))
    (filter (lambda (x) (= tgt (counter x))) lst)))

(define (filter-max counter lst)
  (let ((tgt (apply max (map counter lst))))
    (filter (lambda (x) (= tgt (counter x))) lst)))

(define (shortest-alias lst)
  ;; apparently the sort isnt stable, grrr
  ;; filter by # of dots first, then by # of chars, then in alpha order

  (car (mergesort (filter-min string-length (filter-min count-dots lst))
                  string<?)))


(define (canonical-name-impl node) 
	(let ((aliases (all-node-aliases node)))
		(if (null? aliases) 
				node
				(shortest-alias (all-node-aliases node)))))

(define canonical-name (memoize canonical-name-impl))

(define (iterate-search node-fans start-node search-node)
  (let ((search-aliases (all-node-aliases search-node)))

    (define (search-node? n) (member? n search-aliases))
      
    (let loop ((state (iter-step node-fans `((,start-node))))
               (n 1))

      (let ((matches (filter (lambda(s)(search-node? (car s))) state)))

        (cond ((> n max-iterate-steps) #f)

              ((not (= 0 (length matches))) matches)

              (else (loop (iter-step node-fans state) (+ n 1))))))))
           

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
  
(define (l) (load "meta.scm"))


;;(define a-primitive 
;;        "lib.metastable.primitive.probe.PRIMITIVE_PROBE_WAIT_CORE.1006")

;;(define a-primitive-instance
;;  (car (get-type-instances a-primitive)))

;;(define q0
;;  (string-append a-primitive-instance ".Q.0"))

;;(define qe
;;  (string-append a-primitive-instance ".Q.e"))

(define (find-fanin-path fanout fanin)
  (dis "(find-fanin-path " (stringify fanout) " " (stringify fanin) ")" dnl)
  (map canonical-name (car (iterate-search node-real-fanins fanout fanin))))

(define (all-prefixes n)

  ;; "a.b.c" -> ("a" "a.b" "a.b.c")

  (let loop ((prefixes '())
             (x         n))
    (if (string=? x "") 
        prefixes
        (loop (cons x prefixes) (get-parent x)))))
        

(define (all-splittings n)
  ;; "a.b.c" -> (("a" . "b.c") ("a.b" . "c") ("a.b.c")))
  (let loop ((splittings '())
             (x         (list n)))
    (if (string=? (car x) "") 
        splittings
        (loop (cons x splittings) 
              (cons (get-parent (car x))
                    (string-append (get-tail (car x)) 
                                   (if (null? (cdr x)) ""
                                       (string-append "." (cdr x)))))))))


(define (intersection elem-eq? x . y)
  (if (null? y) 
      x
      (apply intersection elem-eq?
             (cons
              (filter (lambda(z)(mem? elem-eq? z x)) (car y))
              (cdr y)))))

(define (car-equal? a b) (equal? (car a) (car b)))

(define (common-supercell canonical-list)
  ;; not sure if this is quite right if we are using just the canonical names.
  ;; should we be using ALL aliases instead?
  (let ((cands (apply intersection equal?
                          (map all-prefixes canonical-list))))
		(if (null? cands) ""
				(car (filter-max (lambda (x)(count-dots x)) cands)))))


;; food for thought.
;; after common-supercell on the path we have the following:
;; all canonical names are included, and we have the common supercell
;; containing them.  
;; we therefore cannot have higher-level names (no ports possible)


(define (elim-1-prefix pfx str)
  (let* ((len     (string-length pfx))
         (str-pfx (substring str 0 len))
         (str-sfx (substring str len 1e6)))
    (if (not (string=? pfx str-pfx)) 
        (error (string-append "Not a prefix of: " str " : " pfx))
        str-sfx)))

 (define (elim-prefix pfx lst)
   (map (lambda(s)(elim-1-prefix pfx s)) lst))

(define (common-supercell-and-nodes path)
  (let ((super (common-supercell path)))
    (list super
          (elim-prefix (string-append super ".") path))))

(define (get-all-matching-instances primitive-type)
  ;; given a primitive type, e.g., 
  ;; "lib.metastable.primitive.probe.PRIMITIVE_PROBE_WAIT_CORE"
  ;; find all matching instances
  (apply append 
         (map get-type-instances 
              (matching-instantiated-types primitive-type))))

(define (get-actual-parent instance)
  (let ((p (get-parent instance)))
    (cond ( (string=? p "")        #f )
          ( (get-instance-type p)   p )
          ( else                    (get-actual-parent p) )
)))

(define (get-all-parent-types type)
  (map get-instance-type 

       (map (lambda(t)
              (cond ((get-actual-parent t) => (lambda(x)x))
                    (else t)))

            (get-type-instances type))))

(define (apply-repeatedly f n . to)
  (if (= 0 n) to 
      (let ((repeat
             (lambda (this) (apply-repeatedly f (- n 1) this))))
      (repeat (apply f to)))))

(define (make-modula-set type items)
	(let ((ns (modula-type-op type 
														'call-method
														(new-modula-object type)
														'init
														'(10))))
		(let loop ((lst items))
			(if (null? lst) ns (begin (modula-type-op type 'call-method
																								ns 
																								'insert 
																								(list (car lst)))
																(loop (cdr lst)))))))


(load "verify.scm")
