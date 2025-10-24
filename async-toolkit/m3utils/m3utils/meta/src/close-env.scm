;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code for closing the environment on a cell being verified
;;
;;
;;
;; Copyright 2011 Fulcrum Microsystems.  All rights reserved.
;; Author: Mika Nystrom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (find-left-fanin-path channel-text)
  (find-fanin-path (string-append channel-text ".0")
                   (string-append channel-text ".e")))

(define (make-conjunct name dir)
  (new-modula-object 'Dsim.Conjunct  `(sense . ,dir)  `(input . ,name)) )

(define (make-rule tgt-name sense conj-lst)
  (let ((ref-lst
         (let loop ((p conj-lst)
                    (res '()))
           (if (null? p) res (loop (cdr p) (RefList.Cons (car p) res))))))
    (new-modula-object 'Dsim.Rule 
                       `(target . ,tgt-name)  `(sense . ,sense) `(conjuncts . ,ref-lst))))

(define (make-inverter-rules in out)
  (dis "(make-inverter-rules "(stringify in) " " (stringify out) ")" dnl)
  (let* ((in-name  (Name.ParseText in))
         (out-name (Name.ParseText out))
         (p-conj (make-conjunct in-name 'Down))
         (n-conj (make-conjunct in-name 'Up))
         
         (p-rule (make-rule out-name 'Up (list p-conj)))
         (n-rule (make-rule out-name 'Down (list n-conj))))
    (list p-rule n-rule)))

(define (make-nor-rules ins out)
  (dis "(make-nor-rules "(stringify ins) " " (stringify out) ")" dnl)
  (let* ((in-names (map Name.ParseText ins))
         (out-name (Name.ParseText out))
         (p-conjs
          (map (lambda(n)(make-conjunct n 'Down)) in-names))
         (n-conjs
          (map (lambda(n)(make-conjunct n 'Up)) in-names))
         (p-rule (make-rule out-name 'Up p-conjs))
         (n-rules (map (lambda(conj)(make-rule out-name 'Down (list conj)))
                       n-conjs)))
    (cons p-rule n-rules)))

(define (make-inverter-rules in out) (make-nor-rules (list in) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-rule-to-prs rule prs)
  (modula-type-op 'PRS.T 'set-field! prs 'rules 
                  (RefList.Cons rule 
                                (modula-type-op 'PRS.T 'get-field prs 'rules))))
(define (make-inverter-chain node-lst)
  (let loop ((res '())
             (p node-lst))
    (if (null? (cdr p)) 
        res
        (loop (append (make-inverter-rules (car p) (cadr p)) res)
              (cdr p)))))

(define (prs-has-node? prs node)
  (let ((node-tbl (modula-type-op 'PRS.T 'get-field prs 'nodes)))
    (table-get 'NameRefTbl.T node-tbl (Name.ParseText node))))

(define (prs-has-node-under-alias? prs node)
  (> (length (filter (lambda(x)x)
                     (map (lambda(a)(prs-has-node? prs a))
                          (all-node-aliases node))))
     0))

(define (add-node-if-not-exists node prs)
  (dis "(add-node-if-not-exists " (stringify node) " prs)" dnl)
  (if (not (prs-has-node-under-alias? prs node))
      (modula-type-op 'PRS.T 
                      'call-method 
                      prs 
                      'node 
                      (list (Name.ParseText node)))))

(define (add-channel-return-path instance-txt channel-name prs)
  (let ((fanin-path 	(find-left-fanin-path 
                         (string-append instance-txt "." channel-name))))
    (map (lambda(n)(add-node-if-not-exists n prs)) fanin-path)
    (map (lambda(r)(add-rule-to-prs r prs))
         (make-inverter-chain fanin-path))))


(define (add-arbitrary-delay-bitbucket instance-txt rails enable prs tm)
  (define (instantiate n) (string-append instance-txt "." n))

  (let* ((fanins (map canonical-name (map instantiate rails)))
         (fanout (canonical-name (instantiate enable)))
         (nor-rules (make-nor-rules fanins fanout)))
    (map (lambda(fi)
           (modula-type-op 'Circuit.CktTimingModel
                           'call-method
                           tm
                           'addArbitraryDelay
                           (list (Name.ParseText fi)
                                 (Name.ParseText fanout))))
         fanins)

    (map (lambda(r)(add-rule-to-prs r prs)) nor-rules)))
