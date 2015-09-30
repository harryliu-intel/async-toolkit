;; $Id: fact.scm,v 1.2 2008/10/06 08:17:48 mika Exp $
;;
;; Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.
;;
;; Author: Mika Nystrom <mika@alum.mit.edu>
;;

(define (fact x)
  (if (= x 0) 1
      (* x (fact (- x 1)))))

(define (loop n) (if (> n 0) (loop (- n 1))))


