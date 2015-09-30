
(define *num-preds* 20)

(let ((out-wr (filewr-open (string-append "../output/" problem-definition-file ".out"))))
	(dis "RESULT " ((expressions 'retrieve 'TARGET) 'get 'current-limits) dnl out-wr)
	(dis "MINIMIZATION-SEQUENCE " search-sequence dnl out-wr)

	(let* ((tgts      (get-target-components))
				 (late-tgt  (cadadr (cdr tgts)))
				 (early-slew (car (cdaddr  (car (get-target-components)))))
				 (early-tgt (cadadr (car tgts))))
		(dis "EARLY-SLEW " (compute-actual-value early-slew) dnl out-wr)
		(dis "EARLY-PREDECESSORS " (get-n-predecessors early-tgt *num-preds*) dnl out-wr)
		(dis "LATE-PREDECESSORS " (get-n-predecessors late-tgt *num-preds*) dnl out-wr))
		
		
	
	(wr-close out-wr))

