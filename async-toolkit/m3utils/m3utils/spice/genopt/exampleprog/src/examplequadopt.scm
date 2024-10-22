;;
;; An example of the super flexible optimization system in action!
;; The system is implemented as an embedded language in Scheme
;;
;; mika.nystroem@intel.com
;; August, 2024
;;

(require-modules "m3")

(define *m3utils* (Env.Get "M3UTILS"))
(define *bindir* "AMD64_LINUX")
(define *srcdir* (string-append *m3utils* "/spice/genopt/exampleprog/src"))

;; the following are parameters that may be overridden
;; the default values are stated
(def-paramvar 'temp   0)
(def-paramvar 'lib    "i0s")
(def-paramvar 'thresh "ulvt")
;;(def-paramvar 'sweeps 3)
(def-paramvar 'stages 11)
(def-paramvar 'step   1e-9)
(def-paramvar 'mean   1)
(def-paramvar 'sdev   1)


;; the following are the optimization variables
;; the two fields are the initial value and what we may consider a
;; significant delta (an approximation, of course)
(def-optvar 'vdd  0.000 1)
(def-optvar 'delp 0.000 1)
(def-optvar 'deln 0.000 1)

;; failure to eval result
(GenOpt.SetOptFailureResult 1e200)

;;(QuadRobust.SetSigmaK 5.3)

(QuadRobust.SetDoNominal #t)

;; model the three aspects of any variable:
;; nom mu sigma
(QuadRobust.DoModel 'result '(Quadratic Linear    Linear))
(QuadRobust.DoModel 't0     '(Quadratic Linear    Linear))
(QuadRobust.DoModel 't1     '(Linear    Linear    Linear))
(QuadRobust.DoModel 't2     '(Linear    Linear    Linear))
(QuadRobust.DoModel 'x2     '(Quadratic Quadratic Linear))


;;(QuadRobust.SetSelectByAll #t)
;; select mu/sigma fit by all likely data? (or by validation set otherwise)

;; NewUOAs configuration variables
(def-rhobeg 1)    ;; starting step size in terms of significant delta
(def-rhoend 1e-4) ;; ending step size for convergence

;; this is the experimental command to do an evaluation
(define *cmd-path* (string-append *m3utils*
                                  "/spice/genopt/exampleprog/"
                                  *bindir*
                                  "/exampleprog"))

;; all the following assumed to run in a single directory

(def-compute-command
  '(string-append *cmd-path*
                  " -quadstats "
                  " -method " 3
                  " -vdd "    vdd
                  " -temp "   temp
                  " -lib "    lib
                  " -thresh " thresh
                  " -sweeps " *stoc-samples*
                  " -cscale 1.0"
                  " -delp "   delp
                  " -deln "   deln
                  " -modleaves true"
                  " -stages " stages
                  " -step "   step
                  " -mean "   mean  ;; actually the mean shift
                  " -sdev "   sdev
                  (if *stoc-nominal* " -nominal " "")
                  " -p pre -p sim -p conv -p clean -p post"))


;; name of the output file of the computation
(def-data-filename "example.out")

;; the schema of the output of the program (of the data file)
(def-schema-path (string-append *srcdir* "/schema.dat"))

;; Scheme code needed to understand the schema and/or the evaluation
(def-load-scm (string-append *srcdir* "/defs.scm"))

;; the function to minimize, based on the schema and input
(def-eval '(* (+ (nom t1) (mu t1))
              (+ (nom t0) (mu t0) (* sigmaK (sigma t0)))))

;;(def-eval '(* *secret-value* result))

(set-netbatch #f)

