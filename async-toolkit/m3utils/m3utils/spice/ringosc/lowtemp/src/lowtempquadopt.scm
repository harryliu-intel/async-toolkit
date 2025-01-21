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
(define *srcdir* (string-append *m3utils* "/spice/ringosc/lowtemp/src/"))

;; the following are parameters that may be overridden
;; the default values are stated
(def-paramvar 'temp   -40)
(def-paramvar 'lib    "i0s")
(def-paramvar 'thresh "ulvt")
(def-paramvar 'sweeps 30)     ;; make it large enough
(def-paramvar 'stages 11)     ;; should sweep this
(def-paramvar 'step   1e-9)   ;; this seems superfluous
(def-paramvar 'Kcycle 5)      ;; K-factor for cycle time


;; the following are the optimization variables
;; the four fields are the initial value and what we may consider a
;; significant delta (an approximation, of course), and the min and max
;; "reasonable" values
(def-optvar 'vdd  0.30  0.005    0.10   0.60)
(def-optvar 'delp 0.000 0.003   -0.05   1.00)
(def-optvar 'deln 0.000 0.008   -0.05   1.00)

;; failure to eval result
(GenOpt.SetOptFailureResult  1e200)
(GenOpt.SetLambdaMult            0)
(QuadRobust.SetDoNominal        #t)
(QuadRobust.SetMinNewPts        50)
(QuadRobust.SetLookback          7)

(GenOpt.SetCommandTimeout     2000) ;; also look at -ctruntimelimit

;; model the three aspects of any variable:
;; nom mu sigma
(QuadRobust.DoModel 'energy_cost '(Quadratic Linear    Linear))
(QuadRobust.DoModel 'silicon_cost '(Quadratic Linear    Linear))



;; NewUOAs configuration variables
(GenOpt.SetRhoBeg 10  )   ;; starting step size in terms of significant delta
(GenOpt.SetMinRho  1  )   ;; do not quit until we reach at least this rho
(GenOpt.SetRhoEnd  0.1)   ;; ending step size for convergence

;; this is the command to do an evaluation
(define *cmd-path* (string-append *m3utils*
                                  "/spice/ringosc/lowtemp/"
                                  *bindir*
                                  "/lowtemp"))

;; this is the experimental command to do an evaluation
(define *cmd-path* (string-append *m3utils*
                                  "/spice/ringosc/lowtemp/"
                                  *bindir*
                                  "/lowtemp"))

;; all the following assumed to run in a single directory

(def-compute-command
  '(string-append *cmd-path*
                  (if *stoc-nominal* " -nominal " "")
                  " -nb"
                  " -externalsweep    10"
                  " -vdd "            vdd
                  " -temp "           temp
                  " -lib "            lib
                  " -thresh "         thresh
                  " -sweeps "         *stoc-samples*
                  " -cscale           1.0 "
                  " -delp "           delp
                  " -deln "           deln
                  " -modleaves        true"
                  " -stages "         stages
                  " -step "           step
                  " -ctruntimelimit " 1800 ;; seconds
                  " -p pre -p sim -p conv -p clean -p post"))


;; name of the output file of the computation
;;(def-data-filename "measure.dat.stat")
(GenOpt.DefDataFilename "measure.dat")

;; the schema of the output of the program (of the data file)
;;(def-schema-path (string-append *srcdir* "/schema.dat"))
(GenOpt.DefSchemaPath (string-append *srcdir* "/lowtempopt_single.schema"))

;; Scheme code needed to understand the schema and/or the evaluation
(GenOpt.DefLoadScm (string-append *srcdir* "/defs.scm"))

;; the function to minimize, based on the schema and input
(QuadOpt.DefEval '(+ (nom 'energy_cost)
                     (mu 'energy_cost)
                     (nom 'silicon_cost)
                     (mu 'silicon_cost)
                     (* Kcycle (sigma 'silicon_cost))))

