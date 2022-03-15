(load "liberty-utils.scm")

(define *fn* "cdp_lamb_1w1afr_40w_137b__tt_0p75v_85c_typical.lib")
(define *fn* "cdp_lamb_1w1sr_32w_21b__tt_0p75v_85c_typical.lib")
(define *fn* "cdp_lamb_1w1sr_template.lib")


  
(define *lib*
  (let* ((rd (FileRd.Open *fn*))
         (res (LibertyParse.Parse rd)))
    (Rd.Close rd)
    res))

