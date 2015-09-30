;; script file


(load "dir2.scm")

(load "problem-definition.scm")

(load "dir3.scm")

(reset)
(go)
(setup-minimization!)
(search-loop)



(load "do-final-output.scm")
