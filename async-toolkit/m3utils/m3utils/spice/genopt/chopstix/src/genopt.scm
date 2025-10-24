
(dis "**********  Setting up genopt environment  **********" dnl)

(GenOpt.OptInit)
(QuadOpt.OptInit)
(QuadRobust.OptInit)

(GenOpt.SetCallback (make-cb-obj))

(dis "**********  Done setting up genopt environment  **********" dnl)
