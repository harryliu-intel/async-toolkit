(require-modules "m3")

(define recs
  (let* ((rd (FileRd.Open "RING_OSCILLATOR.json"))
         (res (TriOsc.LoadJson rd)))
    (Rd.Close rd)
    res))

(define *cal-temps* '(0 125))
(define *mesh-size* 50)
(define *samples*   500)
(define *k* 0)

(define osc (TriOsc.Calibrate "tttt" recs *cal-temps*))

(define temp-meshes (TriOsc.MakeMeshes `((0 . ,osc) (1 . ,osc) (2 . ,osc))
                                      *cal-temps*
                                      *mesh-size*))
                                      

(define test-point '((0 . 53.0008e6) (1 . 87.8511e6) (2 . 216.8556e6)))

(TriOsc.Estimate test-point temp-meshes *cal-temps* *samples* *k*)
  


               
