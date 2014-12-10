; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun SerializeStackMachine (l @optional (p poport))
  (cond
    ((booleanp l) (if l (fprintf p "true\n") (fprintf p "null\n")))
    ((listp l) (fprintf p "beginlist\n")
               (mapc (lambda (x) (SerializeStackMachine x p)) l)
               (fprintf p "endlist\n"))
    (t (fprintf p "%L\n" l))))

(defun MicronToMeter (x)
  (quotient x 1000000.0))

(defun TransformInstanceName (name)
  ; Change foo(1,2,3) -> foo[1,2,3]
  (rexCompile "(")
  (setq name (rexReplace name "[" 0))
  (rexCompile ")")
  (setq name (rexReplace name "]" 0))

  ; Translating from ][ to , is now done by CadenceReverseNameInterface
  name)

(defun TransformCellName (name)
  name)

(defun ScaleBBox (bbox @optional (fun 'MicronToMeter))
  (let
    ((ll (car bbox))
     (ur (cadr bbox)))
    (list (mapcar fun ll) (mapcar fun ur))))

(defun CenterBBox (bbox)
  (let
    ((ll (car bbox))
     (ur (cadr bbox)))
    (list (quotient (plus (car ll) (car ur)) 2.0)
          (quotient (plus (cadr ll) (cadr ur)) 2.0))))

(defun TranslatePoint (pt dists)
  (list (plus (car pt) (car dists))
        (plus (cadr pt) (cadr dists))))

(defun TranslateBBox (bbox dists)
  (list (TranslatePoint (car bbox) dists)
        (TranslatePoint (cadr bbox) dists)))

; Print pins
(defun PrintPins (CV)
  (let (GND Vdd result)
    GND = (dbFindNetByName CV "GND")
    Vdd = (dbFindNetByName CV "Vdd")
    dists  = (mapcar 'minus (CenterBBox CV->bBox))
    (foreach net (setof net CV->nets (and net!=GND net!=Vdd))
      (foreach pin net->pins
        (if pin->fig != nil
            result=(cons (list net->name (ScaleBBox (TranslateBBox pin->fig->bBox dists))) result)
        )
      )
    )
    result))

(defun GenerateInstancesSingle
  (cv center LibCellPairRegExsToInclude LibCellPairRegExsToExclude 
      @optional (bboxTransform 'ScaleBBox)
      (cellTransform 'TransformCellName)
      (instTransform 'TransformInstanceName))
  (let
    ((dists (mapcar 'minus (CenterBBox cv->bBox))))
    (mapcar
      (lambda (inst)
        (list
          (funcall cellTransform inst->cellName)
          (funcall instTransform inst->name)
          (stringToSymbol inst->orient)
          (funcall bboxTransform
            (if center
              (TranslateBBox inst->bBox dists)
              inst->bBox))))
      (let
        ; Filter out pcell instance masters, and other undesirable cells
        ((candidates
           (setof x cv->instances
             ; Only output real, non-pcell, instances
             (and (equal x->objType "inst")
                  (not x->master->superMaster)))))
        (if LibCellPairRegExsToInclude
          (setq candidates
                (cadr (NameFilterInstances candidates
                                           LibCellPairRegExsToInclude))))
        (if LibCellPairRegExsToExclude
          (setq candidates
                (car (NameFilterInstances candidates
                                          LibCellPairRegExsToExclude))))
        candidates))))
  
(defun GenerateInstancesHierarchy (cv center
                                   LibCellPairRegExsToInclude
                                   LibCellPairRegExsToExclude
                                   @optional (cache (makeTable "cache" nil))
                                             (result nil))
  ; Give a concise but informative warning if an instance master cannot be
  ; found.  This may happen because a cellview was not checked in, or cds.lib
  ; is not setup correct, for example.
  (let
    ((badcells (makeTable "badcells" nil)) key)
    (mapcar
      (lambda (inst)
        (if (not inst->master)
          (let
            ((lcv (list inst->libName inst->cellName inst->viewName)))
            badcells[lcv] = (cons inst->name badcells[lcv]))))
      cv->instances)
    (if (greaterp (length badcells) 0)
      (begin
        (printf "In %L, the following cannot be loaded:\n" (list cv->libName cv->cellName cv->viewName))
        ; (lib cell view): (instance instance ...)
        (foreach key badcells
          (printf "%L: %L\n" key badcells[key])))))

  ; Recurse to subcells
  (mapc
    (lambda (master)
      (unless cache[master]
        (setq result (append result (GenerateInstancesHierarchy master center LibCellPairRegExsToInclude LibCellPairRegExsToExclude cache)))
        cache[master]=t))
    (let
      ; Filter out pcell instance masters, and null instanceMasters
      ((candidates (setof x cv->instanceMasters (and x (not x->superMaster)))))
      (if LibCellPairRegExsToInclude
        (setq candidates
              (cadr (NameFilterInstanceMasters candidates
                                               LibCellPairRegExsToInclude))))
      (if LibCellPairRegExsToExclude
        (setq candidates
              (car (NameFilterInstanceMasters candidates
                                              LibCellPairRegExsToExclude))))
      candidates))

  ; Generate instances for this cell
  (setq result (cons (list cv (GenerateInstancesSingle cv center LibCellPairRegExsToInclude LibCellPairRegExsToExclude )) result))

  result)

(defun WriteInstancesFile (dir cv @key ( center t ) (hierarchical t) (LibCellPairRegExsToInclude nil) (LibCellPairRegExsToExclude nil) )
  (if (isWritable dir)
    (mapcar
      (lambda (z)
        (letseq
          ((cv (car z))
           (x (cadr z))
           (fname (strcat dir "/" cv->cellName ".instances"))
           (out (outfile fname)))
          (if out
            (begin
              (SerializeStackMachine (ScaleBBox cv->bBox) out)
              (setq y (PrintPins cv))
              (if y
                (begin 
                  (SerializeStackMachine 'pins out)
                  (SerializeStackMachine y out)))
              (if x 
                (begin 
                  (SerializeStackMachine 'subcells out)
                  (SerializeStackMachine x out)))
              (drain out)
              (close out))
            (error (strcat "Cannot open file: " fname)))))
      (if hierarchical
        (GenerateInstancesHierarchy cv center LibCellPairRegExsToInclude LibCellPairRegExsToExclude)
        ( list ( list cv (GenerateInstancesSingle cv center LibCellPairRegExsToInclude LibCellPairRegExsToExclude) ) ) ))
    (error (strcat "Cannot write to directory: " dir))))
