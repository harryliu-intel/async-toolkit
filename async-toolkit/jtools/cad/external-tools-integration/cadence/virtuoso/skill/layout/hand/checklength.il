(defun CheckLengthAccumulate ( Fig Table )
  (let (
        ( Type ( getq Fig objType ) )
        ( Key nil )
        ( Incr 0 ) )
    (cond (
           ( equal Type "path" )
           (let ()
             ( setq Key ( getq Fig lpp ) )
             ( setq Incr ( PathGetDistance Fig ) )
           ) )
          (
           ( equal Type "rect" )
           (let (
                 ( BBox ( getq Fig bBox ) ) )
             ( setq Key ( getq Fig lpp ) )
             ( setq Incr ( max ( BBoxGetHeight BBox )
                               ( BBoxGetWidth BBox ) ) ) ) )
          (
           ( equal Type "polygon" )
           (let ()
             ( setq Key ( getq Fig lpp ) )
             ( setq Incr ( quotient ( PolygonPerimeter ( getq Fig points ) )
                                    2 ) ) ) )
          (
           ( equal Type "inst" )
           (let ()
             ( setq Key ( getq Fig master ) )
             ( setq Incr 1 ) ) ) )
    (if Key
        ( setarray Table Key ( plus ( arrayref Table Key ) Incr ) ) ) ) )

(defun CheckLengthRoutedLocal ( Net Table )
  (let (
        ( Fig nil )
        ( Term ( getq Net term ) ) )
    ( foreach
      Fig
      ( getq Net figs )
      ( CheckLengthAccumulate Fig Table ) )
    (if Term
        ( foreach
          Pin
          ( getq Term pins )
          (let (
                ( Fig ( getq Pin fig ) ) )
            (if Fig
                ( CheckLengthAccumulate Fig Table ) ) ) ) )
    Table ) )

(defun CheckLengthPutCache ( Net Value Cache )
  (letseq (
           ( CellView ( getq Net cellView ) )
           ( Key ( list ( getq CellView libName )
                        ( getq CellView cellName )
                        ( getq CellView viewName ) ) )
           ( CellCache ( arrayref Cache Key ) ) )
    (unless CellCache
      ( setq CellCache ( makeTable "cell cache" nil ) )
      ( setarray Cache Key CellCache ) )
    ( setarray CellCache ( getq Net name ) Value ) ) )

(defun CheckLengthLookupCache ( Net Cache )
  (letseq (
           ( CellView ( getq Net cellView ) )
           ( Key ( list ( getq CellView libName )
                        ( getq CellView cellName )
                        ( getq CellView viewName ) ) )
           ( CellCache (if Cache ( arrayref Cache Key ) ) ) )
    (if CellCache
        ( arrayref CellCache ( getq Net name ) ) ) ) )

(defun CheckLengthCombine ( LocalTable RemoteTable )
  (let (
        ( Result ( makeTable "remote" 0 ) ) )
    ( foreach Key LocalTable
      (let (
            ( LocalValue ( arrayref LocalTable Key ) ) 
            ( RemoteValue ( arrayref RemoteTable Key ) ) )
        ( setarray Result Key ( plus LocalValue RemoteValue ) ) ) )
    ( foreach Key RemoteTable
      (let (
            ( LocalValue ( arrayref LocalTable Key ) ) 
            ( RemoteValue ( arrayref RemoteTable Key ) ) )
        (if ( equal LocalValue 0 )
            ( setarray Result Key RemoteValue ) ) ) )
    Result ) )

(defun CheckLengthCombineInplace ( CachedTable RemoteTable )
  ( foreach Key CachedTable
    (let (
          ( CachedValue ( arrayref CachedTable Key ) ) 
          ( RemoteValue ( arrayref RemoteTable Key ) ) )
      ( setarray RemoteTable Key ( plus CachedValue RemoteValue ) ) ) ) )

(defun CheckLengthRoutedRecursive ( Net LocalTable RemoteTable Cache )
  (letseq (
           ( LibCellExpressionPairsToIgnore LeafCellInstanceLibCellPairRegExs )
           ( InstTerm nil )
           ( Recursive ( eq LocalTable RemoteTable ) )
           ( Cached (if Recursive ( CheckLengthLookupCache Net Cache ) ) ) )
    (if Cached
        ( CheckLengthCombineInplace Cached RemoteTable )
      (letseq (
               ( RealRemote ( makeTable "remote" 0 ) ) )
        ( CheckLengthRoutedLocal Net (if Recursive RealRemote LocalTable ) )
        ( foreach
          InstTerm
          ( CheckLengthFilterInstTerms 
            ( getq Net instTerms )
            LibCellExpressionPairsToIgnore )
          (letseq (
                   ( Term ( getq InstTerm term ) )
                   ( Abstract ( CheckLengthGetAbstract Term ) )
                   ( InstNet ( getq Term net ) ) )
            (if Abstract
                (let (
                      ( Key "abstract" ) )
                  ( setarray
                    RealRemote
                    Key
                    ( plus ( arrayref RealRemote Key ) Abstract ) ) )
              ( CheckLengthRoutedRecursive
                InstNet
                RealRemote
                RealRemote
                Cache ) ) ) )
        (if ( and Cache ( getq Net term ) )
            ( CheckLengthPutCache
              Net
              (if Recursive
                  RealRemote
                ( CheckLengthCombine LocalTable RealRemote ) )
              Cache ) )
        ( CheckLengthCombineInplace RealRemote RemoteTable ) ) )
  ) )

(defun CheckLengthSummarize ( Table ViaCost )
  (let (
        ( Key nil )
        ( Sum 0.0 ) )
    ( foreach Key Table
      (let (
            ( Value ( arrayref Table Key ) ) 
            ( Incr 0 ) )
        (cond (
               ( equal Key "abstract" )
               ( setq Incr Value ) )
              (
               ( listp Key )
               ( setq Incr Value ) )
              (
               t
               ( setq Incr ( times ViaCost Value ) ) ) )
        ( setq Sum ( plus Sum Incr ) ) ) )
    Sum ) )

(defun CheckLengthRouted ( Net @key ( ViaCost 0.1 ) ( Cache nil ) )
  (let (
        ( LocalTable ( makeTable "local" 0 ) )
        ( RemoteTable ( makeTable "remote" 0 ) )
        ( LocalSum 0.0 )
        ( RemoteSum 0.0 ) )
    ( CheckLengthRoutedRecursive Net LocalTable RemoteTable Cache )
    ( setq LocalSum ( CheckLengthSummarize LocalTable ViaCost ) )
    ( setq RemoteSum ( CheckLengthSummarize RemoteTable ViaCost ) )
    ( list LocalSum RemoteSum ) ) )

(defun IdentityTransform () '( ( 0 0 ) "R0" 1.0 ) )

; Returns a list of ( Transform Terminal ) pairs for every terminal on a net.
(defun CheckLengthDistanceTerms ( Net Transform )
  (let (
        ( Terms nil )
       )
    ( setq
      Terms
      ( mapcar
        '(lambda
          ( InstTerm )
          ( list
            ( dbConcatTransform
              Transform
              ( getq ( getq InstTerm inst ) transform ) )
            ( getq InstTerm term ) ) )
        ( getq Net instTerms ) ) )
    (if ( getq Net term )
        ( cons ( list Transform ( getq Net term ) ) Terms )
      Terms ) ) )

(defun CheckLengthPinDistance ( Transform1 Pin1 Transform2 Pin2 )
  (let (
        ( Fig1 ( getq Pin1 fig ) )
        ( Fig2 ( getq Pin2 fig ) ) )
    (if ( and Fig1 Fig2 )
        (let (
              ( Result nil )
              ( BBox1 ( dbTransformBBox ( getq Fig1 bBox ) Transform1 ) )
              ( BBox2 ( dbTransformBBox ( getq Fig2 bBox ) Transform2 ) ) )
          ( foreach
            Point1
            ( list ( lowerLeft BBox1 )
                   ( upperRight BBox1 ) )
            ( foreach
              Point2
              ( list ( lowerLeft BBox2 )
                     ( upperRight BBox2 ) )
              ( setq
                Result
                ( CheckLengthDistanceMin
                  Result
                  ( PointManhattanDistance Point1 Point2 ) ) ) ) )
          Result ) ) ) )

(defun CheckLengthDistanceMin ( Dist1 Dist2 )
  (cond ( ( null Dist1 ) Dist2 )
        ( ( null Dist2 ) Dist1 )
        ( ( lessp Dist1 Dist2 ) Dist1 )
        ( t Dist2 ) ) )

(defun CheckLengthManhattanLocal ( Net )
  (let (
        ( Result nil )
        ( Terms ( CheckLengthDistanceTerms Net ( IdentityTransform ) ) ) )
    (cond (
           ( null Terms ) )
          (
           ( null ( cdr Terms ) )
           (let (
                 ( BBox ( getq ( getq Net cellView ) bBox ) ) )
             ( plus ( BBoxGetHeight BBox ) ( BBoxGetWidth BBox ) ) ) )
          (
           ( null ( cddr Terms ) )
           (letseq (
                    ( Info1 ( car Terms ) )
                    ( Info2 ( cadr Terms ) )
                    ( Transform1 ( car Info1 ) )
                    ( Term1 ( cadr Info1 ) )
                    ( Transform2 ( car Info2 ) )
                    ( Term2 ( cadr Info2 ) )
                   )
             ( foreach
               Pin1
               ( getq Term1 pins )
               ( foreach
                 Pin2
                 ( getq Term2 pins )
                 (let (
                       ( Distance ( CheckLengthPinDistance
                                    Transform1
                                    Pin1
                                    Transform2
                                    Pin2 ) ) )
                   ( setq
                     Result
                     ( CheckLengthDistanceMin
                       Result
                       Distance ) ) ) ) ) ) )
          (
           t
           (let (
                 ( BBox nil ) )
             ( foreach
               Info
               Terms
               (let (
                     ( Transform ( car Info ) )
                     ( Term ( cadr Info ) ) )
                 ( foreach
                   Pin
                   ( getq Term pins )
                   (let (
                         ( Fig ( getq Pin fig ) ) )
                     (if Fig
                         ( setq
                           BBox
                           ( BBoxCombine
                             BBox
                             ( dbTransformBBox
                               ( getq Fig bBox )
                               Transform ) ) ) ) ) ) ) )
             (if BBox
                 ( setq
                   Result
                   ( plus ( BBoxGetHeight BBox )
                          ( BBoxGetWidth BBox ) ) ) ) ) ) )
    Result ) )

; Returns all instTerms whose associated instance passes the library cell
; filter
(defun CheckLengthFilterInstTerms ( InstTerms LibCellExpressionPairsToIgnore )
  ( car
    ( NameFilterObjects
      InstTerms
      LibCellExpressionPairsToIgnore
      (lambda ( InstTerm )
        ( getq ( getq InstTerm inst ) libName ) )
      (lambda ( InstTerm )
        ( getq ( getq InstTerm inst ) cellName ) ) ) ) )

; Return the bounding box of all sources and sinks on the given net
(defun CheckLengthJautoBBox ( Net Transform LibCellExpressionPairsToIgnore
                              Cache )
  (letseq (
           ( CellView ( getq Net cellView ) )
           ( Term ( getq Net term ) )
           ( Result (if Term ( CheckLengthLookupCache Net Cache ) ) ) )
    (unless Result
      (let (
            ( InstTerms
              ( CheckLengthFilterInstTerms
                ( getq Net instTerms )
                LibCellExpressionPairsToIgnore ) ) )
        (if InstTerms
            ( foreach InstTerm InstTerms
              (let (
                    ( InstNet ( getq ( getq InstTerm term ) net ) ) )
                ( setq
                  Result
                  ( BBoxCombine
                    Result
                    ( CheckLengthJautoBBox
                      InstNet
                      ( getq ( getq InstTerm inst ) transform )
                      LibCellExpressionPairsToIgnore
                      Cache ) ) ) ) )
          ( setq Result ( getq CellView bBox ) ) )
        (if Term ( CheckLengthPutCache Net Result Cache ) ) ) )
    ( dbTransformBBox Result Transform ) ) )
 
; Compute the wirelength of a given net as Jauto would
(defun CheckLengthJauto ( Net Cache )
  (letseq (
           ( LibCellExpressionPairsToIgnore
             ( append
               WiringCellLibCellPairRegExs
               LeafCellInstanceLibCellPairRegExs ) )
           ( BBox ( CheckLengthJautoBBox
                    Net
                    ( IdentityTransform )
                    LibCellExpressionPairsToIgnore
                    Cache ) ) )
    ( plus ( BBoxGetHeight BBox ) ( BBoxGetWidth BBox ) ) ) )

; Return the wirelength for the given terminal stored in the abstract view
(defun CheckLengthGetAbstract ( Term )
  (letseq (
           ( CellView ( getq Term cellView ) )
           ( AbstractView
             ( nrOpenCellViewReadable
               ( getq CellView libName )
               ( getq CellView cellName )
               "abstract" ) ) )
    (if AbstractView
        (let (
              ( AbstractTerm ( dbFindTermByName
                               AbstractView
                               ( getq Term name ) ) ) )
          (if AbstractTerm
              ( dbGetPropByName AbstractTerm "WireLength" ) ) ) ) ) )

(defun CheckLengthReportNet ( Net Port @key ( Cache nil )
                                            ( JautoCache nil ) )
  (let (
        ( Jauto ( CheckLengthJauto Net JautoCache ) )
        ( Routed ( CheckLengthRouted Net ?Cache Cache ) )
        ( Local ( CheckLengthManhattanLocal Net ) )
       )
    ( fprintf
      Port
      "%s %6.3f %6.3f %6.3f %L\n"
      ( getq Net name )
      Jauto
      ( car Routed )
      ( cadr Routed )
      Local ) ) )

(defun CheckLengthReportAll ( CellView File
                              @key ( Cache ( makeTable "cache" nil ) )
                                   ( JautoCache ( makeTable "jauto" nil ) ) )
  (let (
        ( Port ( outfile File ) )
        ( Success t ) )
    ( fprintf
      Port
      "# This reports, for each net in the current cell, wirelength as would have been calculated by Jauto, routed wirelength in the current cell, routed wirelength in subcells, and manhattan wirelength in the current cell.\n"
    )
    ( foreach
      Net
      ( getq CellView nets )
      (let (
            ( NetName ( getq Net name ) ) )
        (if ( and ( not ( equal NetName GNDNetName ) )
                  ( not ( equal NetName VddNetName ) ) )
            (let (
                  ( Err ( errset ( CheckLengthReportNet
                                   Net
                                   Port
                                   ?Cache Cache
                                   ?JautoCache JautoCache ) ) ) )
              (if ( null Err )
                  (let ()
                    ( printf "Error: cannot report wirelength for %s (%L)\n"
                             ( getq Net name )
                             errset.errset )
                    ( setq Success nil ) ) ) ) ) ) )
    ( close Port )
    Success ) )



;get a list of netname/length pairs from file
(defun GetNetLengthListFromFile (filename)
 (let (file line pair netinfo netlengths)
  file=(infile filename)
  (if !file then
    (printf "ERROR: can't read file %s" filename)
  else
    (while (gets line file)
      (if (not (equal (getchar line 1) '\#))
        (let ()
          netinfo=(parseString line " ")
          pair=(list (car netinfo) (atof (nth 4 netinfo)) )
          netlengths=(cons pair netlengths)))
    )
  )
  (close file)
  netlengths
 )
)


(defun CompareWireLengthsToManhattan (infilename outfilename)
 (let (cv netlengths paths wirelength ratio file)
  cv=(geGetWindowCellView)
  file=(outfile outfilename)
  netlengths=(GetNetLengthListFromFile infilename)
  (foreach pair netlengths
    paths=(GetPathsForNetName (car pair))
    (if paths then
      wirelength=(TotalPathLength paths 0)
      ratio=wirelength/(cadr pair)
    else
      wirelength=(cadr pair)
      ratio=0.0
    )
    (fprintf file "%s %f %f\n" (car pair) (cadr pair) ratio)
  )
  (close file)
 )
)


(defun CompareBusWireLengthsToManhattan (infilename outfilename)
 (let (cv netlengths paths wirelength ratio file)
  cv=(geGetWindowCellView)
  file=(outfile outfilename)
  netlengths=(GetNetLengthListFromFile infilename)
  (foreach pair netlengths
    paths=(GetBusScriptPathsForNetName (car pair))
    (if paths && (cadr pair) then
      wirelength=(TotalPathLength paths 0)
      ratio=wirelength/(plus (cadr pair) 0.01)
    else
      wirelength=(cadr pair)
      ratio=0.0
    )
    (if (cadr pair) then
      (fprintf file "%s %f %f\n" (car pair) (cadr pair) ratio)
    else
      (fprintf file "%s %s %f\n" (car pair) "NA" ratio)
    )
  )
  (close file)
 )
)

; Functions for displaying the wirelength report
(defun CheckLengthAdd (a b) (if (or (null a) (null b)) nil (plus a b)))

(defun CheckLengthReadFullReport (filename)
  (let (file line fields result)
    (setq file (infile filename))
    (if file
        (while (gets line file)
          (if (not (equal (getchar line 1) '\#))
            (let ()
              (setq fields (parseString line " "))
              (setq result
                (cons (list (nth 0 fields)
                            (CheckLengthAdd (atof (nth 2 fields))
                                            (atof (nth 3 fields)))
                            (atof (nth 2 fields))
                            (atof (nth 3 fields))
                            (atof (nth 1 fields))
                            (atof (nth 4 fields))) result)))))
      (error "Cannot read file: %s" filename))
    result))

(defvar CheckLengthReportState)

(defun CheckLengthSelectNet (sel func)
  (let (name instTerm net fig (cv CheckLengthReportState->cellView))
    (foreach name sel
      net = (dbFindNetByName cv name)
      (if net
          (let ()
            (foreach fig net->figs
              (funcall func fig))
            (foreach instTerm net->instTerms
              (if (equal instTerm->inst->libName TechLibName)
                (funcall func instTerm->inst))))
        (printf "Warning: cannot find net: %L\n" name)))))

(defun CheckLengthSelectCB (sel)
  (CheckLengthSelectNet sel 'geSelectObject))

(defun CheckLengthDeselectCB (sel)
  (CheckLengthSelectNet sel 'geDeselectObject))

(defun CheckLengthReportCB (field values)
  (let ((table (makeTable "SelectedTable" nil)) sel desel net
        (prevTable CheckLengthReportState->table)
       )
    (foreach value values
      (let ((choices checkLengthReportForm->checkLengthReportField->choices)) 
        net = (car (nth value choices))
        table[net] = t
        (if (not prevTable[net])
            sel = (cons net sel))))
    (foreach net prevTable
      (if (null table[net])
          desel = (cons net desel)))
    CheckLengthReportState->table = table
    (if (and desel CheckLengthReportState->deselectCB)
        (funcall CheckLengthReportState->deselectCB desel))
    (if (and sel CheckLengthReportState->selectCB)
        (funcall CheckLengthReportState->selectCB sel))))

(defun CheckLengthSearchCB (fieldName regex sourceOfChange)
  (let (net (first (getchar regex 1)) magic hint)
    (if (eq first '\/)  ; if starts with /, treat as regular expression
        (let ()
          (setq regex (substring regex 2))
          (setq hint "Input regular expression")
          (setq magic t))
      (setq hint (if (equal regex "") "Use / to start regular expression" "")))

    (if (or (null regex) (equal regex ""))
        checkLengthReportForm->checkLengthReportField->choices =
          CheckLengthReportState->allRows
      (let ()
        (rexMagic magic)
        (if (errset (rexCompile regex))
            (let (choices)
              (foreach row CheckLengthReportState->allRows
                (setq net (car row))
                (if (rexExecute net)
                    (setq choices (cons row choices))))
              checkLengthReportForm->checkLengthReportField->choices = choices)
          (if magic
              hint = (car (nth 4 errset.errset)))
        )
        (rexMagic t)))
    (if hint
        checkLengthReportForm->checkLengthRegexField->value = hint)
    t))

(defun CheckLengthReportWidth (label text)
  (max (ceiling (plus (hiGetTextWidth (hiGetFont "label") label) 10))
       (ceiling (hiGetTextWidth (hiGetFont "text") text))))

(defun CheckLengthReportForm (wirelengths)
  (let (searchField mainField form netWidth fieldWidth allWidth)
    CheckLengthReportState = '(nil)
    CheckLengthReportState->table = (makeTable "SelectedTable" nil)
    CheckLengthReportState->cellView = (geGetEditCellView)
    CheckLengthReportState->selectCB = 'CheckLengthSelectCB
    CheckLengthReportState->deselectCB = 'CheckLengthDeselectCB
    CheckLengthReportState->allRows = wirelengths
    (setq searchField
      (hiCreateStringField
        ?name 'checkLengthSearchField
        ?prompt "Filter net:  "
        ?modifyCallback "CheckLengthSearchCB"
        ?editable t))
    (setq regexField
      (hiCreateLabel
        ?name 'checkLengthRegexField
        ?labelText "Use / to start regular expression"
        ?enabled t))
    netWidth = 300
    fieldWidth = (max (CheckLengthReportWidth "Total" "88888.888")
                      (CheckLengthReportWidth "Local" "88888.888")
                      (CheckLengthReportWidth "Subcells" "88888.888")
                      (CheckLengthReportWidth "Jauto" "88888.888")
                      (CheckLengthReportWidth "Manhattan" "88888.888")) + 10
    allWidth = netWidth + fieldWidth * 5 + 24 ; vertical scrollbar 24px wide
    (setq mainField
      (hiCreateReportField
        ?name 'checkLengthReportField
        ?headers `(("Net" ,netWidth left string t)
                   ("Total" ,fieldWidth right float3 t)
                   ("Local" ,fieldWidth right float3 t)
                   ("Subcells" ,fieldWidth right float3 t)
                   ("Jauto" ,fieldWidth right float3 t)
                   ("Manhattan" ,fieldWidth right float3 t))
        ?choices wirelengths
        ?sort '(1 t)
        ?callback 'CheckLengthReportCB
        ?enableDeselectCB t
      ))
    (setq form
      (hiCreateAppForm
        ?name 'checkLengthReportForm
        ?formTitle "Wirelength Report"
        ?initialSize t
        ?buttonLayout 'OKCancel
        ?fields
          (list 
            (list searchField 0:0 200:hicLineHeight
                  (hiGetTextWidth (hiGetFont "label") searchField->prompt))
            (list regexField 250:0 100:hicLineHeight)
            (list mainField 0:hicLineHeight allWidth:1000 100))))
    (hiDisplayForm form)))
