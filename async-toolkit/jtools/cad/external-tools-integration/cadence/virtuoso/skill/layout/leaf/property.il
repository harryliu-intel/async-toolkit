
(defun SetLeafProp (cv val)
  (when cv
    (dbReplaceProp cv "LeafCell" "boolean" val)
  ) 
)

(defun SetAvagoProp (cv val)
  (when cv
    (dbReplaceProp cv "AvagoCell" "boolean" val)
    (dbReplaceProp cv "LeafCell" "boolean" nil)
  )
)

(defun SetProteusProp (cv val)
  (when cv
    (dbReplaceProp cv "ProteusCell" "boolean" val)
    (dbReplaceProp cv "AvagoCell" "boolean" nil)
    (dbReplaceProp cv "LeafCell" "boolean" nil)
  )
)


(defun IsLeafCell (cellname @key (viewname "floorplan")
                            (tempdir ConfigFileGetValue(TheCDSConfigTable "TEMP"))
                            (castpath ConfigFileGetValue(TheCDSConfigTable "CAST_PATH")) )
 (let (isleaf cv)
  isleaf=nil
  cv=(nrOpenCellViewReadable (car (NameParseCellName cellname)) cellname viewname) 
;(if cv then
  (if cv && (dbGetPropByName cv "LeafCell") then
    (when (dbGetPropByName cv "LeafCell")->value=="TRUE"
      isleaf=t
    )
  else
    isleaf=(nrIsLeafCell nil cellname nil ?tempdir tempdir ?castpath castpath)
  )
;else
;    isleaf=(nrIsLeafCell nil cellname nil ?tempdir tempdir ?castpath castpath)
;)
  isleaf
 )
)

(defun IsProteusCell (cellname @key (viewname "floorplan")
                            (tempdir ConfigFileGetValue(TheCDSConfigTable "TEMP"))
                            (castpath ConfigFileGetValue(TheCDSConfigTable "CAST_PATH")) )
 (let (isproteus cv)
  isproteus=nil
  cv=(nrOpenCellViewReadable (car (NameParseCellName cellname)) cellname viewname) 
;(if cv then

  (if cv && (dbGetPropByName cv "ProteusCell") then
    (when (dbGetPropByName cv "ProteusCell")->value=="TRUE"
      isproteus=t
    )
  else
    isproteus=nil
  )
;else
;    isleaf=(nrIsLeafCell nil cellname nil ?tempdir tempdir ?castpath castpath)
;)
  isproteus
 )
)

(defun IsAvagoCell (cellname @key (viewname "floorplan")
                            (tempdir ConfigFileGetValue(TheCDSConfigTable "TEMP"))
                            (castpath ConfigFileGetValue(TheCDSConfigTable "CAST_PATH")) )
 (let (isavago cv)
  isavago=nil
  cv=(nrOpenCellViewReadable (car (NameParseCellName cellname)) cellname viewname)

  (if cv && (dbGetPropByName cv "AvagoCell") then
    (when (dbGetPropByName cv "AvagoCell")->value=="TRUE"
      isavago=t
    )
  else
    isavago=nil
  )
  isavago
 )
)


