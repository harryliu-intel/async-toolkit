; From Fulcrum skill code.
; Recursively find subcells of a cell that match a filter and apply an
; action.  Returns the list of cells that passed the filter.
(defun FindSubcells (@key (CV (geGetEditCellView))
                          (visited (makeTable "visited" nil))
                          (filter nil) ; (filter CV) function
                          (action nil) ; (action CV) function
                          (viewName nil) ; recurse to an alternate view
                          )
  (let (result master)
    (when !visited[CV]
      visited[CV]=t
      (foreach inst CV->instances
               master = inst->master
               (when viewName
                 master = (dbOpenCellViewByType master->libName master->cellName viewName)
                 )
               (when master result =
                     (append result (FindSubcells ?filter filter ?action action
                                                  ?CV master ?visited visited ?viewName viewName))
                     )
               )
      (when !filter || (apply filter (list CV))
            (printf "%s\n" CV->cellName)
            (when action
              (dbReopen CV "a")
              (apply action (list CV))
              (dbSave CV)
              )
            result = (cons CV result)
            )
      )
    result
    )
  )

; Filter and action functions for use with FindSubcells.
leafCellFilter = (lambda (CV) (dbGetPropByName CV "LeafCell")->value=="TRUE")
editCellAction = (lambda (CV) (cdsp4edit ?CV CV))
addCellAction = (lambda (CV) (cdsp4add ?CV CV))
