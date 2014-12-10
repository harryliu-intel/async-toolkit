; Recursively find subcells of a cell that match a filter and apply an
; action.  Returns the list of cells that passed the filter.
(defun FindSubcells (@key (CV (geGetEditCellView))
                          (visited (makeTable "visited" nil))
                          (filter nil) ; (filter CV) function
                          (action nil) ; (action CV) function
                          )
  (let (result)
    (when !visited[CV]
      visited[CV]=t
      (foreach inst CV->instances
               result = (append result (FindSubcells ?filter filter ?action action
                                                     ?CV inst->master ?visited visited))
               )
      (when !filter || (apply filter (list CV))
            (printf "%s\n" CV->cellName)
            (when action (apply action (list CV)))
            result = (cons CV result)
            )
      )
    result
    )
  )

; Filter and action functions for use with FindSubcells.
leafCellFilter = (lambda (CV) (dbGetPropByName CV "LeafCell")->value=="TRUE")
editCellAction = (lambda (CV) (cdsp4edit ?CV CV))
