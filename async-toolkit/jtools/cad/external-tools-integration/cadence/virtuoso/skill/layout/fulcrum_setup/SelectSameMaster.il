; This Fuction is used to select all the instances in the layout that 
; have the same master as the selected cell(s)
procedure( SelectSameMaster( )

        SelectedCellList = geGetSelectedSet( )

	geSelectAllFig( )

	PassedCellList = geGetSelectedSet( )

        foreach( SelectedCell SelectedCellList 

               if( SelectedCell->objType == "inst"
                        
                        then
				foreach( PassedCell PassedCellList

					if( PassedCell->master != SelectedCell->master
						then
							geDeselectObject( PassedCell )
					) ;end if

				) ;end foreach

		        else

                                geDeselectObject( SelectedCell )
                ) ;if

        ) ;foreach

	PassedCellList

) ;procedure


