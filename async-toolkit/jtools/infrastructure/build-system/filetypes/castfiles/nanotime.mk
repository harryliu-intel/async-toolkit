$(SPICE_DIR)/%/cell.lib $(SPICE_DIR)/%/cell.paths: \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(GDS_DIR)/cell.cdl_gds2 \
	$(SPICE_DIR)/cell.spef \
	$(SPICE_DIR)/cell.dpf
	#TASK=nanotime CELL=$(call GET_CAST_CDL_NAME,$(@D))
	task=nanotime && $(CASTFILES_ENQUEUE_TASK) && \
	working_dir=`mktemp -d "$(WORKING_DIR)/nanotime.XXXXXX"` && \
	cd "$$working_dir" && \
	run_nanotime --spice='$(word 2,$+)' \
				 --spef='$(word 3,$+)' \
				 --dpf='$(word 4,$+)' \
				 --cell='$(call GET_GDS2_CDL_NAME,$(@D))' \
				 --corner='$(call GET_NTH_FROM_LAST_DIR,$(@D),3)' \
				 --true='$(call GET_NTH_FROM_LAST_DIR,$(@D),2)' \
				 --temp='$(call GET_NTH_FROM_LAST_DIR,$(@D),1)' \
				 --node-props='$<' \
				 --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT)' \
				 --working-dir="$$working_dir" >nanotime.log 2>&1 && \
	mkdir -p '$(@D)' && \
	cp "$$working_dir/cell.lib" '$(@D)' && \
	cp "$$working_dir/cell.paths" '$(@D)' && \
	cp "$$working_dir/nanotime.log" '$(@D)' && \
	cd / && ([[ '$(KEEP_NANOTIME_DIR)' == 1 ]] || rm -rf "$$working_dir"); \
	task=nanotime && $(CASTFILES_DEQUEUE_TASK)
