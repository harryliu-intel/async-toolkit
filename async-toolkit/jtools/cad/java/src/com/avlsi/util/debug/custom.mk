$(CURR_TARGET_DIR)/com_avlsi_util_debug_GetTime.h: $(CURR_TARGET_DIR)/GetTime
	$(JAVAFILES_LOCAL_JNI_JAVAH-$(@D)) -o $@ com.avlsi.util.debug.GetTime

CURR_RESULT_FILES := $(CURR_TARGET_DIR)/com_avlsi_util_debug_GetTime.h \
                     $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/GetTime.o : $(CURR_TARGET_DIR)/com_avlsi_util_debug_GetTime.h
