#Recurse into subdirectories.



ifeq ($(CURR_SUB_TARGET_MK_INCLUDES),)
else
-include $(CURR_SUB_TARGET_MK_INCLUDES)
endif

