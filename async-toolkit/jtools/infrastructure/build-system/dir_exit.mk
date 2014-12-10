ifneq ("$(strip $(FILE_TYPES))","")
FILE_TYPE_DIR_OUT_MK_FILES := $(addsuffix /dirout.mk, $(addprefix $(BUILD)/filetypes/, $(FILE_TYPES) ) )
else
FILE_TYPE_DIR_OUT_MK_FILES := $(wildcard $(BUILD)/filetypes/*/dirout.mk )
endif
include $(FILE_TYPE_DIR_OUT_MK_FILES)
