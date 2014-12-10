

javafiles_this_dir_classes_cache_root := $(strip $(JAVAFILES_CLASSES_CACHE_ROOT))

ifeq ($(JAVAFILES_VARS_CHANGED),1)

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_GCJ_DEP
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_CLASSPATH
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_JAVAC_FLAGS
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_JAVAC
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_JDK_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_JRE_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_CLASSES_TO_BUILD
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_APPS_CLASSES
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_PARSERS_USED
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_PARSERS_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_JAR
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_JAVAH
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_CLASSES_CACHE_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_CLASSES_SRC_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := JAVAFILES_EXTRA_JAR_CLASSES
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME :=JAVAFILES_JAVADOC
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME :=JAVAFILES_JAVADOC_FLAGS
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME :=JAVAFILES_JAVADOC_LINKS
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME :=JAVAFILES_JNI_INCLUDE_DIR
include $(BUILD_SYSTEM_ROOT)/include-functions/popscopedvar.mk

javafiles_jre_lib := $(JAVAFILES_JRE_ROOT)/lib
javafiles_jre_jars := $(javafiles_jre_lib)/rt.jar $(wildcard $(javafiles_jre_lib)/ext/*.jar) $(wildcard $(JAVAFILES_CLASSES_JAR_ROOT)/*.jar)
javafiles_jre_jars := $(filter-out /usr/local/java/jre/lib/ext/readline.jar, $(javafiles_jre_jars))
javafiles_boot_classpath := $(subst $(javafiles_space),:,$(javafiles_jre_jars))

endif

JAVAFILES_VARS_CHANGED :=$(strip $(firstword $(JAVAFILES_VARS_CHANGED_STACK)))
JAVAFILES_VARS_CHANGED_STACK := $(call POP_STACK,$(JAVAFILES_VARS_CHANGED_STACK))



javafiles_popped_classes_cache_root := $(strip $(JAVAFILES_CLASSES_CACHE_ROOT))




ifneq ("$(javafiles_this_dir_classes_cache_root)","$(javafiles_popped_classes_cache_root)")
.PHONEY: $(CURR_TARGET_DIR)/clean_java_classes_cache $(CURR_TARGET_DIR)/clean_java_jars

javafiles_local_clean_class_cache-$(strip $(CURR_TARGET_DIR)) := rm -rf $(javafiles_this_dir_classes_cache_root)

$(CURR_TARGET_DIR)/clean_java_classes_cache:
	$(javafiles_local_clean_class_cache-$(strip $(@D)))


$(CURR_TARGET_DIR)/cleanintermediates: $(CURR_TARGET_DIR)/clean_java_classes_cache

$(CURR_TARGET_DIR)/clean_java_jars:
	$(GNUFIND) $(@D) -type f -name "*.jar" -print0 | xargs -0 -r rm

$(CURR_TARGET_DIR)/clean_java_manifests:
	$(GNUFIND) $(@D) -type f -name "*.manifest" -print0 | xargs -0 -r rm

$(CURR_TARGET_DIR)/cleanintermediates: $(CURR_TARGET_DIR)/clean_java_manifests

$(CURR_TARGET_DIR)/cleanresults: $(CURR_TARGET_DIR)/clean_java_jars

endif
