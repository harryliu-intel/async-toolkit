JAVAFILES_VARS_CHANGED_STACK :=$(JAVAFILES_VARS_CHANGED) $(JAVAFILES_VARS_CHANGED_STACK)
JAVAFILES_VARS_CHANGED :=0

JAVAFILES_JAVAFILES_CUSTOM_MK := \
   $(wildcard $(CURR_PROJECT_DIR)/javafiles-custom.mk)

ifneq ("$(strip $(JAVAFILES_JAVAFILES_CUSTOM_MK))","")

JAVAFILES_GCJ_DEP_TEMP           := $(JAVAFILES_GCJ_DEP)

JAVAFILES_CLASSPATH_TEMP         := $(JAVAFILES_CLASSPATH)
JAVAFILES_JAVAC_FLAGS_TEMP       := $(JAVAFILES_JAVAC_FLAGS)
JAVAFILES_JAVAC_TEMP             := $(JAVAFILES_JAVAC)
JAVAFILES_JDK_ROOT_TEMP          := $(JAVAFILES_JDK_ROOT)
JAVAFILES_JRE_ROOT_TEMP          := $(JAVAFILES_JRE_ROOT)


JAVAFILES_PARSERS_USED_TEMP      := $(JAVAFILES_PARSERS_USED)
JAVAFILES_PARSERS_ROOT_TEMP      := $(JAVAFILES_PARSERS_ROOT)
JAVAFILES_CLASSES_TO_BUILD_TEMP  := $(JAVAFILES_CLASSES_TO_BUILD)
JAVAFILES_APPS_CLASSES_TEMP      := $(JAVAFILES_APPS_CLASSES)
JAVAFILES_JAR_TEMP               := $(JAVAFILES_JAR)
JAVAFILES_JAVAH_TEMP             := $(JAVAFILES_JAVAH)

JAVAFILES_EXTRA_JAR_CLASSES_TEMP := $(JAVAFILES_EXTRA_JAR_CLASSES) 


JAVAFILES_CLASSES_CACHE_ROOT_TEMP    := $(JAVAFILES_CLASSES_CACHE_ROOT)
JAVAFILES_CLASSES_SRC_ROOT_TEMP      := $(JAVAFILES_CLASSES_SRC_ROOT)


JAVAFILES_JAVADOC_TEMP           := $(JAVAFILES_JAVADOC)
JAVAFILES_JAVADOC_FLAGS_TEMP     := $(JAVAFILES_JAVADOC_FLAGS)
JAVAFILES_JAVADOC_LINKS_TEMP     := $(JAVAFILES_JAVADOC_LINKS)

JAVAFILES_JNI_INCLUDE_DIR_TEMP   := $(JAVAFILES_JNI_INCLUDE_DIR)

-include $(CURR_PROJECT_DIR)/javafiles-custom.mk 


ifneq ("$(strip $(JAVAFILES_GCJ_DEP_TEMP))","$(strip $(JAVAFILES_GCJ_DEP))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_CLASSPATH_TEMP))","$(strip $(JAVAFILES_CLASSPATH_DEFINES))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JAVAC_FLAGS_TEMP))","$(strip $(JAVAFILES_JAVAC_FLAGS))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JAVAC_TEMP))","$(strip $(JAVAFILES_JAVAC))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JDK_ROOT_TEMP))","$(strip $(JAVAFILES_JDK_ROOT))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JRE_ROOT_TEMP))","$(strip $(JAVAFILES_JRE_ROOT))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_CLASSES_TO_BUILD_TEMP))","$(strip $(JAVAFILES_CLASSES_TO_BUILD))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_APPS_CLASSES_TEMP))","$(strip $(JAVAFILES_APPS_CLASSES))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_PARSERS_USED_TEMP))","$(strip $(JAVAFILES_PARSERS_USED))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_PARSERS_ROOT_TEMP))","$(strip $(JAVAFILES_PARSERS_ROOT))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JAR_TEMP))","$(strip $(JAVAFILES_JAR))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JAVAH_TEMP))","$(strip $(JAVAFILES_JAVAH))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_CLASSES_CACHE_ROOT_TEMP))","$(strip $(JAVAFILES_CLASSES_CACHE_ROOT))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_CLASSES_SRC_ROOT_TEMP))","$(strip $(JAVAFILES_CLASSES_SRC_ROOT))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_EXTRA_JAR_CLASSES_TEMP))","$(strip $(JAVAFILES_EXTRA_JAR_CLASSES))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JAVADOC_TEMP))","$(strip $(JAVAFILES_JAVADOC))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JAVADOC_FLAGS_TEMP))","$(strip $(JAVAFILES_JAVADOC_FLAGS))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JAVADOC_LINKS_TEMP))","$(strip $(JAVAFILES_JAVADOC_LINKS))")
JAVAFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(JAVAFILES_JNI_INCLUDE_DIR_TEMP))","$(strip $(JAVAFILES_JNI_INCLUDE_DIR))")
JAVAFILES_VARS_CHANGED :=1
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif

ifeq ($(JAVAFILES_VARS_CHANGED),1)

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_GCJ_DEP
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_CLASSPATH
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_JAVAC_FLAGS
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_JAVAC
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_JDK_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_JRE_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_CLASSES_TO_BUILD
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_APPS_CLASSES
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_PARSERS_USED
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_PARSERS_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_JAR
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME := JAVAFILES_JAVAH
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME :=JAVAFILES_CLASSES_CACHE_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME :=JAVAFILES_CLASSES_SRC_ROOT
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME :=JAVAFILES_EXTRA_JAR_CLASSES
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME :=JAVAFILES_JAVADOC
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME :=JAVAFILES_JAVADOC_FLAGS
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME :=JAVAFILES_JAVADOC_LINKS
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

PUSH_SCOPED_VAR_VAR_NAME :=JAVAFILES_JNI_INCLUDE_DIR
include $(BUILD_SYSTEM_ROOT)/include-functions/pushscopedvar.mk

javafiles_jre_lib := $(JAVAFILES_JRE_ROOT)/lib
javafiles_jre_jars := $(javafiles_jre_lib)/rt.jar $(wildcard $(javafiles_jre_lib)/ext/*.jar) $(wildcard $(JAVAFILES_CLASSES_JAR_ROOT)/*.jar)
javafiles_jre_jars := $(filter-out /usr/local/java/jre/lib/ext/readline.jar, $(javafiles_jre_jars))
javafiles_boot_classpath := $(subst $(javafiles_space),:,$(javafiles_jre_jars))

endif

endif


javafiles_java_files_in_dir := $(wildcard $(CURR_PROJECT_DIR)/*.java)

javafiles_parsers_java_files := $(foreach parser, $(JAVAFILES_PARSERS_USED),$(parser).java)

javafiles_parsers_g_files_in_dir := $(wildcard $(CURR_PROJECT_DIR)/*.g)

javafiles_java_and_g_files_in_dir := $(javafiles_java_files_in_dir) $(javafiles_parsers_g_files_in_dir)

javafiles_jar_specs_in_dir := $(wildcard $(CURR_PROJECT_DIR)/*.jar.spec)
javafiles_packages_in_dir := $(wildcard $(CURR_PROJECT_DIR)/*.package)

javafiles_java_g_files_and_jar_specs_in_dir := $(javafiles_java_and_g_files_in_dir) \
                                               $(javafiles_jar_specs_in_dir)        \
                                               $(javafiles_packages_in_dir)


ifneq ("$(strip $(javafiles_java_g_files_and_jar_specs_in_dir))","")


JAVAFILES_ALL_JAVA_FILES := $(JAVAFILES_ALL_JAVA_FILES) $(javafiles_java_files_in_dir)

javafiles_curr_classpath := $(strip $(JAVAFILES_CLASSES_SRC_ROOT)):$(strip $(JAVAFILES_PARSERS_ROOT)):$(strip $(JAVAFILES_CLASSPATH)):$(strip $(javafiles_boot_classpath)):$(strip $(JAVAFILES_CLASSES_CACHE_ROOT))
javafiles_curr_javac := $(JAVAFILES_JAVAC) $(JAVAFILES_JAVAC_FLAGS) -classpath $(javafiles_curr_classpath) \
                                           -d $(JAVAFILES_CLASSES_CACHE_ROOT)
javafiles_curr_javac := $(call CONONICALIZE_LIST,$(javafiles_curr_javac))
javafiles_local_javac-$(strip $(CURR_TARGET_DIR)) := $(javafiles_curr_javac)


javafiles_curr_jni_javah := $(JAVAFILES_JAVAH) -jni -classpath $(javafiles_curr_classpath)
JAVAFILES_LOCAL_JNI_JAVAH-$(strip $(CURR_TARGET_DIR)) := $(javafiles_curr_jni_javah)

javafiles_curr_jar := $(JAVAFILES_JAR)
javafiles_local_jar-$(strip $(CURR_TARGET_DIR)) := $(javafiles_curr_jar)

javafiles_curr_jar_classpath := $(strip $(JAVAFILES_CLASSES_SRC_ROOT)):$(strip $(JAVAFILES_PARSERS_ROOT)):$(strip $(JAVAFILES_CLASSPATH)):$(strip $(javafiles_boot_classpath))

javafiles_curr_jar_javac := $(JAVAFILES_JAVAC) $(JAVAFILES_JAVAC_FLAGS) -classpath $(javafiles_curr_jar_classpath)
javafiles_local_jar_javac-$(strip $(CURR_TARGET_DIR)) := $(javafiles_curr_jar_javac)

javafiles_curr_jar_javac_with_extra_classes := $(javafiles_curr_jar_javac) $(JAVAFILES_EXTRA_JAR_CLASSES)
javafiles_local_jar_javac_with_extra_classes-$(strip $(CURR_TARGET_DIR)) := $(javafiles_curr_jar_javac_with_extra_classes)

javafiles_curr_gcj_dep := $(JAVAFILES_GCJ_DEP) -classpath $(javafiles_curr_classpath)
JAVAFILES_LOCAL_GCJ_DEP-$(strip $(CURR_TARGET_DIR)) := $(javafiles_curr_gcj_dep)

#.jar files for which we will make .jar.d files
javafiles_spec_jar_jars_with_deps := $(foreach jarBaseName, $(JAVAFILES_JAR_DEPS), $(CURR_TARGET_DIR)/$(jarBaseName))
#.jar.d files we should make.
javafiles_spec_jar_jar_deps_to_make := $(foreach jarBaseName, $(JAVAFILES_JAR_DEPS), $(CURR_TARGET_DIR)/$(jarBaseName).d)


javafiles_spec_jar_names := $(patsubst $(CURR_PROJECT_DIR)/%.jar.spec, %, $(javafiles_jar_specs_in_dir))
javafiles_spec_jar_targets := $(foreach specJarName, $(javafiles_spec_jar_names), $(CURR_TARGET_DIR)/$(specJarName).jar)
javafiles_spec_jar_java_deps := $(foreach specJarName, $(javafiles_spec_jar_names), $(CURR_TARGET_DIR)/$(specJarName).java.d)

javafiles_spec_jar_deps := $(filter $(javafiles_spec_jar_jar_deps_to_make), \
                             $(foreach specJarName, \
                               $(javafiles_spec_jar_names), \
                               $(CURR_TARGET_DIR)/$(specJarName).jar.d) )
javafiles_spec_jar_java := $(foreach specJarName, $(javafiles_spec_jar_names), $(CURR_TARGET_DIR)/$(specJarName).java)

javafiles_spec_jar_jars_without_deps := $(filter-out $(javafiles_spec_jar_jars_with_deps), \
                                                     $(javafiles_spec_jar_targets))

javafiles_package_jar_names := $(patsubst $(CURR_PROJECT_DIR)/%.package, %, $(javafiles_packages_in_dir))
javafiles_package_jar_targets := $(foreach packageJarName, \
                                   $(javafiles_package_jar_names), \
                                   $(CURR_TARGET_DIR)/$(packageJarName).jar)

javafiles_package_jar_deps := $(filter $(javafiles_spec_jar_jar_deps_to_make), \
                               $(foreach packageJarName, \
                                 $(javafiles_package_jar_names), \
                                 $(CURR_TARGET_DIR)/$(packageJarName).jar.d) )
javafiles_package_jar_jars_without_deps := $(filter-out $(javafiles_spec_jar_jars_with_deps), \
                                                     $(javafiles_package_jar_targets))

CURR_TARGET_DEPS := $(CURR_TARGET_DEPS) \
                    $(javafiles_spec_jar_deps) \
                    $(javafiles_spec_jar_java_deps) \
                    $(javafiles_package_jar_deps)
CURR_INTERMEDIATE_FILES := $(CURR_INTERMEDIATE_FILES) $(javafiles_spec_jar_java)
CURR_RESULT_FILES := $(CURR_RESULT_FILES) $(javafiles_spec_jar_targets) $(javafiles_package_jar_targets)

#Get a list of all the top level java classes in the current project directory
#by enumerating all the java files and stripping of the current project directory.
#If the current project directory was /scratch/chrisb/p4_wd/sw/cad/java/src/com/avlsi/tools/jauto
#and that directory contained a file called Jauto.java then the result of this expression
#would contain Jauto.
javafiles_non_inner_classes_in_dir := $(patsubst $(CURR_PROJECT_DIR)/%.java,%,$(javafiles_java_files_in_dir))

javafiles_non_inner_classes_with_project_path := $(foreach class,                           \
                                                    $(javafiles_non_inner_classes_in_dir), \
                                                    $(CURR_PROJECT_DIR)/$(class))

javafiles_non_inner_classes_with_target_path := $(foreach class,                           \
                                                    $(javafiles_non_inner_classes_in_dir), \
                                                    $(CURR_TARGET_DIR)/$(class))

javafiles_class_build_targets := $(foreach class,                                     \
                                     $(javafiles_non_inner_classes_with_target_path), \
                                     $(class).java_files_classbuild )


javafiles_curr_mkdir := $(BUILD_SYSTEM_ROOT)/safemkdir.zsh $(JAVAFILES_CLASSES_CACHE_ROOT)
javafiles_local_mkdir-$(strip $(CURR_TARGET_DIR)) := $(javafiles_curr_mkdir)

javafiles_all_class_jars := $(foreach class, \
                            $(javafiles_non_inner_classes_with_target_path), \
                            $(class).class.jar)

CURR_RESULT_FILES := $(CURR_RESULT_FILES) $(javafiles_all_class_jars)

ifneq ("$(strip $(javafiles_java_files_in_dir))","")
$(CURR_TARGET_DIR)/%.java_files_classbuild: $(CURR_PROJECT_DIR)/%.java
	$(javafiles_local_mkdir-$(strip $(@D))) 
	$(javafiles_local_javac-$(strip $(@D))) $<



$(CURR_TARGET_DIR)/javaclasses: $(javafiles_java_files_in_dir)
	$(javafiles_local_mkdir-$(strip $(@D)))
	$(javafiles_local_javac-$(strip $(@D))) $(filter-out $(javafiles_parsers_java_files),$^)




$(CURR_TARGET_DIR)/javaclasses $(javafiles_class_build_targets): $(javafiles_parsers_java_files)
$(CURR_PROJECT_DIR)/javaclasses : $(CURR_TARGET_DIR)/javaclasses

#You can't have pattern dependencies, but you can have pattern rules whose action doesn't do anything.
#These pattern rules were added so you can ask to have a class compiled using either the
#target or project path to that class.
$(CURR_TARGET_DIR)/% $(CURR_PROJECT_DIR)/%: $(CURR_TARGET_DIR)/%.java_files_classbuild
	@true
endif

.PHONEY : $(CURR_TARGET_DIR)/javaclasses                   \
          $(CURR_PROJECT_DIR)/javaclasses                  \
          $(javafiles_class_build_targets)

$(CURR_TARGET_DIR)/%.java.d: $(JAVAFILES_MK_JAR_TOP_LEVEL_JAVA) $(CURR_PROJECT_DIR)/%.jar.spec
	$< -d $(firstword $(call STRIP_FIRST_WORD, $^)) $(*F) $(patsubst %.java.d, %.java, $@ ) > $@

$(CURR_TARGET_DIR)/%.java: $(JAVAFILES_MK_JAR_TOP_LEVEL_JAVA) $(CURR_PROJECT_DIR)/%.jar.spec
	$< $(firstword $(call STRIP_FIRST_WORD, $^)) $(*F) $@

$(CURR_TARGET_DIR)/%.jar.d $(CURR_TARGET_DIR)/%.jar: $(CURR_TARGET_DIR)/%.java $(CURR_TARGET_DIR)/%.resources
	rm -rf $(@D)/$(*F)-jarbuild
	mkdir -p $(@D)/$(*F)-jarbuild
	sed -e "s/class[[:space:]]\+$(*F)/class blah/g" $< >$(@D)/$(*F)-jarbuild/blah.java
	echo "Implementation-Version: $(FULCRUM_BUILD_ID)" >$(@D)/$(*F)-jarbuild/manifest
	echo "" >>$(@D)/$(*F)-jarbuild/manifest
	$(JAVAFILES_JAVADEP_CMD) \
           "$(javafiles_local_jar_javac-$(strip $(@D))) -d $(@D)/$(*F)-jarbuild $(@D)/$(*F)-jarbuild/blah.java" \
	   $(@D)/$(*F)-jarbuild/blah.java \
           $(@D)/$(*F).jar \
           $(@D)/$(*F).jar.d
	$(javafiles_local_jar-$(strip $(@D))) cmf         \
               $(@D)/$(*F)-jarbuild/manifest              \
               $(@D)/$(*F).jar                            \
               `$(GNUFIND) $(@D)/$(*F)-jarbuild -mindepth 1 -maxdepth 1 -type d -printf "-C $(@D)/$(*F)-jarbuild %f "` \
               `$(GNUSED) 's/^/-C /' '$(word 2, $^)'`
	rm -rf $(@D)/$(*F)-jarbuild

$(CURR_TARGET_DIR)/%.class.jar: $(JAVAFILES_JAVA2PACKAGE_SCRIPT) $(CURR_PROJECT_DIR)/%.java
	rm -rf $(@D)/$(*F)-jarbuild
	mkdir -p $(@D)/$(*F)-jarbuild
	echo -n "Main-Class: " >$(@D)/$(*F)-jarbuild/manifest
	$(JAVAFILES_JAVA2PACKAGE_CMD) $(firstword $(call STRIP_FIRST_WORD, $^)) | $(GNUGAWK) -- '{ print $$1 ".$(*F)" }' >>$(@D)/$(*F)-jarbuild/manifest
	echo -n "Implementation-Version: " >>$(@D)/$(*F)-jarbuild/manifest
	echo "$(FULCRUM_BUILD_ID)" >>$(@D)/$(*F)-jarbuild/manifest
	echo "" >>$(@D)/$(*F)-jarbuild/manifest
	$(javafiles_local_jar_javac_with_extra_classes-$(strip $(@D))) -d $(@D)/$(*F)-jarbuild $(firstword $(call STRIP_FIRST_WORD, $^))
	$(javafiles_local_jar-$(strip $(@D))) cmf         \
               $(@D)/$(*F)-jarbuild/manifest              \
               $@                                         \
               `$(GNUFIND) $(@D)/$(*F)-jarbuild -mindepth 1 -maxdepth 1 -type d -printf "-C $(@D)/$(*F)-jarbuild %f "`
	rm -rf $(@D)/$(*F)-jarbuild

$(CURR_TARGET_DIR)/javafiles_phoney_target:
	@true


$(javafiles_all_class_jars) \
$(javafiles_spec_jar_jars_without_deps) \
$(javafiles_package_jar_jars_without_deps): $(CURR_TARGET_DIR)/javafiles_phoney_target


$(javafiles_all_class_jars) \
$(javafiles_spec_jar_deps)  \
$(javafiles_spec_jar_targets) \
$(javafiles_package_jar_deps) \
$(javafiles_package_jar_targets): $(javafiles_parsers_java_files)

$(javafiles_spec_jar_deps) $(javafiles_package_jar_deps): $(JAVAFILES_JAVADEP_CMD)

#You can't have pattern dependencies, but you can have pattern rules whose action doesn't do anything.
#This pattern rule was added so that you could ask for a class's jar file  using a path in the source directory hierarchy
$(CURR_PROJECT_DIR)/%.jar: $(CURR_TARGET_DIR)/%.jar
	@true

.PHONEY: $(CURR_TARGET_DIR)/javafiles_phoney_target


$(CURR_TARGET_DIR)/dir.jar: $(javafiles_parsers_java_files) \
                            $(javafiles_java_files_in_dir) \
                            $(CURR_TARGET_DIR)/javafiles_phoney_target
	rm -rf $(@D)/dirjarbuild
	mkdir -p $(@D)/dirjarbuild
	echo -n "Implementation-Version: " >$(@D)/dirjarbuild/manifest	echo "$(FULCRUM_BUILD_ID)" >>$(@D)/dirjarbuild/manifest
	echo "" >>$(@D)/dirjarbuild/manifest
	$(javafiles_local_jar_javac_with_extra_classes-$(strip $(@D))) \
            -d $(@D)/dirjarbuild \
            $(filter-out %/javafiles_phoney_target ,$^)
	$(javafiles_local_jar-$(strip $(@D))) cmf \
            $(@D)/dirjarbuild/manifest            \
            $@                                    \
            `$(GNUFIND) $(@D)/dirjarbuild -mindepth 1 -maxdepth 1 -type d -printf "-C $(@D)/dirjarbuild %f "`
	rm -rf $(@D)/dirjarbuild


endif



ifneq ("$(JAVAFILES_JAVADOC_SOURCE_PATH)","")

ifneq ("$(JAVAFILES_JAVADOC_DIR)","")

javafiles_curr_javadoc_links := $(foreach link, $(JAVAFILES_JAVADOC_LINKS), -link $(link) )

JAVAFILES_JAVADOC_SOURCE_PATH := $(strip $(JAVAFILES_JAVADOC_SOURCE_PATH))

JAVAFILES_PARSERS_ROOT := $(strip $(JAVAFILES_PARSERS_ROOT))

javafiles_curr_sourcepath := $(JAVAFILES_JAVADOC_SOURCE_PATH):$(JAVAFILES_PARSERS_ROOT)

javafiles_curr_source_dirs := $(JAVAFILES_PARSERS_ROOT) $(JAVAFILES_JAVADOC_SOURCE_PATH)

javafiles_curr_javadoc := \
     mkdir -p $(JAVAFILES_JAVADOC_DIR) && \
     $(JAVAFILES_JAVADOC) $(JAVAFILES_JAVADOC_FLAGS) -d $(JAVAFILES_JAVADOC_DIR) \
     -sourcepath $(javafiles_curr_sourcepath) \
     $(javafiles_curr_javadoc_links) \
     `$(GNUFIND) $(javafiles_curr_source_dirs) -type f -name "*.java" | xargs $(BUILD_SYSTEM_ROOT)/java2package.zsh |sort |uniq`

JAVAFILES_LOCAL_JAVADOC-$(strip $(CURR_TARGET_DIR)):=$(javafiles_curr_javadoc)

javadoc: $(CURR_TARGET_DIR)/javadoc

$(CURR_PROJECT_DIR)/javadoc : $(CURR_TARGET_DIR)/javadoc

$(CURR_TARGET_DIR)/javadoc : $(javafiles_parsers_java_files)
	$(JAVAFILES_LOCAL_JAVADOC-$(strip $(@D))) 

JAVAFILES_JAVADOC_SOURCE_PATH :=
JAVAFILES_JAVADOC_DIR :=

endif

endif
