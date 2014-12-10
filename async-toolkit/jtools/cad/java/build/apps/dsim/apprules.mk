dsimjar : $(APPS_TARGET_DIRS)/dsim.jar

DSIM_BUILD_TEMP := $(APPS_TARGET_DIRS)/dsim.jar-tempdir

DSIM_JAVA_FILES := src/com/avlsi/tools/dsim/DSimMain.java \
                   src/com/avlsi/util/cmdline/DSimModule.java

$(DSIM_BUILD_TEMP):
	mkdir -p $@

$(DSIM_BUILD_TEMP)/appmanifest: $(DSIM_BUILD_TEMP)
	echo "Main-Class: com.avlsi.tools.dsim.DSimMain" > $@

$(APPS_TARGET_DIRS)/dsim.jar: $(ALL_JAVA_FILES) $(DSIM_BUILD_TEMP) $(DSIM_BUILD_TEMP)/appmanifest
	$(JAVAC) -g -deprecation -classpath $(APP_BUILD_BASE_CLASSPATH):src:$(JAVAFILES_CLASSES_JAR_ROOT)/antlr.jar: \
		 -d $(DSIM_BUILD_TEMP) $(DSIM_JAVA_FILES)
	jar cmf $(DSIM_BUILD_TEMP)/appmanifest $@ -C $(DSIM_BUILD_TEMP)  com
	rm -rf $(DSIM_BUILD_TEMP)
