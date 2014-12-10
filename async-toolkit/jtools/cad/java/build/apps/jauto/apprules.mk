jauto : $(APPS_TARGET_DIRS)/jauto.jar

JAUTO_BUILD_TEMP := $(APPS_TARGET_DIRS)/jauto.jar-tempdir

JAUTO_JAVA_FILE := src/com/avlsi/tools/jauto/Jauto.java

$(JAUTO_BUILD_TEMP):
	mkdir -p $@

$(JAUTO_BUILD_TEMP)/appmanifest: $(JAUTO_BUILD_TEMP)
	echo "Main-Class: com.avlsi.tools.jauto.Jauto" > $@

$(APPS_TARGET_DIRS)/jauto.jar: $(ALL_JAVA_FILES) $(JAUTO_BUILD_TEMP) $(JAUTO_BUILD_TEMP)/appmanifest
	$(JAVAC) -g -deprecation -classpath $(APP_BUILD_BASE_CLASSPATH):src: $(JAVAFILES_CLASSES_JAR_ROOT)/antlr.jar: \
		 -d $(JAUTO_BUILD_TEMP) $(JAUTO_JAVA_FILE)
	jar cmf $(JAUTO_BUILD_TEMP)/appmanifest $@ -C $(JAUTO_BUILD_TEMP)  com
	rm -rf $(JAUTO_BUILD_TEMP)
