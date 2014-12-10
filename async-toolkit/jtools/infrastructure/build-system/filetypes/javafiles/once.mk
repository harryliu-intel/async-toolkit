javafiles_dollar =$

javafiles_paren :=(

javafiles_dollar_at :=$(javafiles_dollar)@

javafiles_dollar_wildcard :=$(javafiles_dollar)$(javafiles_paren)wildcard


JAVAFILES_CLASSPATH_FRAME_SIZE        :=0
JAVAFILES_JAVAC_FLAGS_FRAME_SIZE      :=0
JAVAFILES_JAVAC_FRAME_SIZE            :=0
JAVAFILES_JRE_ROOT_FRAME_SIZE         :=0
JAVAFILES_APPS_CLASSES_FRAME_SIZE     :=0

JAVAFILES_JAVA2PACKAGE_SCRIPT := $(BUILD_SYSTEM_ROOT)/filetypes/javafiles/java2package
JAVAFILES_JAVA2PACKAGE_CMD := $(JAVAFILES_JAVA2PACKAGE_SCRIPT) $(GCC) $(GNUGREP) $(GNUSED)

JAVAFILES_JAVADEP_CMD := $(BUILD_SYSTEM_ROOT)/filetypes/javafiles/javadep.sh

JAVAFILES_MK_JAR_TOP_LEVEL_JAVA := $(BUILD_SYSTEM_ROOT)/filetypes/javafiles/mkTopLevelJava.pl

ifeq ("$(FULCRUM_BUILD_ID)","")

FULCRUM_BUILD_ID := $(USER)-$(shell hostname)-$(shell date +%Y-%m-%d-%H-%M-%S-%Z)-unofficial


endif



.PHONEY: javadoc




JAVAFILES_ALL_JAVA_FILES := 

javafiles_space:= $(empty) $(empty)

