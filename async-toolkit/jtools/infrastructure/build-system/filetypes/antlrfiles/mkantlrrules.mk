MAKE_ANTLR_RULES_G_FILE_NAME :=$(strip $(MAKE_ANTLR_RULES_G_FILE_NAME))
MAKE_ANTLR_RULES_LEXER_NAME :=$(strip $(MAKE_ANTLR_RULES_LEXER_NAME))
MAKE_ANTLR_RULES_PARSER_NAME :=$(strip $(MAKE_ANTLR_RULES_PARSER_NAME))
MAKE_ANTLR_RULES_TOKEN_TYPES_NAME :=$(strip $(MAKE_ANTLR_RULES_TOKEN_TYPES_NAME))

$(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_G_FILE_NAME): $(CURR_PROJECT_DIR)/$(MAKE_ANTLR_RULES_G_FILE_NAME)
	cp $< $@
	chmod u+w $@


ifneq ("$(strip $(MAKE_ANTLR_RULES_LEXER_NAME))","")
make_antlr_rules_results := $(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_PARSER_NAME).java           \
                            $(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_LEXER_NAME).java            \
                            $(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_TOKEN_TYPES_NAME)TokenTypes.java \
                            $(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_TOKEN_TYPES_NAME)TokenTypes.txt

else
make_antlr_rules_results := $(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_PARSER_NAME).java           \
                            $(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_TOKEN_TYPES_NAME)TokenTypes.java \
                            $(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_TOKEN_TYPES_NAME)TokenTypes.txt
endif

$(make_antlr_rules_results) : $(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_G_FILE_NAME)
	cd $(@D) && JAVAFILES_CLASSES_JAR_ROOT='$(JAVAFILES_CLASSES_JAR_ROOT)' $(ANTLR) $(<F)
	touch $@


CURR_INTERMEDIATE_FILES := $(make_antlr_rules_results)                                       \
                           $(CURR_TARGET_DIR)/$(MAKE_ANTLR_RULES_G_FILE_NAME)                \
                           $(CURR_INTERMEDIATE_FILES)
