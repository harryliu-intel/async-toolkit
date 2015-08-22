LIBERTY_PARSER_DIST := liberty_parse-2.5

LIBERTY_PARSER_SOURCE := \
    PI.c \
    attr_lookup.c \
    group_lookup.c \
    liberty_front_lex.c \
    liberty_parser.c \
    libhash.c \
    libstrtab.c \
    mymalloc.c \
    syntax_decls.c \
    syntax_checks.c \
    token.c

LIBERTY_PARSER_OBJECTS := $(patsubst %.c,$(CURR_TARGET_DIR)/$(LIBERTY_PARSER_DIST)/%.o,$(LIBERTY_PARSER_SOURCE))

$(LIBERTY_PARSER_OBJECTS): $(CURR_TARGET_DIR)/$(LIBERTY_PARSER_DIST)/Makefile
	cd '$(@D)' && make -o Makefile

PROGFILES_LINKER := $(GCC)

MAKE_LINK_RULE_TARGET_NAME := libLiberty.so
MAKE_LINK_RULE_TARGET_DEPENDENCIES := $(LIBERTY_PARSER_OBJECTS) $(CURR_TARGET_DIR)/liberty_wrap.o
include $(BUILD_SYSTEM_ROOT)/filetypes/progfiles/mksolinkrule.mk
