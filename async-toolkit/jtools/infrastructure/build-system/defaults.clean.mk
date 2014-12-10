
CURR_SUB_CLEAN_MAKE_FILES := $(foreach subtargetdir, $(CURR_SUB_TARGETS_DIRS),\
                             $(subtargetdir)/cleanmakefiles )
CURR_SUB_CLEAN_INTERMEDIATES := $(foreach subtargetdir, $(CURR_SUB_TARGETS_DIRS),\
                                $(subtargetdir)/cleanintermediates )
CURR_SUB_CLEAN_RESULTS := $(foreach subtargetdir, $(CURR_SUB_TARGETS_DIRS),\
                          $(subtargetdir)/cleanresults )

CURR_INTERMEDIATES_TO_KILL := $(foreach thing, $(CURR_INTERMEDIATE_FILES), $(thing).thingtokill )
CURR_RESULTS_TO_KILL := $(foreach thing, $(CURR_RESULT_FILES), $(thing).thingtokill )
CURR_INTERMEDIATE_MAKES_TO_KILL := $(foreach thing, $(CURR_INTERMEDIATE_MAKE_FILES), $(thing).thingtokill )

CURR_INTERMEDIATE_FILES := 
CURR_RESULT_FILES :=
CURR_INTERMEDIATE_MAKE_FILES := 

.PHONY: $(CURR_TARGET_DIR)/cleanall                     \
        $(CURR_TARGET_DIR)/cleanmakefiles               \
        $(CURR_TARGET_DIR)/cleanintermediates           \
        $(CURR_TARGET_DIR)/cleanresults                 \
        $(CURR_INTERMEDIATES_TO_KILL)                  \
        $(CURR_RESULTS_TO_KILL)                        \
        $(CURR_INTERMEDIATE_MAKES_TO_KILL)

$(CURR_INTERMEDIATES_TO_KILL) :
	rm -f "$(patsubst %.thingtokill,%,$@)"

$(CURR_RESULTS_TO_KILL) :
	rm -f "$(patsubst %.thingtokill,%,$@)"

$(CURR_INTERMEDIATE_MAKES_TO_KILL) :
	rm -f "$(patsubst %.thingtokill,%,$@)"


$(CURR_TARGET_DIR)/cleanmakefiles: $(CURR_SUB_CLEAN_MAKE_FILES) $(CURR_INTERMEDIATE_MAKES_TO_KILL)

$(CURR_TARGET_DIR)/cleanintermediates: $(CURR_SUB_CLEAN_INTERMEDIATES) $(CURR_INTERMEDIATES_TO_KILL)

$(CURR_TARGET_DIR)/cleanresults: $(CURR_SUB_CLEAN_RESULTS) $(CURR_RESULTS_TO_KILL)

$(CURR_TARGET_DIR)/cleanall: $(CURR_TARGET_DIR)/cleanmakefiles $(CURR_TARGET_DIR)/cleanintermediates $(CURR_TARGET_DIR)/cleanresults


