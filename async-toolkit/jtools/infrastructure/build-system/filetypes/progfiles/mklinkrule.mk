
make_link_rule_path_to_target := $(CURR_TARGET_DIR)/$(strip $(MAKE_LINK_RULE_TARGET_NAME))


$(make_link_rule_path_to_target): $(MAKE_LINK_RULE_TARGET_DEPENDENCIES)
	 $(PROGFILES_LOCAL_LD_PREFIX-$(strip $(@D))) -o $@ $^ $(PROGFILES_LOCAL_LD_POSTFIX-$(strip $(@D)))

CURR_RESULT_FILES := $(make_link_rule_path_to_target) $(CURR_RESULT_FILES)
