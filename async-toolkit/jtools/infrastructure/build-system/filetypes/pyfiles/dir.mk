$(CURR_TARGET_DIR)/%.pyo: $(CURR_PROJECT_DIR)/%.py
	$(PYTHON2) -O -c "import py_compile; py_compile.compile(\"$<\", \"$@\")"

$(CURR_TARGET_DIR)/%.pyc: $(CURR_PROJECT_DIR)/%.py
	$(PYTHON2) -c "import py_compile; py_compile.compile(\"$<\", \"$@\")"

$(CURR_TARGET_DIR)/__init__.py:
	touch $@
