# TESTS := scribble speedtest structassign
# Need HDK for ToolConfig call to work
ifndef MODEL_ROOT
 $(error "No MODEL_ROOT defined, did you run setnhdk?")
endif

CM3=$(shell ToolConfig.pl get_tool_exec cm3)

SCRIBBLE = scribble/AMD64_LINUX/scribble
SPEEDTEST = speedtest/AMD64_LINUX/speedtest
STRUCTASSIGN = structassign/AMD64_LINUX/structassign

# structassign not done yet
all: $(SCRIBBLE) $(SPEEDTEST) speedtest

speedtest: $(SPEEDTEST)
	$(SPEEDTEST)

$(SCRIBBLE):
	cd scribble && $(CM3) -x
$(SPEEDTEST):
	cd speedtest && $(CM3) -x
$(STRUCTASSIGN):
	cd structassign && $(CM3) -x

clean:
	find . -name AMD64_LINUX | xargs rm -rf 
